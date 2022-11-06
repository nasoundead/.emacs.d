;; Open/close directories with double-click, RET or Space keys.
;; To jump to the parent directory, hit the Backspace key.
;; To toggle open/closed state of the subtree of the current directory, hit the x key.
;; RET on different files starts the Ediff (or open file if one absent or the same)
;; Space show the simple diff window for the current file instead of Ediff (or view file if one absent or the same)
;; TAB to fast switch between panels
;; h key to toggle show/hide identical files/directories
;; H key to toggle show/hide hidden/ignored files/directories
;; C key to copy current file or directory to the left or right panel
;; D key to delete current file or directory
;; v key to quick view the current file
;; r initiates the rescan/refresh of current file or subdirectory
;; F5 forces the full rescan.
(use-package ztree
  :defer t
  :ensure t)

(defun sea/dired-diff ()
  "Ediff marked files in dired or selected files in separate window"
  (interactive)
  (let* ((marked-files (dired-get-marked-files nil nil))
         (other-win (get-window-with-predicate
                     (lambda (window)
                       (with-current-buffer (window-buffer window)
                         (and (not (eq window (selected-window)))
                              (eq major-mode 'dired-mode))))))
         (other-marked-files (and other-win
                                  (with-current-buffer (window-buffer other-win)
                                    (dired-get-marked-files nil)))))
    (cond ((= (length marked-files) 2)
           (if (and (file-directory-p (nth 0 marked-files))
                    (file-directory-p (nth 1 marked-files)))
               (ztree-diff (nth 0 marked-files)
                           (nth 1 marked-files))
             (ediff-files (nth 0 marked-files)
                          (nth 1 marked-files))))
          ((= (length marked-files) 3)
           (ediff-files3 (nth 0 marked-files)
                         (nth 1 marked-files)
                         (nth 2 marked-files)
                         ))
          ((and (= (length marked-files) 1)
                (= (length other-marked-files) 1))
           (if (and (file-directory-p (nth 0 marked-files))
                    (file-directory-p (nth 0 other-marked-files)))
               (ztree-diff (nth 0 marked-files)
                           (nth 0 other-marked-files)))
           (ediff-files (nth 0 marked-files)
                        (nth 0 other-marked-files)))
          ((= (length marked-files) 1)
           (dired-diff))
          (t (error "mark exactly 2 files, at least 1 locally")))))

;; we want dired not not make always a new buffer if visiting a directory
;; but using only one dired buffer for all directories.
(eval-after-load "dired"
  '(progn
     (defadvice dired-advertised-find-file (around dired-subst-directory activate)
       "Replace current buffer if file is a directory."
       (interactive)
       (let* ((orig (current-buffer))
              ;; (filename (dired-get-filename))
              (filename (dired-get-filename t t))
              (bye-p (file-directory-p filename)))
         ad-do-it
         (when (and bye-p (not (string-match "[/\\\\]\\.$" filename)))
           (kill-buffer orig))))))


(eval-after-load "dired"
  ;; don't remove `other-window', the caller expects it to be there
  '(defun dired-up-directory (&optional other-window)
     "Run Dired on parent directory of current directory."
     (interactive "P")
     (let* ((dir (dired-current-directory))
            (orig (current-buffer))
            (up (file-name-directory (directory-file-name dir))))
       (or (dired-goto-file (directory-file-name dir))
           ;; Only try dired-goto-subdir if buffer has more than one dir.
           (and (cdr dired-subdir-alist)
                (dired-goto-subdir up))
           (progn
             (kill-buffer orig)
             (dired up)
             (dired-goto-file dir))))))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
          ("C-c C-p" . wdired-change-to-wdired-mode)
          ("=" . sea/dired-diff))
  :config
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)

  (when  IS-MAC
    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil)

    (when (executable-find "gls")
      ;; Use GNU ls as `gls' from `coreutils' if available.
      (setq insert-directory-program "gls")))

  (when (or (and IS-MAC (executable-find "gls"))
            (and (or IS-LINUX IS-MAC) (executable-find "ls")))
    ;; Using `insert-directory-program'
    (setq ls-lisp-use-insert-directory-program t)
    ;; Show directory first
    (setq dired-listing-switches "-alh --group-directories-first"))

  ;; Quick sort dired buffers via hydra
  (use-package dired-quick-sort
    :bind (:map dired-mode-map
            ("S" . hydra-dired-quick-sort/body)))

  ;; Show git info in dired
  (use-package dired-git-info
    :bind (:map dired-mode-map
            (")" . dired-git-info-mode)))

  ;; Allow rsync from dired buffers
  (use-package dired-rsync
    :bind (:map dired-mode-map
            ("C-c C-r" . dired-rsync)))

  ;; Colorful dired
  (use-package diredfl
    :hook (dired-mode . diredfl-mode))

  ;; Shows icons
  (use-package all-the-icons-dired
    :diminish
    :hook (dired-mode . (lambda ()
                          (when (icon-displayable-p)
                            (all-the-icons-dired-mode))))
    :init (setq all-the-icons-dired-monochrome nil)
    :config
    (with-no-warnings
      (defun my-all-the-icons-dired--icon (file)
        "Return the icon for FILE."
        (if (file-directory-p file)
            (all-the-icons-icon-for-dir file
                                        :height 0.9
                                        :face 'all-the-icons-dired-dir-face
                                        :v-adjust all-the-icons-dired-v-adjust)
          (apply 'all-the-icons-icon-for-file file
                 (append
                  '(:height 0.9)
                  `(:v-adjust ,all-the-icons-dired-v-adjust)
                  (when all-the-icons-dired-monochrome
                    `(:face ,(face-at-point)))))))
      (advice-add #'all-the-icons-dired--icon :override #'my-all-the-icons-dired--icon)))

  ;; Extra Dired functionality
  (use-package dired-aux :ensure nil)
  (use-package dired-x
    :ensure nil
    :demand t
    :config
    (let ((cmd (cond (sys/mac-x-p "open")
                     (sys/linux-x-p "xdg-open")
                     (sys/win32p "start")
                     (t ""))))
      (setq dired-guess-shell-alist-user
            `(("\\.pdf\\'" ,cmd)
              ("\\.docx\\'" ,cmd)
              ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
              ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
              ("\\.\\(?:xcf\\)\\'" ,cmd)
              ("\\.csv\\'" ,cmd)
              ("\\.tex\\'" ,cmd)
              ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
              ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
              ("\\.html?\\'" ,cmd)
              ("\\.md\\'" ,cmd))))

    (setq dired-omit-files
          (concat dired-omit-files
                  "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*"))))

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired))



(provide 'init-dired)
