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
(with-eval-after-load 'dired
  (require 'dired-x)
  (setq dired-dwin-target 1)
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  (define-key dired-mode-map (kbd "=") 'sea/dired-diff)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "~") '(lambda ()(interactive)(find-alternate-file "~/"))))


;(evil-define-key 'normal  dired-mode-map (kbd "~") '(lambda ()(interactive)(find-alternate-file "~/")))
;(evil-define-key 'normal  dired-mode-map (kbd "RET") 'dired-find-alternate-file)

(provide 'init-dired)
