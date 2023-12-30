;; init-edit.el --- Initialize edit configurations.	-*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defvar sea-large-file-size 1
  "Size (in MB) above which the user will be prompted to open the file literally
to avoid performance issues. Opening literally means that no major or minor
modes are active and the buffer is read-only.")

(defvar sea-large-file-modes-list
  '(archive-mode tar-mode jka-compr git-commit-mode image-mode
    doc-view-mode doc-view-mode-maybe ebrowse-tree-mode pdf-view-mode)
  "Major modes that `sea|check-large-file' will ignore.")

(setq-default
 vc-follow-symlinks t
 ;; Save clipboard contents into kill-ring before replacing them
 save-interprogram-paste-before-kill t
 ;; Bookmarks
 bookmark-default-file (concat sea-cache-dir "bookmarks")
 bookmark-save-flag t
 ;; Formatting
 delete-trailing-lines nil
 fill-column 80
 sentence-end-double-space nil
 word-wrap t
 ;; Scrolling
 hscroll-margin 1
 hscroll-step 1
 scroll-conservatively 1001
 scroll-margin 0
 scroll-preserve-screen-position t
 ;; Whitespace (see `editorconfig')
 indent-tabs-mode nil
 require-final-newline t
 tab-always-indent t
 tab-width 4
 tabify-regexp "^\t* [ \t]+" ; for :retab
 ;; Wrapping
 truncate-lines t
 truncate-partial-width-windows 50
 ;; whitespace-mode
 whitespace-line-column fill-column
 whitespace-style
 '(face indentation tabs tab-mark spaces space-mark newline newline-mark
   trailing lines-tail)
 whitespace-display-mappings
 '((tab-mark ?\t [?› ?\t])
   (newline-mark ?\n [?¬ ?\n])
   (space-mark ?\  [?·] [?.])))

(delete-selection-mode 1)
(setq-default major-mode 'text-mode)
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
(add-hook 'abbrev-mode-hook (lambda () (diminish 'abbrev-mode)))

;; ediff
(setq ediff-diff-options "-w"
      ediff-split-window-function #'split-window-horizontally
      ediff-window-setup-function #'ediff-setup-windows-plain)

(defun sea|dont-kill-scratch-buffer ()
  "Don't kill the scratch buffer."
  (or (not (string= (buffer-name) "*scratch*"))
      (ignore (bury-buffer))))
(add-hook 'kill-buffer-query-functions #'sea|dont-kill-scratch-buffer)

;; temporary windows often have q bound to `quit-window', which only buries the
;; contained buffer. I rarely don't want that buffer killed, so...
(defun sea*quit-window (orig-fn &optional kill window)
  (funcall orig-fn (not kill) window))
(advice-add #'quit-window :around #'sea*quit-window)

(defun sea|check-large-file ()
  "Check if the buffer's file is large (see `sea-large-file-size'). If so, ask
for confirmation to open it literally (read-only, disabled undo and in
fundamental-mode) for performance sake."
  (let* ((filename (buffer-file-name))
         (size (nth 7 (file-attributes filename))))
    (when (and (not (memq major-mode sea-large-file-modes-list))
               size (> size (* 1024 1024 sea-large-file-size))
               (y-or-n-p
                (format (concat "%s is a large file, open literally to "
                                "avoid performance issues?")
                        (file-relative-name filename))))
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode))))
(add-hook 'find-file-hook #'sea|check-large-file)

(push '("/LICENSE$" . text-mode) auto-mode-alist)

;; revert buffers for changed files
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

;; enabled by default in Emacs 25+. No thanks.
;; (electric-indent-mode -1)
(electric-pair-mode)

;; savehist / saveplace
(setq savehist-file (concat sea-cache-dir "savehist")
      savehist-save-minibuffer-history t
      savehist-autosave-interval nil ; save on kill only
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
      save-place-file (concat sea-cache-dir "saveplace"))
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'save-place-mode)

 ;; Keep track of recently opened files
(use-package recentf
  :init
  (add-hook 'find-file-hook (lambda ()
                              (unless recentf-mode
                                (recentf-mode)
                                (recentf-track-opened-file))))
  :config
  (setq recentf-save-file (concat sea-cache-dir "recentf")
        recentf-max-menu-items 0
        recentf-max-saved-items 300
        recentf-filename-handlers '(file-truename)
        recentf-exclude
        (list "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
              "^/var/folders/.+$"
              ;; ignore private sea temp files (but not all of them)
              (concat "^" (file-truename sea-cache-dir)))))

(use-package savehist
  :ensure nil
  :init
  (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
        history-length 1000
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
        savehist-autosave-interval 60)
  (add-hook 'after-init-hook #'savehist-mode))

;; History
;; Emacsag 25 has a proper mode for `save-place'
(add-hook 'after-init-hook #'save-place-mode)

;; Show number of matches in mode-line while searching
;; (use-package visual-regexp-steroids
;;   :init
;;   (use-package visual-regexp)
;;   :bind (([remap query-replace-regexp] . vr/query-replace)))

(use-package anzu
  :straight (anzu
             :type git
             :host github
             :repo "syohex/emacs-anzu")
  :init (global-anzu-mode +1)
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :config (setq anzu-replace-to-string-separator
                (if (char-displayable-p ?→) " → " " -> "))
  )

(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "%s/%s ")
(setq lazy-highlight-cleanup nil)
;; 这样可以在literal的isearch中，把空格直接当成正则里面的.*匹
(setq isearch-lax-whitespace t)
(setq search-whitespace-regexp ".*")
;; 默认的isearch-forward函数是literal的，也就是用户输入什么就匹配什么，没有正则解释，没有转义，完全literal。
;; 这样的好处就是，想搜啥就是啥，不用考虑太多。其实默认用它就可以了。
;; 当然，完全可以开启正则匹配等功能，下面就说说这几个toggle函数。
;; isearch-toggle-regexp 在使用isearch搜索时（即按下C-s isearch-forward后）绑定到M-s r。 按下后，您的输入全部都会被以正则来匹配了。
;; isearch-toggle-case-fold 默认绑到M-s c。
;; 默认isearch对大小写是类似于rg一样“smart”的。具体地说，如果用户全部输入小写，则不匹分大小写进行匹配，如果用户输入中包括大写，则精确匹配大小写。
;; 再举个例子，默认情况下，默认foo可以匹配foo,Foo,FOO。输入Foo，只能匹配到Foo。打开这个选项后，就是case sensitive了，也就只能精确匹配了。
;; 个人认为，该选项用处不太大。
;; isearch-toggle-word 默认绑定到M-s w。打开word匹配。直接举例：未打开以前，foo可以匹配foobar，foo。打开该选项后，foo只能匹配foo了，foobar就匹配不到了。 可以看出来，开启该选项后，isearch必须完全匹配一个完整地word。这个功能可以帮忙过滤很多杂项。
;; isearch-toggle-symbol 默认绑定到M-s _ 。它和isearch-toggle-word的基本一样，不过它使isearch完全匹配一个symbol。 简单来说，symbol和word的区别：isearch-toggle-word是一个symbol，它包括isearch toggle和word三个word。
;; isearch-toggle-lax-whitespace 默认绑定到M-s SPC。开启该功能后，可以把输入中的空格当做一个固定的正则表达式，这个固定的正则表达式存在于search-whitespace-regexp变量中。关于这个功能，我在后面 空格的特殊用法 中进行详细说明。
(with-eval-after-load 'isearch
  ;; DEL during isearch should edit the search string, not jump back to the previous result
  (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)
  ;; Activate occur easily inside isearch
  (when (fboundp 'isearch-occur)
    ;; to match ivy conventions
    (define-key isearch-mode-map (kbd "C-c C-o") 'isearch-occur))
  (defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))
)


;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :init
  ;; show org ediffs unfolded
  (with-eval-after-load 'outline
    (add-hook 'ediff-prepare-buffer-hook #'show-all))
  ;; restore window layout when done
  (with-eval-after-load 'winner
    (add-hook 'ediff-quit-hook #'winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

(use-package rg)

(use-package aggressive-indent
  :init
  (dolist (hook '(emacs-lisp-mode-hook css-mode-hook))
    (add-hook hook #'aggressive-indent-mode)))

(use-package ace-link
  :commands (ace-link-help ace-link-org))

(use-package command-log-mode
  :commands (command-log-mode global-command-log-mode)
  :config
  (set-popup-rule! "*command-log*"
                   :size 40
                   :align 'right
                   :noselect t)
  (setq command-log-mode-auto-show t
        command-log-mode-open-log-turns-on-mode t))

;; Increase selected region by semantic units
(use-package expand-region
  :commands (er/expand-region er/contract-region er/mark-symbol er/mark-word))

(use-package pcre2el
  :commands rxt-quote-pcre
  :init (add-hook 'after-init-hook #'rxt-global-mode)
  )

;; Treat undo history as a tree
(use-package undo-tree
  :diminish undo-tree-mode
  :init (add-hook 'after-init-hook #'global-undo-tree-mode)
  :config
  (setq
   undo-tree-auto-save-history nil
   undo-tree-history-directory-alist `(("." . ,(concat sea-cache-dir "undo/")))))

;; Handles whitespace (tabs/spaces) settings externally. This way projects can
;; specify their own formatting rules.
(use-package editorconfig
  :config
  (add-hook 'after-init-hook #'editorconfig-mode)

  ;; editorconfig cannot procure the correct settings for extension-less files.
  ;; Executable scripts with a shebang line, for example. So why not use Emacs'
  ;; major mode to drop editorconfig a hint? This is accomplished by temporarily
  ;; appending an extension to `buffer-file-name' when we talk to editorconfig.
  (defvar sea-editorconfig-mode-alist
    '((sh-mode     . "sh")
      (python-mode . "py")
      (ruby-mode   . "rb")
      (perl-mode   . "pl")
      (php-mode    . "php"))
    "An alist mapping major modes to extensions. Used by
`sea*editorconfig-smart-detection' to give editorconfig filetype hints.")

  (defun sea*editorconfig-smart-detection (orig-fn &rest args)
    "Retrieve the properties for the current file. If it doesn't have an
extension, try to guess one."
    (let ((buffer-file-name
           (if (file-name-extension buffer-file-name)
               buffer-file-name
             (format "%s%s" buffer-file-name
                     (let ((ext (cdr (assq major-mode sea-editorconfig-mode-alist))))
                       (or (and ext (concat "." ext))
                           ""))))))
      (apply orig-fn args)))
  (advice-add #'editorconfig-call-editorconfig-exec :around #'sea*editorconfig-smart-detection)

  ;; Editorconfig makes indentation too rigid in Lisp modes, so tell
  ;; editorconfig to ignore indentation. I prefer dynamic indentation support
  ;; built into Emacs.
  (dolist (mode '(emacs-lisp-mode lisp-mode))
    (setq editorconfig-indentation-alist
          (assq-delete-all mode editorconfig-indentation-alist)))

  (defvar whitespace-style)
  (defun sea|editorconfig-whitespace-mode-maybe (&rest _)
    "Show whitespace-mode when file uses TABS (ew)."
    (when indent-tabs-mode
      (let ((whitespace-style '(face tabs tab-mark trailing-lines tail)))
        (whitespace-mode +1))))
  (add-hook 'editorconfig-custom-hooks #'sea|editorconfig-whitespace-mode-maybe))

(use-package tiny)

(use-package ialign
  :init
  (setq ialign-pcre-mode t)
  (setq ialign-initial-group -1)
  (setq ialign-initial-repeat t)
  (setq ialign-initial-regexp "([ ,=])")
)

;; markdown
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; 输入法切换
(when IS-WIN
  (defun emacs-ime-disable ()
    ;; (setq pgtk-use-im-context-on-new-connection nil)
    (w32-set-ime-open-status nil))

  (defun emacs-ime-enable ()
    (w32-set-ime-open-status t))
  (add-hook 'after-init-hook 'emacs-ime-disable)
  (add-hook 'evil-insert-state-exit-hook 'emacs-ime-disable)
  (add-hook 'evil-insert-state-entry-hook 'emacs-ime-enable)
  )

(use-package hungry-delete)
(global-hungry-delete-mode)


(provide 'init-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
