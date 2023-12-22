;;; init-basic.elAuthor: Haibo Wang <nasoundead@163.com>
;;; Code:
;; Key Modifiers
(when IS-WIN
  ;; make PC keyboard's Win key or other to type Super or Hyper, for emacs running on Windows.
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super) ; Left Windows key

  (setq w32-pass-rwindow-to-system nil)
  (setq w32-rwindow-modifier 'super) ; Right Windows key

  (setq w32-pass-apps-to-system nil)

  (setq w32-apps-modifier 'hyper)
  (setq w32-rwindow-modifier 'hyper)
  )

;; coding
(defun windows-shell-mode-coding ()
    (set-buffer-file-coding-system 'gbk)
    (set-buffer-process-coding-system 'gbk 'gbk))
(defun python-encode-in-org-babel-execute (func body params)
    (let ((coding-system-for-write 'utf-8))
      (funcall func body params)))
(cond
 ((eq system-type 'IS-WIN)
  (set-language-environment "chinese-gbk")
  (prefer-coding-system 'utf-8)
  (set-terminal-coding-system 'gbk)
  (modify-coding-system-alist 'process "*" 'gbk)
  (add-hook 'shell-mode-hook #'windows-shell-mode-coding)
  (add-hook 'inferior-python-mode-hook #'windows-shell-mode-coding)
  (advice-add #'org-babel-execute:python :around
              #'python-encode-in-org-babel-execute))
 (t
  (set-language-environment "UTF-8")
  (prefer-coding-system 'utf-8)))

;; Environment
(when (or IS-MAC IS-LINUX)
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"))
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

(defcustom sea/buffer-skip-regexp
  (rx bos
      (or (or "*Backtrace*" "*Compile-Log*" "*Completions*"
              "*Messages*" "*scratch*" "*Help*"
              "*package*" "*Warnings*"
              "*Async-native-compile-log*")
          (seq "magit-diff" (zero-or-more anything))
          (seq "magit-process" (zero-or-more anything))
          (seq "magit-revision" (zero-or-more anything))
          (seq "magit-stash" (zero-or-more anything)))
      eos)
  "Regular expression matching buffers ignored by `next-buffer' and
`previous-buffer'."
  :type 'regexp)
(defun sea/buffer-skip-p (window buffer bury-or-kill)
  "Return t if BUFFER name matches `sea/buffer-skip-regexp'."
  (string-match-p sea/buffer-skip-regexp (buffer-name buffer)))
(setq switch-to-prev-buffer-skip 'sea/buffer-skip-p)

;; Show native line numbers if possible, otherwise use linum
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode yaml-mode conf-mode) . display-line-numbers-mode)
  :init (setq display-line-numbers-width-start t))

(defadvice term (before force-bash)
  (interactive (list "/usr/local/bin/fish")))
(ad-activate 'term)
(add-hook 'term-mode-hook #'hide-mode-line-mode)
(add-hook 'term-mode-hook (lambda ()
                            (linum-mode -1)
                            (setq left-fringe-width 0)
                            (setq right-fringe-width 0)
                            (local-unset-key (kbd "C-r"))))

(use-package hide-mode-line)

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode)
  :config
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 5)
  ;; embolden local bindings
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom))

(use-package ido-vertical-mode
  :init
  (ido-vertical-mode 1)
  :config
  (setq ido-vertical-show-count 1))


(setq kill-ring-max 200)
;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

(global-hl-line-mode)

;; Kill & Mark things easily
(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

;; Interactively insert items from kill-ring
(use-package browse-kill-ring
  :bind ("C-c k" . browse-kill-ring)
  :init (add-hook 'after-init-hook #'browse-kill-ring-default-keybindings))

(use-package dash
  :ensure t
  :defer t)
(use-package f
  :ensure t
  :defer t)
(use-package s
  :ensure t
  :defer t)
(use-package eldoc-eval)
(use-package shrink-path
  :commands (shrink-path-prompt shrink-path-file-mixed))

(provide 'init-basic)
;;; base ends here
