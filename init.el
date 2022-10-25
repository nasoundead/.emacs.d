;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:

(unless after-init-time
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 1.0))

(defun sea-finalize ()
  "The main starup function."
  (dolist (hook '(sea-init-hook))
    (run-hook-with-args hook)
  (run-hook-wrapped 'sea-post-init-hook #'sea-try-run-hook))

  (sea-load-autoload)
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.15))

(add-hook 'emacs-startup-hook #'sea-finalize t)

;;; Directories/files
(defvar sea-emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defconst sea-autoload-dir
  (expand-file-name "autoload/" user-emacs-directory)
  "autoload directory.")

(defconst sea-local-dir
  (expand-file-name ".local/" user-emacs-directory)
  "local directory.")

(defconst sea-private-dir
  (expand-file-name "./" user-emacs-directory)
  "private directory.")

(defconst sea-cache-dir
  (expand-file-name "cache/" sea-local-dir)
  "Cache directory.")

(defconst sea-etc-dir
  (expand-file-name "etc/" sea-local-dir)
  "etc directory.")

(defconst sea-core-dir
  (expand-file-name "lisp/" user-emacs-directory)
  "core directory.")

(defconst sea-site-lisp-dir
  (expand-file-name "site-lisp/" user-emacs-directory)
  "site-lisp directory.")

(defconst EMACS26+ (> emacs-major-version 25))
(defconst EMACS27+ (> emacs-major-version 26))

(defconst IS-MAC
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst IS-LINUX
  (eq system-type 'gnu/linux)
  "Are we running on a Linux system?")

(defconst IS-WIN
  (eq system-type 'windows-nt)
  "Are we running on a Linux system?")

(defvar sea-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, all sea functions will be verbose. Set DEBUG=1 in the command
line or use --debug-init to enable this.")
(defvar sea-project-hook nil
  "Hook run when a project is enabled. The name of the project's mode and its
state are passed in.")
(defvar sea-init-hook nil
  "Hooks run after all init.el files are loaded, including your private and all
module init.el files, but before their config.el files are loaded.")

(defvar sea-post-init-hook nil
  "A list of hooks run when sea is fully initialized. Fires near the end of
`emacs-startup-hook', as late as possible. Guaranteed to run after everything
else (except for `window-setup-hook').")

(defvar sea-reload-hook nil
  "A list of hooks to run when `sea/reload' is called.")

(defun sea-try-run-hook (hook)
  "Run HOOK (a hook function), but handle errors better, to make debugging
issues easier.
Meant to be used with `run-hook-wrapped'."
  (let ((gc-cons-threshold 20000000))
    (when sea-debug-mode
      (message "Running sea hook: %s" hook))
    (condition-case e
        (funcall hook)
      ((debug error)
       (signal 'sea-hook-error (list hook e))))
    ;; return nil so `run-hook-wrapped' won't short circuit
    nil))

(add-to-list 'load-path sea-core-dir)
(add-to-list 'load-path sea-site-lisp-dir)
(require 'init-package)
(require 'init-basic)
(require 'init-lib)
(require 'init-funcs)
(require 'init-evil)
(require 'init-ui)
(require 'init-modeline)
(require 'init-edit)
(require 'init-smartparens)
(require 'init-folding)
(require 'init-highlight)
(require 'init-lookup)
(require 'init-keybinds)
;; (require 'init-ivy)
(require 'init-vertico)
(require 'init-company)
;; (require 'init-corfu)
(require 'init-treesitter)

(require 'init-yasnippet)
(require 'init-projectile)
(require 'init-flycheck)
(require 'init-treemacs)

(require 'init-utils)
(require 'init-vcs)
(require 'init-dired)
(require 'init-c)
(require 'init-restore)
(require 'init-org)
(require 'init-eshell)
(require 'init-prog)
(require 'init-emacs-lisp)
(require 'init-lsp)
(require 'init-go)
(require 'init-py)
(require 'init-rust)
(require 'init-js)
(require 'init-web)

;; Start server
(require 'server)
(unless (server-running-p)
  (server-start))

(dolist (dir (list sea-local-dir sea-cache-dir sea-etc-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; (setq custom-file (concat sea-cache-dir "custom.el"))
;; (load custom-file t t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes 'nil)
 '(package-selected-packages
   '(vscode-dark-plus-theme toml-mode ztree youdao-dictionary whitespace-cleanup-mode which-key wgrep-ag web-mode web-beautify visual-regexp-steroids vimrc-mode vertico-posframe use-package unicode-fonts unicode-escape undo-tree treemacs-tab-bar treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil treemacs-all-the-icons tree-sitter-langs tiny tide tern tao-theme symbol-overlay switch-window swift-mode smeargle smartparens skewer-mode shrink-path scss-mode rustic rg restart-emacs rainbow-mode rainbow-delimiters quickrun py-autopep8 powershell plantuml-mode php-mode pcre2el origami org-projectile org-bullets orderless ob-ipython modern-cpp-font-lock mocha marginalia magit-svn magit-gitflow macrostep lsp-ui lsp-python-ms lsp-pyright lsp-java lsp-ivy json-mode js2-refactor js-comint ivy-xref ido-vertical-mode ibuffer-projectile hl-todo highlight-indent-guides hide-mode-line haml-mode gruber-darker-theme go-tag go-playground go-impl go-gen-test go-fill-struct go-dlv git-timemachine git-messenger general flymd flycheck-golangci-lint flx fish-mode evil-snipe evil-multiedit evil-mc evil-matchit evil-lion evil-exchange evil-escape evil-embrace evil-easymotion evil-commentary evil-collection evil-args eshell-z eshell-git-prompt emmet-mode embark elisp-slime-nav eldoc-eval ein editorconfig easy-kill dumb-jump doom-themes dockerfile-mode diminish diff-hl dictionary deft dashboard dash-docs css-eldoc csharp-mode counsel corfu consult company-statistics company-dict company-c-headers command-log-mode color-theme-sanityinc-tomorrow coffee-mode ccls browse-url-dwim browse-kill-ring browse-at-remote better-jumper anzu amx ample-theme aggressive-indent ag ace-link)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:background "#46D9FF"))))
 '(diff-hl-delete ((t (:background "#ff6c6b"))))
 '(diff-hl-insert ((t (:background "#98be65"))))
 '(hl-todo ((t (:box t :bold t))))
 '(lsp-ui-sideline-code-action ((t (:inherit warning))))
 '(org-table ((t (:family "Ubuntu Mono")))))
