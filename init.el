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
        gc-cons-percentage 0.15)
  )

(add-hook 'emacs-startup-hook #'sea-finalize t)
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

(defconst sea-bin-dir
  (expand-file-name "bin/" user-emacs-directory)
  "bin directory.")

(defconst sea-site-lisp-dir
  (expand-file-name "site-lisp/" user-emacs-directory)
  "site-lisp directory.")

(defconst EMACS26+ (> emacs-major-version 25))
(defconst EMACS27+ (> emacs-major-version 26))
(defconst EMACS28+ (> emacs-major-version 27))

(defconst sea-logo (expand-file-name
                    (if (display-graphic-p) "logo.png" "banner.txt")
                    user-emacs-directory)
  "Set logo. nil means official logo.")

(defconst IS-MAC
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst IS-LINUX
  (eq system-type 'gnu/linux)
  "Are we running on a Linux system?")

(defconst IS-WIN
  (eq system-type 'windows-nt)
  "Are we running on a Linux system?")

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/mac-ns-p
  (eq window-system 'ns)
  "Are we running on a GNUstep or Macintosh Cocoa display?")

(defconst sys/mac-cocoa-p
  (featurep 'cocoa)
  "Are we running with Cocoa on a Mac system?")

(defconst sys/mac-port-p
  (eq window-system 'mac)
  "Are we running a macport build on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defconst emacs/>=28p
  (>= emacs-major-version 28)
  "Emacs is 28 or above.")

(defconst emacs/>=29p
  (>= emacs-major-version 29)
  "Emacs is 29 or above.")


;;; Directories/files
(defvar sea-emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")


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
(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))
(add-subdirs-to-load-path sea-site-lisp-dir)

(require 'init-package)
(require 'init-basic)
(require 'init-lib)
(require 'init-funcs)
(require 'init-evil)
(require 'init-keybinds)

(when (display-graphic-p)
  (require 'init-font)
  (require 'init-ui)
  )

(require 'init-modeline)
(require 'init-edit)
(require 'init-smartparens)
(require 'init-folding)
(require 'init-hydra)
(require 'init-highlight)
(require 'init-lookup)
(require 'init-vertico)
(require 'init-corfu)
(require 'init-treesitter)

(require 'init-yasnippet)
(require 'init-projectile)
(require 'init-flycheck)
(require 'init-treemacs)

(require 'init-utils)
(require 'init-vcs)
(require 'init-dired)
(require 'init-restore)
(require 'init-org)
(require 'init-eshell)
(require 'init-prog)
;; (require 'init-lsp)
(require 'init-c)
(require 'init-go)
(require 'init-py)
(require 'init-rust)
(require 'init-js)
(require 'init-web)

;; Start server
(require 'server)
(server-mode 1)
(unless (server-running-p)
  (server-start))

(dolist (dir (list sea-local-dir sea-cache-dir sea-etc-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; (setq custom-file (concat sea-cache-dir "custom.el"))
;; (load custom-file t t)
(put 'dired-find-alternate-file 'disabled nil)
