;;; init-ui.el -*- lexical-binding: t; -*-
(defvar sea-init-ui-hook nil
  "ui hook")

;; Title
(setq frame-title-format
      '("GNU Emacs " emacs-version "@" user-login-name " : "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(setq icon-title-format frame-title-format)


(setq custom-safe-themes t)
(use-package color-theme-sanityinc-tomorrow)
;; (use-package zenburn-theme)
;; (use-package srcery-theme)
;; (use-package darktooth-theme)
(use-package doom-themes)
;; (setq-default custom-enabled-themes '(sanityinc-tomorrow-bright))
;; (setq-default custom-enabled-themes '(doom-zenburn))
;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)

(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (reapply-themes))


                                        ;!; highlight matching delimiters
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t)
(add-hook 'sea-init-ui-hook #'show-paren-mode)

;; undo/redo changes to Emacs' window layout
(defvar winner-dont-bind-my-keys t) ; I'll bind keys myself
(autoload 'winner-mode "winner" nil t)
(add-hook 'sea-init-ui-hook #'winner-mode)


;; Restore old window configurations
(use-package winner
  :ensure nil
  :init
  (setq winner-boring-buffers '("*Completions*"
                                "*Compile-Log*"
                                "*inferior-lisp*"
                                "*Fuzzy Completions*"
                                "*Apropos*"
                                "*Help*"
                                "*cvs*"
                                "*Buffer List*"
                                "*Ibuffer*"
                                "*esh command on file*")))


(defun sea/init-ui (&optional frame)
  "Set the theme and load the font, in that order."
  (setq fonts
        (cond ((eq system-type 'darwin)     '("Monaco"    "STHeiti"))
              ((eq system-type 'gnu/linux)  '("Ubuntu Mono"     "WenQuanYi Micro Hei Mono"))
              ((eq system-type 'windows-nt) '("Monaco"  "宋体"))))
  (set-face-attribute 'default nil :font
                      (format "%s:pixelsize=%d" (car fonts) 16))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family (car (cdr fonts)))))
  ;; Fix chinese font width and rescale
  (setq face-font-rescale-alist '(("宋体". 1.0) ("Microsoft Yahei" . 1.2) ("WenQuanYi Micro Hei Mono" . 1.2) ("STHeiti". 1.2)))

  (custom-set-faces
   '(org-table ((t (:family "Ubuntu Mono"))))
   )

  ;; (require 'font-lock+)
  (use-package unicode-fonts)
  (reapply-themes)
  (run-hooks 'sea-init-ui-hook))

(add-hook 'after-init-hook #'sea/init-ui)

(require 'autoloads sea-autoload-file t)

(use-package switch-window
  :config
  (setq-default switch-window-shortcut-style 'alphabet)
  (setq-default switch-window-timeout nil)
  (global-set-key (kbd "C-x o") 'switch-window))

(use-package windmove
  :ensure nil
  :init (add-hook 'sea-init-ui-hook #'windmove-default-keybindings))


(use-package all-the-icons
  :commands (all-the-icons-octicon all-the-icons-faicon all-the-icons-fileicon
                                   all-the-icons-wicon all-the-icons-material all-the-icons-alltheicon)
  :init
  (defun sea*disable-all-the-icons-in-tty (orig-fn &rest args)
    (when (display-graphic-p)
      (apply orig-fn args)))
  :config
  (setq inhibit-compacting-font-caches t)
  ;; all-the-icons doesn't work in the terminal, so we "disable" it.
  (dolist (fn '(all-the-icons-octicon all-the-icons-material
                                      all-the-icons-faicon all-the-icons-fileicon
                                      all-the-icons-wicon all-the-icons-alltheicon))
    (advice-add fn :around #'sea*disable-all-the-icons-in-tty)))

(use-package mini-frame
  :init (add-hook 'sea-init-ui-hook #'mini-frame-mode)
  :config
  (custom-set-variables
   '(mini-frame-show-parameters
     '((top . 0)
       (width . 0.7)
       (left . 0.5)))))

(provide 'init-ui)
