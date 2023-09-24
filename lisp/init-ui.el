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

(when emacs/>=29p
  (pixel-scroll-precision-mode t)
  )

(setq custom-safe-themes t)
(use-package color-theme-sanityinc-tomorrow)
(use-package doom-themes
  :ensure t
  :straight (doom-themes
             :type git
             :host github
             :repo "doomemacs/themes"))
(use-package gruber-darker-theme)
(use-package nord-theme)

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1))

(use-package beacon
  :init
  (beacon-mode 1))

;; (setq-default custom-enabled-themes '(doom-one))
;; (setq-default custom-enabled-themes '(sanityinc-tomorrow-night))
(setq-default custom-enabled-themes '(sanityinc-tomorrow-bright))
;; (setq-default custom-enabled-themes '(gruber-darker))
;; (setq-default custom-enabled-themes '(doom-tokyo-night))
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

(use-package dashboard
  :ensure t
  :init
  (dashboard-setup-startup-hook)
  :config
  ;; Set the title
  (setq dashboard-banner-logo-title "Welcome to Sea Emacs. Enjoy!")

  ;; Set the banner
  ;; (setq dashboard-startup-banner 'logo)
  (setq dashboard-startup-banner (or sea-logo 'official))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)

  (setq dashboard-set-init-info t)
  (setq dashboard-items '((recents  . 9)
                          (bookmarks . 5)
                          (projects . 9)
                          (agenda . 5)
                          (registers . 5)))
  (setq show-week-agenda-p t)
  )

;; highlight matching delimiters
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t)
(add-hook 'sea-init-ui-hook #'show-paren-mode)

;; undo/redo changes to Emacs' window layout
(defvar winner-dont-bind-my-keys t) ; I'll bind keys myself
(autoload 'winner-mode "winner" nil t)
(add-hook 'sea-init-ui-hook #'winner-mode)


;; postframe
(use-package posframe)


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



(reapply-themes)
(run-hooks 'sea-init-ui-hook)

(use-package switch-window
  :config
  (setq-default switch-window-shortcut-style 'alphabet)
  (setq-default switch-window-timeout nil)
  (global-set-key (kbd "C-x o") 'switch-window))

(use-package windmove
  :ensure nil
  :init (add-hook 'sea-init-ui-hook #'windmove-default-keybindings))




(use-package highlight-indent-guides
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'column)
  ;; (setq highlight-indent-guides-method 'character)
  :config
  (add-hook 'focus-in-hook #'highlight-indent-guides-auto-set-faces)
  ;; `highlight-indent-guides' breaks in these modes
  (add-hook! '(visual-line-mode-hook org-indent-mode-hook)
    (defun +indent-guides-disable-maybe-h ()
      (when highlight-indent-guides-mode
        (highlight-indent-guides-mode -1)))))

(use-package page-break-lines
  :hook ((prog-mode text-mode conf-mode) . page-break-lines-mode)
  )

(use-package zoom
  :config
  (custom-set-variables
    '(zoom-size '(0.618 . 0.618)))
  )


(provide 'init-ui)
