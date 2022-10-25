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
(use-package doom-themes)
(use-package gruber-darker-theme)

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1))
(use-package vscode-dark-plus-theme
  :config
  ;; (load-theme 'vscode-dark-plus t)
  ;; Remove the border around the TODO word on org-mode files
  (setq vscode-dark-plus-box-org-todo nil)

  ;; Do not set different heights for some org faces
  (setq vscode-dark-plus-scale-org-faces nil)

  ;; Avoid inverting hl-todo face
  (setq vscode-dark-plus-invert-hl-todo nil)
  )


;; (setq-default custom-enabled-themes '(vscode-dark-plus-theme))
;; (setq-default custom-enabled-themes '(sanityinc-tomorrow-bright))
(setq-default custom-enabled-themes '(sanityinc-tomorrow-night))
;; (setq-default custom-enabled-themes '(doom-dracula))
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
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  ;; Set the banner
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)

  (setq dashboard-set-init-info t)
  (setq dashboard-items '((recents  . 9)
                          (bookmarks . 5)
                          (projects . 9)
                          (agenda . 5)
                          (registers . 5)))
  (setq show-week-agenda-p t))

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

;; sort tab
;; (require 'sort-tab)
;; (sort-tab-mode 1)

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
              ;; ((eq system-type 'windows-nt) '("JetBrains Mono"  "宋体"))
              ((eq system-type 'windows-nt) '("JetBrainsMono Nerd Font"  "宋体"))
              ))
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

  ;; set font for emoji
  (set-fontset-font
   t
   '(#x1f300 . #x1fad0)
   (cond
    ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
    ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
    ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
    ((member "Symbola" (font-family-list)) "Symbola")
    ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji"))
   ;; Apple Color Emoji should be before Symbola, but Richard Stallman disabled it.
   ;; GNU Emacs Removes Color Emoji Support on the Mac
   ;; http://ergoemacs.org/misc/emacs_macos_emoji.html
   ;;
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


(use-package highlight-indent-guides
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character)
  :config
  (add-hook 'focus-in-hook #'highlight-indent-guides-auto-set-faces)
  ;; `highlight-indent-guides' breaks in these modes
  (add-hook! '(visual-line-mode-hook org-indent-mode-hook)
    (defun +indent-guides-disable-maybe-h ()
      (when highlight-indent-guides-mode
        (highlight-indent-guides-mode -1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;----------------------------------------------------------------------------
;; When splitting window, show (other-buffer) in the new window
;;----------------------------------------------------------------------------
(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))

(defun sanityinc/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") 'sanityinc/toggle-delete-other-windows)

;;----------------------------------------------------------------------------
;; Rearrange split windows
;;----------------------------------------------------------------------------
(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(global-set-key (kbd "C-x |") 'split-window-horizontally-instead)
(global-set-key (kbd "C-x _") 'split-window-vertically-instead)


;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
(defun sanityinc/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'sanityinc/split-window)
      (progn
        (jump-to-register :sanityinc/split-window)
        (setq this-command 'sanityinc/unsplit-window))
    (window-configuration-to-register :sanityinc/split-window)
    (switch-to-buffer-other-window nil)))

(global-set-key (kbd "<f7>") 'sanityinc/split-window)



(defun sanityinc/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))

(global-set-key (kbd "C-c <down>") 'sanityinc/toggle-current-window-dedication)

(provide 'init-ui)
