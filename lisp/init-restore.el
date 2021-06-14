;; init-restore.el --- Initialize restore configurations.	-*- lexical-binding: t -*-
;;; Code:
;; save a list of open files in ~/.emacs.d/.emacs.desktop
;; (setq desktop-path (list sea-cache-dir)
;;       desktop-auto-save-timeout 600)
;; (desktop-save-mode 1)
(setq make-backup-files nil)

;; (use-package session)
;; (setq session-save-file (expand-file-name ".session" sea-cache-dir))
;; (setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
;; (setq session-save-file-coding-system 'utf-8)
;; (add-hook 'after-init-hook 'session-initialize)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
;; (setq desktop-globals-to-save
;;       (append '((comint-input-ring        . 50)
;;                 (compile-history          . 30)
;;                 desktop-missing-file-warning
;;                 (dired-regexp-history     . 20)
;;                 (extended-command-history . 30)
;;                 (face-name-history        . 20)
;;                 (file-name-history        . 100)
;;                 (grep-find-history        . 30)
;;                 (grep-history             . 30)
;;                 (ido-buffer-history       . 100)
;;                 (ido-last-directory-list  . 100)
;;                 (ido-work-directory-list  . 100)
;;                 (ido-work-file-list       . 100)
;;                 (ivy-history              . 100)
;;                 (magit-read-rev-history   . 50)
;;                 (minibuffer-history       . 50)
;;                 (org-clock-history        . 50)
;;                 (org-refile-history       . 50)
;;                 (org-tags-history         . 50)
;;                 (query-replace-history    . 60)
;;                 (read-expression-history  . 60)
;;                 (regexp-history           . 60)
;;                 (regexp-search-ring       . 20)
;;                 register-alist
;;                 (search-ring              . 20)
;;                 (shell-command-history    . 50)
;;                 tags-file-name
;;                 tags-table-list)))
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  ;; Set the title
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  ;; Set the banner
  ;; (setq dashboard-startup-banner [VALUE])
  ;; Value can be
  ;; 'official which displays the official emacs logo
  ;; 'logo which displays an alternative emacs logo
  ;; 1, 2 or 3 which displays one of the text banners
  ;; "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever image/text you would prefer

  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)

  ;; To disable shortcut "jump" indicators for each section, set
  (setq dashboard-show-shortcuts nil)

  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5)))
  )

(provide 'init-restore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-restore.el ends here
