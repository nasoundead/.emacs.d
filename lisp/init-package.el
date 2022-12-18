;; init-package.el ---.	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; Install use-package
(straight-use-package 'use-package)
(require 'package)

(use-package straight
  :custom (straight-use-package-by-default t))

(use-package anzu
  :straight (anzu
             :type git
             :host github
             :repo "syohex/emacs-anzu")
  :init (global-anzu-mode +1)
  :bind ("C-q" . anzu-query-replace-regexp))


;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/"))
;; (package-initialize)
;; (package-refresh-contents)
;; (unless (package-installed-p 'evil)
;;   (package-install 'evil))
;; (require 'evil)
;; (evil-mode 1)

;; (setq package-enable-at-startup nil
;;       package-archives '(("gnu"   . "http://mirrors.bfsu.edu.cn/elpa/gnu/")
;;                          ("melpa" . "http://mirrors.bfsu.edu.cn/elpa/melpa/"))
;;       ;; package-archives
;;       ;; '(("melpa" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/")
;;       ;;   ("org"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/org/")
;;       ;;   ("gnu"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/"))
;;       ;; package-archives
;;       ;; '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;       ;;   ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
;;       ;;   ;; ("org" . "http://orgmode.org/elpa/")
;;       ;;   ("marmalade" . "http://marmalade-repo.org/packages/")
;;       ;;   )
;;       )

;; (package-initialize)
;; ;; Bootstrap `use-package'
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; (require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(use-package diminish)



(defvar sea-autoload-file (concat sea-local-dir "autoloads.el")
  "The path of autoload file which has all the autoload functions.")

(defun sea-load-autoload ()
  "Load `sea-autoload-file'."
  (if (file-exists-p sea-autoload-file)
        (load sea-autoload-file)
    (progn
        (sea/generate-autoload-file)
        (load sea-autoload-file))))



(defun sea/generate-autoload-file ()
  "Extract autload file from each star to `sea-autoload-file'."
  (interactive)
  (let ((autoload-file-list
         (file-expand-wildcards
          (expand-file-name "*.el" sea-autoload-dir))))
    (dolist (file (reverse autoload-file-list))
      (message
       (cond ((update-file-autoloads file t sea-autoload-file)
              "Nothing in %s")
             (t "Scanned %s"))
       (file-relative-name file user-emacs-directory)))
    ))



(provide 'init-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
