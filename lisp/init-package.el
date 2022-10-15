;; init-package.el ---.	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'package)
(setq package-enable-at-startup nil
      package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
			 ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
			 ("org" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")
			 ))

(package-initialize)
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
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
