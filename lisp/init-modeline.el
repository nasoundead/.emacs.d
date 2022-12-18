;; init-modeline.el --- modeline.	-*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;
;; Modeline
;;
;; (use-package nyan-mode)
;; (add-hook 'sea-init-ui-hook #'nyan-mode)

;; (require 'simple-modeline)
;; (simple-modeline-mode)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
(provide 'init-modeline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-modeline.el ends here
