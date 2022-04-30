;;; package --- python configs
;;; Commentary:
;;; Contains my python configs

(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))
(use-package ein
  :init
  (setq ein:use-auto-complete t))
(use-package py-autopep8
  :init
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))


(provide 'init-py)
;;; init-py.el ends here
