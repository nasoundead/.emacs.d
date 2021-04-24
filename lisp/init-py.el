;;; package --- python configs
;;; Commentary:
;;; Contains my python configs

;; (use-package elpy
;;   :ensure t
;;   :init
;;   (setq elpy-rpc-backend "jedi")
;;   (add-hook 'python-mode-hook 'elpy-mode)
;;   :config
;;   (with-eval-after-load 'elpy
;;     ;; (setq python-shell-interpreter "ipython"
;;     ;;       python-shell-interpreter-args "-i --simple-prompt")
;;     (setq python-shell-interpreter "jupyter"
;;           python-shell-interpreter-args "console --simple-prompt"
;;           python-shell-prompt-detect-failure-warning nil)
;;     (add-to-list 'python-shell-completion-native-disabled-interpreters
;;                  "jupyter")
;;     (when (require 'flycheck nil t)
;;       (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;       ;; (setq elpy-modules (delq 'elpy-module-company elpy-modules))
;;       ;; (add-hook 'elpy-mode-hook 'flycheck-mode)
;;       )
;;     )
;;   :bind (("M-*" . pop-tag-mark))
;;   )
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))
(use-package ein
  :init
  (setq ein:use-auto-complete t))
(use-package py-autopep8
  :init
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))
(use-package realgud
  :ensure t)
(use-package indent-tools
  :ensure t
  :init
  (add-hook 'python-mode-hook 'indent-tools-minor-mode)
  :bind
  ("C-c i" . indent-tools-hydra/body))


(provide 'init-py)
;;; init-py.el ends here
