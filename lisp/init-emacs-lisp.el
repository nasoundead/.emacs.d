;; init-emacs-lisp.el --- Initialize Emacs Lisp configurations.	-*- lexical-binding: t -*-

;; Emacs lisp mode
;; Note: `elisp-mode' was called `emacs-lisp-mode' in <=24


;; Show function arglist or variable docstring
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :init
  ;; Enable Eldoc in lisp modes in 24
  ;; `global-eldoc-mode' is enabled by default in 25.
  (unless (fboundp 'global-eldoc-mode)
    (dolist (hook '(emacs-lisp-mode-hook
                    lisp-interaction-mode-hook
                    ielm-mode-hook
                    eval-expression-minibuffer-setup-hook))
      (add-hook hook #'eldoc-mode))))

;; Interactive macro expander
(use-package macrostep
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)
              :map lisp-interaction-mode-map
              ("C-c e" . macrostep-expand)))

;; Make M-. and M-, work in elisp like they do in slime.
;; `xref' is perfect since 25, so only use in <=24.
(unless (featurep 'xref)
  (use-package elisp-slime-nav
    :diminish elisp-slime-nav-mode
    :bind (:map elisp-slime-nav-mode-map
                ("C-h o" . elisp-slime-nav-describe-elisp-thing-at-point))
    ;; :init (dolist (hook '(emacs-lisp-mode-hook
    ;;                       lisp-interaction-mode-hook
    ;;                       ielm-mode-hook))
    ;;         (add-hook hook #'turn-on-elisp-slime-nav-mode))
    )
  )

(defun recompile-elpa ()
  "Recompile packages in elpa directory. Useful if you switch Emacs versions."
  (interactive)
  (byte-recompile-directory package-user-dir nil t))


(provide 'init-emacs-lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-emacs-lisp.el ends here
