(use-package eglot
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                          (eglot-ensure))))
         ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure))
  :init (setq eglot-send-changes-idle-time 0)
  :config
  (use-package consult-eglot
    :bind (:map eglot-mode-map
            ("C-M-." . consult-eglot-symbols))))

(cl-defmacro lsp-org-babel-enable (lang)
"Support LANG in org source code block."
(cl-check-type lang string)
(let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
       (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
  `(progn
     (defun ,intern-pre (info)
       (setq buffer-file-name (or (->> info caddr (alist-get :file))
                                  "org-src-babel.tmp"))
       (when (fboundp 'eglot-ensure)
         (eglot-ensure))
       )
     (put ',intern-pre 'function-documentation
          (format "Enable `%s' in the buffer of org source block (%s)."
                  'elgot (upcase ,lang)))

     (if (fboundp ',edit-pre)
         (advice-add ',edit-pre :after ',intern-pre)
       (progn
         (defun ,edit-pre (info)
           (,intern-pre info))
         (put ',edit-pre 'function-documentation
              (format "Prepare local buffer environment for org source block (%s)."
                      (upcase ,lang))))))))

(defconst org-babel-lang-list
  '("go" "python" "ipython" "ruby" "js" "css" "sass" "c" "rust" "java" "cpp" "c++" "shell")
  "The supported programming languages for interactive Babel.")
(dolist (lang org-babel-lang-list)
  (eval `(lsp-org-babel-enable ,lang)))

(provide 'init-elgot)
