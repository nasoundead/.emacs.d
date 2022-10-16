;; vertico
(use-package vertico
  :ensure t
  :bind (:map vertico-map
          ("C-j" . vertico-next)
          ("C-k" . vertico-previous)
          :map minibuffer-local-map
          ("M-h" . backward-kill-word))
  :custom
  (vertico-cycle t)
  :config
  (setq vertico-resize nil)
  :init
  (vertico-mode))


;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))

(use-package vertico-posframe
  :init
  (vertico-posframe-mode)
  ;; :hook (vertico-mode . vertico-posframe-mode)
  :config
  (add-hook 'sea-reload-hook #'posframe-delete-all))

(use-package consult
  :defer t
  :init
  (if IS-WIN
      (progn
        (add-to-list 'process-coding-system-alist '("es" gbk . gbk))
        (add-to-list 'process-coding-system-alist '("explorer" gbk . gbk))
        (setq consult-locate-args (encode-coding-string "es.exe -i -p -r" 'gbk))))
  (advice-add #'multi-occur :override #'consult-multi-occur)
  :config
  (global-set-key (kbd "M-y") 'consult-yank-pop)
  (setq ;; consult-project-root-function #'doom-project-root
   consult-narrow-key "<"
   consult-line-numbers-widen t
   consult-async-min-input 2
   consult-async-refresh-delay  0.15
   consult-async-input-throttle 0.2
   consult-async-input-debounce 0.1)

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   :preview-key (kbd "C-o"))

  (consult-customize
   consult-theme
   :preview-key (list (kbd "C-o") :debounce 0.5 'any)))

(use-package embark
  :defer t
  :init
  (setq which-key-use-C-h-commands nil
        prefix-help-command #'embark-prefix-help-command)
  (map! [remap describe-bindings] #'embark-bindings
        "C-;"               #'embark-act  ; to be moved to :config default if accepted
        (:map minibuffer-local-map
          "C-;"               #'embark-act
          "C-c C-;"           #'embark-export
          "C-c C-l"           #'embark-collect
          :desc "Export to writable buffer" "C-c C-e" #'+vertico/embark-export-write)
        (:leader
          :desc "Actions" "a" #'embark-act)) ; to be moved to :config default if accepted
  :config
  (require 'consult)

  ;; (set-popup-rule! "^\\*Embark Export:" :size 0.35 :ttl 0 :quit nil)
  (cl-nsubstitute #'+vertico-embark-which-key-indicator #'embark-mixed-indicator embark-indicators)
  ;; add the package! target finder before the file target finder,
  ;; so we don't get a false positive match.
  (let ((pos (or (cl-position
                  'embark-target-file-at-point
                  embark-target-finders)
                 (length embark-target-finders))))
    (cl-callf2
        cons
        '+vertico-embark-target-package-fn
        (nthcdr pos embark-target-finders)))
  (map! (:map embark-file-map
          :desc "Open target with sudo"        "s"   #'sea/sudo-find-file
          :desc "Open magit-status of target"  "g"   #'+vertico/embark-magit-status
          )))


(provide 'init-vertico)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-vertico.el ends here
