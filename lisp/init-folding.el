;; init-folding.el
;; Flexible text folding
;; (use-package origami
;;   :hook (prog-mode . origami-mode)
;;   :init (setq origami-show-fold-header t)
;;   :after hydra
;;   :config
;;   (defhydra origami-hydra (:color blue :hint none)
;;     "
;;       _:_: recursively toggle node       _a_: toggle all nodes    _t_: toggle node
;;       _o_: show only current node        _u_: undo                _r_: redo
;;       _R_: reset
;;       "
;;     (":" origami-recursively-toggle-node)
;;     ("a" origami-toggle-all-nodes)
;;     ("t" origami-toggle-node)
;;     ("o" origami-show-only-node)
;;     ("u" origami-undo)
;;     ("r" origami-redo)
;;     ("R" origami-reset))

;;   :bind (:map origami-mode-map
;;               ("C-`" . origami-hydra/body))
;;   :config
;;   (face-spec-reset-face 'origami-fold-header-face))


(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))

(defun toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
      (if (condition-case nil
              (hs-toggle-hiding)
            (error t))
          (hs-show-all))
    (toggle-selective-display column)))

(load-library "hideshow")
(global-set-key (kbd "C-{") 'toggle-hiding)
(global-set-key (kbd "C-\\") 'toggle-selective-display)
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(defun display-code-line-counts (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'help-echo
                 (buffer-substring (overlay-start ov)
                                   (overlay-end ov)))))

(setq hs-set-up-overlay 'display-code-line-counts)

(provide 'init-folding)
