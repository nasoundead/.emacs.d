;; init-projectile.el --- Initialize projectile configurations.	-*- lexical-binding: t -*-
;;; Code:

;; Manage and navigate projects
(use-package projectile
  :bind (("s-t" . projectile-find-file))
  :init (add-hook 'after-init-hook #'projectile-mode)
  :config
  (setq projectile-mode-line
        '(:eval (format "[%s]" (projectile-project-name))))

  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" sea-cache-dir))

  (setq projectile-completion-system 'ivy)

  (setq projectile-sort-order 'recentf)
  (setq projectile-use-git-grep t)

  (setq projectile-switch-project-action
        '(lambda ()
           (venv-projectile-auto-workon)
           (projectile-find-file)))

  ;; Faster indexing on Windows
  ;; `ripgrep' is the fastest
  (when IS-WIN
    (when (executable-find "rg")
      (setq projectile-generic-command "rg -0 --files --color=never --hidden")
      (setq projectile-indexing-method 'alien)
      (setq projectile-enable-caching nil))

    ;; FIXME: too slow while getting submodule files on Windows
    (setq projectile-git-submodule-command ""))

  ;; Support Perforce project
  (let ((val (or (getenv "P4CONFIG") ".p4config")))
    (add-to-list 'projectile-project-root-files-bottom-up val)))

;; Group ibuffer's list by project root
(use-package ibuffer-projectile
  :bind ("C-x C-b" . ibuffer)
  :init
  (setq ibuffer-filter-group-name-face 'font-lock-function-name-face)
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-auto-mode 1)
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))
				
(provide 'init-projectile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-projectile.el ends here
