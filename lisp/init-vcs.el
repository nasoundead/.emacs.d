;; init-vcs.el --- Initialize version control system configurations.	-*- lexical-binding: t -*-
;;; Code:

;; Git
(use-package magit
  :init
  ;; Must be set early to prevent ~/.emacs.d/transient from being created
  (setq transient-levels-file  (concat sea-etc-dir "transient/levels")
        transient-values-file  (concat sea-etc-dir "transient/values")
        transient-history-file (concat sea-etc-dir "transient/values")
        transient-history-file (concat sea-etc-dir "transient/history"))
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  ;; :bind
  ;; Magic
  ;; ("C-x g s" . magit-status)
  )

(use-package magit-popup)

;; Gitflow externsion for Magit
(use-package magit-gitflow
  :diminish magit-gitflow-mode
  :init (add-hook 'magit-mode-hook #'turn-on-magit-gitflow))

;; (use-package evil-magit
;;   :init
;;   (setq evil-magit-state 'normal
;;         evil-magit-use-z-for-folds t)
;;   :config
;;   (unmap! magit-mode-map
;;     ;; Replaced by z1, z2, z3, etc
;;     "M-1" "M-2" "M-3" "M-4"
;;     "1" "2" "3" "4"
;;     "0") ; moved to g=
;;   (evil-define-key* 'normal magit-status-mode-map [escape] nil) ; q is enough
;;   (evil-define-key* '(normal visual) magit-mode-map
;;     "%"  #'magit-gitflow-popup
;;     "zz" #'evil-scroll-line-to-center
;;     "g=" #'magit-diff-default-context)
;;   (define-key! 'normal
;;     (magit-status-mode-map
;;      magit-stash-mode-map
;;      magit-revision-mode-map
;;      magit-diff-mode-map)
;;     [tab] #'magit-section-t   (when-let (desc (assoc (car key) evil-magit-rebase-commands-w-descriptions))
;;                                 (setcar desc (cdr key))))
;;   (evil-define-key* evil-magit-state git-rebase-mode-map
;;     "gj" #'git-rebase-move-line-down
;;     "gk" #'git-rebase-move-line-up))

;; Git-Svn extension for Magit
(use-package magit-svn
  :diminish magit-svn-mode
  :init (add-hook 'magit-mode-hook #'magit-svn-mode))

;;; Pop up last commit information of current line
(use-package git-messenger
  :commands git-messenger:copy-message
  :bind (("C-x v p" . git-messenger:popup-message)
         :map git-messenger-map
         ("m" . git-messenger:copy-message))
  :init
  ;; Use magit-show-commit for showing status/diff commands
  (setq git-messenger:use-magit-popup t))

;; Walk through git revisions of a file
(use-package git-timemachine)

;; Highlighting regions by last updated time
(use-package smeargle
  :bind (("C-x v S" . smeargle)
         ("C-x v C" . smeargle-commits)
         ("C-x v R" . smeargle-clear)))

;; Git modes
(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)


;; Open github/gitlab/bitbucket page
(use-package browse-at-remote)

(provide 'init-vcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-vcs.el ends here
