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
  )

(use-package magit-popup)

;; Gitflow externsion for Magit
(use-package magit-gitflow
  :diminish magit-gitflow-mode
  :init (add-hook 'magit-mode-hook #'turn-on-magit-gitflow))

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


;; Open github/gitlab/bitbucket page
(use-package browse-at-remote)

(provide 'init-vcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-vcs.el ends here
