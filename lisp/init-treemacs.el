;; init-treemacs.el --- Initialize treemacs.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 Vincent Zhang

;; Author: Bruce Wong <nasoundead@163.com>
;; URL: https://github.com/nasoundead/.emacs.d.minimal

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Treemacs: A tree layout file explorer.
;;

;;; Code:
;; A tree layout file explorer
(use-package treemacs
  :defines winum-keymap
  :bind (
         ;; ([f8]        . treemacs)
         :map treemacs-mode-map
         ([mouse-1]   . treemacs-single-click-expand-action))

  :config
  (setq
   treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
   treemacs-width                   30
   treemacs-missing-project-action  'remove
   treemacs-sorting                 'alphabetic-asc
   )

  (treemacs-follow-mode t)
  ;; (treemacs-load-all-the-icons-with-workaround-font)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-hide-gitignored-files-mode nil)
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))
  )



;; Projectile integration for treemacs
(use-package treemacs-projectile
  :after treemacs projectile
  :bind (([M-f8] . treemacs-projectile)
         :map projectile-command-map
         ("h" . treemacs-projectile)))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :hook (dired-mode . treemacs-icons-dired-mode)
  )

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))
(use-package treemacs-evil
  :after (treemacs evil))
(use-package treemacs-magit
  :after treemacs magit
  :commands treemacs-magit--schedule-update
  :hook ((magit-post-commit
          git-commit-post-finish
          magit-post-stage
          magit-post-unstage)
         . treemacs-magit--schedule-update))

(use-package treemacs-all-the-icons
  :after treemacs)

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(provide 'init-treemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-treemacs.el ends here
