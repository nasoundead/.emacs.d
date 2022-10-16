;; init-company.el --- Initialize company configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 Bruce Wong

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
;; Auto-completion configurations.
;;

;;; Code:
(use-package company
  :commands company-complete-common company-manual-begin company-grab-line
  ;; :after-call pre-command-hook after-find-file
  :init
  (setq company-minimum-prefix-length 2
        company-tooltip-limit 14
        ;; Trigger completion immediately.
        company-idle-delay 0
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes
        '(not erc-mode message-mode help-mode gud-mode eshell-mode)
        company-backends '(company-capf)
        company-frontends
        '(company-pseudo-tooltip-frontend
          company-echo-metadata-frontend))
  :hook (after-init . global-company-mode)
  :config
  (add-hook 'company-mode-hook #'evil-normalize-keymaps)
  ;; Allow users to switch between backends on the fly. E.g. C-x C-s followed
  ;; by C-x C-n, will switch from `company-yasnippet' to
  ;; `company-dabbrev-code'.
  (defadvice! +company--abort-previous-a (&rest _)
    :before #'company-begin-backend
    (company-abort))

  (add-hook 'company-mode-hook #'+company-init-backends-h)
  )

(use-package company-statistics
  :after company
  :config
  (setq company-statistics-file (concat sea-cache-dir "company-stats-cache.el"))
  (quiet! (company-statistics-mode +1)))

(use-package company-dict
  :commands company-dict
  :config
  (defun +company|enable-project-dicts (mode &rest _)
    "Enable per-project dictionaries."
    (if (symbol-value mode)
        (cl-pushnew mode company-dict-minor-mode-list :test #'eq)
      (setq company-dict-minor-mode-list (delq mode company-dict-minor-mode-list))))
  (add-hook 'sea-project-hook #'+company|enable-project-dicts))


;; (require 'company-tabnine)
;; (add-to-list 'company-backends #'company-tabnine)


(provide 'init-company)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ; init-company.el ends here
