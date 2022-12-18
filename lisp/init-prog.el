;; init-prog.el --- Initialize prog configurations.	-*- lexical-binding: t -*-
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 3.1.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Configurations for prog mode.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; (use-package prog-mode
;;   :ensure nil
;;   :init
;;   ;; Prettify Symbols
;;   ;; e.g. display “lambda” as “λ”
;;   (when (boundp 'global-prettify-symbols-mode)
;;     (add-hook 'after-init-hook #'global-prettify-symbols-mode)
;;     (add-hook 'emacs-lisp-mode-hook
;;               (lambda ()
;;                 (push '("<=" . ?≤) prettify-symbols-alist)))))

(use-package quickrun)
(use-package powershell)
(use-package dockerfile-mode :mode "Dockerfile\\'")
(use-package vimrc-mode)

;; New `conf-toml-mode' in Emacs26
(unless (fboundp 'conf-toml-mode)
  (use-package toml-mode))

(use-package editorconfig
  :diminish editorconfig-mode
  :init (add-hook 'prog-mode-hook #'editorconfig-mode))

;; New `bat-mode' in 25, only use `batch-mode' in 24.
(unless (fboundp 'bat-mode)
  (use-package batch-mode
    :mode (("\\.\\(cmd\\|bat\\)$" . batch-mode))))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode))
  :init
  (when (executable-find "multimarkdown")
    (setq markdown-command "multimarkdown"))
  :config
  ;; On the fly markdown preview
  (use-package flymd
    :bind (:map markdown-mode-map ("C-c C-c f" . flymd-flyit))))


(use-package robot-mode
  :ensure nil
  :load-path "site-lisp"
  :commands robot-mode
  :mode "\\.robot\\'")

;; (add-hook 'prog-mode-hook 'yafolding-mode)

(provide 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
