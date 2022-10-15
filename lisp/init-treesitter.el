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
;; treesitter configurations.
;;

;;; Code:
(use-package tree-sitter
  :hook (python-mode . (lambda()
                         (tree-sitter-hl-mode)))
  )
(use-package tree-sitter-langs)
(global-tree-sitter-mode)

;; (add-hook 'python-mode-hook 'tree-sitter-hl-mode)
;; (add-hook 'rustic-mode-hook 'tree-sitter-hl-mode)

;; (defvar tree-sitter-hl-mode-list
;;   '("go" "python" "ipython" "ruby" "js" "css" "sass" "C" "rust" "java"))
;; (dolist (lang org-babel-lang-list)
;;   (eval `(lsp-org-babel-enable ,lang)))

(provide 'init-treesitter)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ; init-treesitter.el ends here
