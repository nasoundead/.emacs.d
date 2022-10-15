;; init-rust.el --- Initialize Golang configurations.	-*- lexical-binding: t -*-

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
;; Golang configurations.
;;

;;; Code:
;;
;;
;; (use-package rust-mode
;;   :init
;;   (autoload 'rust-mode "rust-mode" nil t)
;;   (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))
;; (use-package rust-mode
;;   :mode "\\.rs\\'"
;;   :init
;;   (setq rust-format-on-save t))
(use-package rustic
  :hook (rustic-mode . lsp)
  :hook (lsp-mode . (lambda()
                      (tree-sitter-hl-mode)
                      (lsp-rust-analyzer-cargo-watch-command "clippy")
                      (lsp-rust-analyzer-server-display-inlay-hints t)
                      (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
                      (lsp-rust-analyzer-display-chaining-hints t)
                      (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
                      (lsp-rust-analyzer-display-closure-return-type-hints t)
                      (lsp-rust-analyzer-display-parameter-hints nil)
                      (lsp-rust-analyzer-display-reborrow-hints nil)
                      ))

  )
(provide 'init-rust)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rust.el ends here
