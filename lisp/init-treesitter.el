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
(use-package treesit-auto
  :straight (treesit-auto
              :type git
              :host github
              :repo "renzmann/treesit-auto")

  :custom
  (treesit-auto-install 'prompt)
  (treesit-font-lock-level 4)
  :config
  (global-treesit-auto-mode)
  )

(use-package combobulate
  :straight (combobulate
             :type git
             :host github
             :repo "mickeynp/combobulate")

  :custom
  (combobulate-key-prefix "C-c o")
  :hook
  ((python-ts-mode
    js-ts-mode
    css-ts-mode
    yaml-ts-mode
    typescript-ts-mode
    tsx-ts-mode
    rust-ts-mode
    go-ts-mode) . combobulate-mode)
  )


(provide 'init-treesitter)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ; init-treesitter.el ends here
