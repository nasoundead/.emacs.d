;; init-lsp.el --- Initialize lsp configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 Haibo Wang

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


;; Emacs client for the Language Server Protocol
;; https://github.com/emacs-lsp/lsp-mode#supported-languages
(use-package lsp-mode
  :defines (lsp-clients-python-library-directories lsp-rust-rls-server-command)
  :commands (lsp-enable-which-key-integration lsp-format-buffer lsp-organize-imports)
  :diminish
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                          (lsp-deferred))))
         (lsp-mode . (lambda ()
                       ;; Integrate `which-key'
                       (lsp-enable-which-key-integration)
                       ;; Format and organize imports
                       (add-hook 'before-save-hook #'lsp-format-buffer t t)
                       (add-hook 'before-save-hook #'lsp-organize-imports t t)
                       )))
  :bind (:map lsp-mode-map
          ("C-c C-d" . lsp-describe-thing-at-point)
          ([remap xref-find-definitions] . lsp-find-definition)
          ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init
  ;; @see https://github.com/emacs-lsp/lsp-mode#performance
  (setq read-process-output-max (* 1024 1024)) ;; 1MB

  (setq lsp-auto-guess-root nil      ; Detect project root
        lsp-keep-workspace-alive nil ; Auto-kill LSP server
        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil
        ;; lsp-prefer-capf t
        lsp-keymap-prefix "C-c l")

  ;; For `lsp-clients'
  (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/"))
  (unless (executable-find "rls")
    (setq lsp-rust-rls-server-command '("rustup" "run" "stable" "rls"))))

(use-package lsp-ui
  :custom-face
  (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :bind (("C-c u" . lsp-ui-imenu)
         :map lsp-ui-mode-map
         ("M-<f6>" . lsp-ui-hydra/body))
  :hook (lsp-mode . lsp-ui-mode)
  :init (setq lsp-ui-doc-enable t
              ;; lsp-ui-doc-use-webkit nil
              ;; lsp-ui-doc-delay 0.2
              lsp-ui-doc-include-signature t
              lsp-ui-doc-position 'at-point
              lsp-ui-doc-border (face-foreground 'default)
              lsp-eldoc-enable-hover nil ; Disable eldoc displays in minibuffer

              lsp-ui-sideline-enable t
              lsp-ui-sideline-show-hover nil
              lsp-ui-sideline-show-diagnostics nil
              lsp-ui-sideline-ignore-duplicate t

              lsp-ui-imenu-enable t
              lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                                    ,(face-foreground 'font-lock-string-face)
                                    ,(face-foreground 'font-lock-constant-face)
                                    ,(face-foreground 'font-lock-variable-name-face)))
  :config
  (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

  ;; Reset `lsp-ui-doc-background' after loading theme
  (add-hook 'after-load-theme-hook
            (lambda ()
              (setq lsp-ui-doc-border (face-foreground 'default))
              (set-face-background 'lsp-ui-doc-background
                                   (face-background 'tooltip)))))

;; Ivy integration
(use-package lsp-ivy
  :after lsp-mode
  :bind (:map lsp-mode-map
          ([remap xref-find-apropos] . lsp-ivy-workspace-symbol)
          ("C-s-." . lsp-ivy-global-workspace-symbol)))

;; `treemacs' requires 25.2+, so `dap-mode' and `lsp-treemacs' also requires 25.2+
(when EMACS26+
  ;; Debug
  (use-package dap-mode
    :functions dap-hydra/nil
    :diminish
    :bind (:map lsp-mode-map
            ("<f5>" . dap-debug)
            ("M-<f5>" . dap-hydra))
    :hook ((after-init . dap-mode)
           (dap-mode . dap-ui-mode)
           (dap-session-created . (lambda (_args) (dap-hydra)))
           (dap-stopped . (lambda (_args) (dap-hydra)))
           (dap-terminated . (lambda (_args) (dap-hydra/nil)))

           (python-mode . (lambda () (require 'dap-python)))
           (ruby-mode . (lambda () (require 'dap-ruby)))
           (go-mode . (lambda () (require 'dap-go)))
           (java-mode . (lambda () (require 'dap-java)))
           ((c-mode c++-mode objc-mode swift-mode) . (lambda () (require 'dap-lldb)))
           (php-mode . (lambda () (require 'dap-php)))
           (elixir-mode . (lambda () (require 'dap-elixir)))
           ((js-mode js2-mode) . (lambda () (require 'dap-chrome)))
           (powershell-mode . (lambda () (require 'dap-pwsh)))))

  ;; `lsp-mode' and `treemacs' integration
  (use-package lsp-treemacs
    :after lsp-mode
    :bind (:map lsp-mode-map
            ("C-<f8>" . lsp-treemacs-errors-list)
            ("M-<f8>" . lsp-treemacs-symbols)
            ("s-<f8>" . lsp-treemacs-java-deps-list))
    :config
    (with-eval-after-load 'ace-window
      (when (boundp 'aw-ignored-buffers)
        (push 'lsp-treemacs-symbols-mode aw-ignored-buffers)
        (push 'lsp-treemacs-java-deps-mode aw-ignored-buffers)))

    (with-no-warnings
      (when (require 'all-the-icons nil t)
        (treemacs-create-theme "centaur-colors"
          :extends "doom-colors"
          :config
          (progn
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "repo" :height 1.0 :v-adjust -0.1 :face 'all-the-icons-blue))
             :extensions (root))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
             :extensions (boolean-data))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "settings_input_component" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-orange))
             :extensions (class))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "palette" :height 0.95 :v-adjust -0.15))
             :extensions (color-palette))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "square-o" :height 0.95 :v-adjust -0.15))
             :extensions (constant))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "file-text-o" :height 0.95 :v-adjust -0.05))
             :extensions (document))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "storage" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-orange))
             :extensions (enumerator))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "format_align_right" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
             :extensions (enumitem))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "bolt" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-orange))
             :extensions (event))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
             :extensions (field))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "search" :height 0.95 :v-adjust -0.05))
             :extensions (indexer))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "filter_center_focus" :height 0.95 :v-adjust -0.15))
             :extensions (intellisense-keyword))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "share" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
             :extensions (interface))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
             :extensions (localvariable))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-purple))
             :extensions (method))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "view_module" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
             :extensions (namespace))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "format_list_numbered" :height 0.95 :v-adjust -0.15))
             :extensions (numeric))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "control_point" :height 0.95 :v-adjust -0.2))
             :extensions (operator))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
             :extensions (property))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "format_align_center" :height 0.95 :v-adjust -0.15))
             :extensions (snippet))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "text-width" :height 0.9 :v-adjust -0.05))
             :extensions (string))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange))
             :extensions (structure))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "format_align_center" :height 0.95 :v-adjust -0.15))
             :extensions (template))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "chevron-right" :height 0.75 :v-adjust 0.1 :face 'font-lock-doc-face))
             :extensions (collapsed) :fallback "+")
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "chevron-down" :height 0.75 :v-adjust 0.1 :face 'font-lock-doc-face))
             :extensions (expanded) :fallback "-")
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-binary" :height 0.9  :v-adjust 0.0 :face 'font-lock-doc-face))
             :extensions (classfile))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-blue))
             :extensions (default-folder-opened))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-blue))
             :extensions (default-folder))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-green))
             :extensions (default-root-folder-opened))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-green))
             :extensions (default-root-folder))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-binary" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
             :extensions ("class"))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-zip" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
             :extensions (file-type-jar))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
             :extensions (folder-open))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
             :extensions (folder))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-orange))
             :extensions (folder-type-component-opened))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-orange))
             :extensions (folder-type-component))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-green))
             :extensions (folder-type-library-opened))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-green))
             :extensions (folder-type-library))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-pink))
             :extensions (folder-type-maven-opened))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-pink))
             :extensions (folder-type-maven))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'font-lock-type-face))
             :extensions (folder-type-package-opened))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'font-lock-type-face))
             :extensions (folder-type-package))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "plus" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
             :extensions (icon-create))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "list" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
             :extensions (icon-flat))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-material "share" :height 0.95 :v-adjust -0.2 :face 'all-the-icons-lblue))
             :extensions (icon-hierarchical))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "link" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
             :extensions (icon-link))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "refresh" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
             :extensions (icon-refresh))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "chain-broken" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
             :extensions (icon-unlink))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-alltheicon "java" :height 1.0 :v-adjust 0.0 :face 'all-the-icons-orange))
             :extensions (jar))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "book" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-green))
             :extensions (library))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "folder-open" :face 'all-the-icons-lblue))
             :extensions (packagefolder-open))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
             :extensions (packagefolder))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-faicon "archive" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
             :extensions (package))
            (treemacs-create-icon
             :icon (format "%s " (all-the-icons-octicon "repo" :height 1.0 :v-adjust -0.1 :face 'all-the-icons-blue))
             :extensions (java-project))))

        (setq lsp-treemacs-theme "centaur-colors")))))

;; Microsoft python-language-server support
(use-package lsp-python-ms
  :hook (python-mode . (lambda () (require 'lsp-python-ms)))
  :init
  (when (executable-find "python3")
    (setq lsp-python-ms-python-executable-cmd "python3")))

;; C/C++/Objective-C support
(use-package ccls
  :defines projectile-project-root-files-top-down-recurring
  :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls)))
  :config
  (with-eval-after-load 'projectile
    (setq projectile-project-root-files-top-down-recurring
          (append '("compile_commands.json" ".ccls")
                  projectile-project-root-files-top-down-recurring))))

;; Julia support
(use-package lsp-julia
  :hook (julia-mode . (lambda () (require 'lsp-julia))))

;; Java support
(use-package lsp-java
  :hook (java-mode . (lambda () (require 'lsp-java))))



;; Enable LSP in org babel
;; https://github.com/emacs-lsp/lsp-mode/issues/377
(cl-defmacro lsp-org-babel-enable (lang)
  "Support LANG in org source code block."
  (cl-check-type lang stringp)
  (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
         (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
    `(progn
       (defun ,intern-pre (info)
         (let ((file-name (->> info caddr (alist-get :file))))
           (unless file-name
             (user-error "LSP:: specify `:file' property to enable"))

           (setq buffer-file-name file-name)
           (and (fboundp 'lsp-deferred) (lsp-deferred))))
       (put ',intern-pre 'function-documentation
            (format "Enable `%s' in the buffer of org source block (%s)."
                    "LSP mode" (upcase ,lang)))

       (if (fboundp ',edit-pre)
           (advice-add ',edit-pre :after ',intern-pre)
         (progn
           (defun ,edit-pre (info)
             (,intern-pre info))
           (put ',edit-pre 'function-documentation
                (format "Prepare local buffer environment for org source block (%s)."
                        (upcase ,lang))))))))

(defvar org-babel-lang-list
  '("go" "python" "ipython" "ruby" "js" "css" "sass" "C" "rust" "java"))
(add-to-list 'org-babel-lang-list (if EMACS26+ "shell" "sh"))
(dolist (lang org-babel-lang-list)
  (eval `(lsp-org-babel-enable ,lang)))


(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
