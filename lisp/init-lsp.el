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


(use-package lsp-mode
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
          ([remap xref-find-references] . lsp-find-references))

  :init
  ;; @see https://github.com/emacs-lsp/lsp-mode#performance
  (setq read-process-output-max (* 3 1024 1024)
        lsp-semantic-tokens-enable t
        )
  )

(use-package lsp-ui
  :custom-face
  (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :pretty-hydra
  ((:title (pretty-hydra-title "LSP UI" 'faicon "rocket" :face 'all-the-icons-green)
           :color amaranth :quit-key "q")
   ("Doc"
    (("d e" (progn
              (lsp-ui-doc-enable (not lsp-ui-doc-mode))
              (setq lsp-ui-doc-enable (not lsp-ui-doc-enable)))
      "enable" :toggle lsp-ui-doc-mode)
     ("d s" (setq lsp-ui-doc-include-signature (not lsp-ui-doc-include-signature))
      "signature" :toggle lsp-ui-doc-include-signature)
     ("d t" (setq lsp-ui-doc-position 'top)
      "top" :toggle (eq lsp-ui-doc-position 'top))
     ("d b" (setq lsp-ui-doc-position 'bottom)
      "bottom" :toggle (eq lsp-ui-doc-position 'bottom))
     ("d p" (setq lsp-ui-doc-position 'at-point)
      "at point" :toggle (eq lsp-ui-doc-position 'at-point))
     ("d h" (setq lsp-ui-doc-header (not lsp-ui-doc-header))
      "header" :toggle lsp-ui-doc-header)
     ("d f" (setq lsp-ui-doc-alignment 'frame)
      "align frame" :toggle (eq lsp-ui-doc-alignment 'frame))
     ("d w" (setq lsp-ui-doc-alignment 'window)
      "align window" :toggle (eq lsp-ui-doc-alignment 'window)))
    "Sideline"
    (("s e" (progn
              (lsp-ui-sideline-enable (not lsp-ui-sideline-mode))
              (setq lsp-ui-sideline-enable (not lsp-ui-sideline-enable)))
      "enable" :toggle lsp-ui-sideline-mode)
     ("s h" (setq lsp-ui-sideline-show-hover (not lsp-ui-sideline-show-hover))
      "hover" :toggle lsp-ui-sideline-show-hover)
     ("s d" (setq lsp-ui-sideline-show-diagnostics (not lsp-ui-sideline-show-diagnostics))
      "diagnostics" :toggle lsp-ui-sideline-show-diagnostics)
     ("s s" (setq lsp-ui-sideline-show-symbol (not lsp-ui-sideline-show-symbol))
      "symbol" :toggle lsp-ui-sideline-show-symbol)
     ("s c" (setq lsp-ui-sideline-show-code-actions (not lsp-ui-sideline-show-code-actions))
      "code actions" :toggle lsp-ui-sideline-show-code-actions)
     ("s i" (setq lsp-ui-sideline-ignore-duplicate (not lsp-ui-sideline-ignore-duplicate))
      "ignore duplicate" :toggle lsp-ui-sideline-ignore-duplicate))
    "Action"
    (("h" backward-char "←")
     ("j" next-line "↓")
     ("k" previous-line "↑")
     ("l" forward-char "→")
     ("C-a" mwim-beginning-of-code-or-line nil)
     ("C-e" mwim-end-of-code-or-line nil)
     ("C-b" backward-char nil)
     ("C-n" next-line nil)
     ("C-p" previous-line nil)
     ("C-f" forward-char nil)
     ("M-b" backward-word nil)
     ("M-f" forward-word nil)
     ("c" lsp-ui-sideline-apply-code-actions "apply code actions"))))
  :bind (("C-c u" . lsp-ui-imenu)
         :map lsp-ui-mode-map
         ("M-<f6>" . lsp-ui-hydra/body)
         ("s-<return>" . lsp-ui-sideline-apply-code-actions))
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-delay 0.1
        lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                              ,(face-foreground 'font-lock-string-face)
                              ,(face-foreground 'font-lock-constant-face)
                              ,(face-foreground 'font-lock-variable-name-face)))
  ;; Set correct color to borders
  (defun my-lsp-ui-doc-set-border ()
    "Set the border color of lsp doc."
    (setq lsp-ui-doc-border
          (if (facep 'posframe-border)
              (face-background 'posframe-border nil t)
            (face-foreground 'shadow nil t))))
  (my-lsp-ui-doc-set-border)
  (add-hook 'after-load-theme-hook #'my-lsp-ui-doc-set-border t)
  :config
  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)
  )


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
            )
    ))

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


;; Python
(use-package lsp-pyright
  :preface
  ;; Use yapf to format
  (defun lsp-pyright-format-buffer ()
    (interactive)
    (when (and (executable-find "yapf") buffer-file-name)
      (call-process "yapf" nil nil nil "-i" buffer-file-name)))
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (add-hook 'after-save-hook #'lsp-pyright-format-buffer t t)))
  :init (when (executable-find "python3")
          (setq lsp-pyright-python-executable-cmd "python3")))

;; C/C++/Objective-C
(use-package ccls
  :defines projectile-project-root-files-top-down-recurring
  :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls)))
  :config
  (with-eval-after-load 'projectile
    (setq projectile-project-root-files-top-down-recurring
          (append '("compile_commands.json" ".ccls")
                  projectile-project-root-files-top-down-recurring)))
  (with-no-warnings
    ;; FIXME: fail to call ccls.xref
    ;; @see https://github.com/emacs-lsp/emacs-ccls/issues/109
    (cl-defmethod my-lsp-execute-command
      ((_server (eql ccls)) (command (eql ccls.xref)) arguments)
      (when-let ((xrefs (lsp--locations-to-xref-items
                         (lsp--send-execute-command (symbol-name command) arguments))))
        (xref--show-xrefs xrefs nil)))
    (advice-add #'lsp-execute-command :override #'my-lsp-execute-command)))

(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
