;; init-edit.el --- Initialize edit configurations.	-*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defvar sea-large-file-size 1
  "Size (in MB) above which the user will be prompted to open the file literally
to avoid performance issues. Opening literally means that no major or minor
modes are active and the buffer is read-only.")

(defvar sea-large-file-modes-list
  '(archive-mode tar-mode jka-compr git-commit-mode image-mode
    doc-view-mode doc-view-mode-maybe ebrowse-tree-mode pdf-view-mode)
  "Major modes that `sea|check-large-file' will ignore.")

(setq-default
 vc-follow-symlinks t
 ;; Save clipboard contents into kill-ring before replacing them
 save-interprogram-paste-before-kill t
 ;; Bookmarks
 bookmark-default-file (concat sea-cache-dir "bookmarks")
 bookmark-save-flag t
 ;; Formatting
 delete-trailing-lines nil
 fill-column 80
 sentence-end-double-space nil
 word-wrap t
 ;; Scrolling
 hscroll-margin 1
 hscroll-step 1
 scroll-conservatively 1001
 scroll-margin 0
 scroll-preserve-screen-position t
 ;; Whitespace (see `editorconfig')
 indent-tabs-mode nil
 require-final-newline t
 tab-always-indent t
 tab-width 4
 tabify-regexp "^\t* [ \t]+" ; for :retab
 ;; Wrapping
 truncate-lines t
 truncate-partial-width-windows 50
 ;; whitespace-mode
 whitespace-line-column fill-column
 whitespace-style
 '(face indentation tabs tab-mark spaces space-mark newline newline-mark
   trailing lines-tail)
 whitespace-display-mappings
 '((tab-mark ?\t [?› ?\t])
   (newline-mark ?\n [?¬ ?\n])
   (space-mark ?\  [?·] [?.])))

(delete-selection-mode 1)
(setq-default major-mode 'text-mode)
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
(add-hook 'abbrev-mode-hook (lambda () (diminish 'abbrev-mode)))

;; ediff
(setq ediff-diff-options "-w"
      ediff-split-window-function #'split-window-horizontally
      ediff-window-setup-function #'ediff-setup-windows-plain)

(defun sea|dont-kill-scratch-buffer ()
  "Don't kill the scratch buffer."
  (or (not (string= (buffer-name) "*scratch*"))
      (ignore (bury-buffer))))
(add-hook 'kill-buffer-query-functions #'sea|dont-kill-scratch-buffer)

;; temporary windows often have q bound to `quit-window', which only buries the
;; contained buffer. I rarely don't want that buffer killed, so...
(defun sea*quit-window (orig-fn &optional kill window)
  (funcall orig-fn (not kill) window))
(advice-add #'quit-window :around #'sea*quit-window)

(defun sea|check-large-file ()
  "Check if the buffer's file is large (see `sea-large-file-size'). If so, ask
for confirmation to open it literally (read-only, disabled undo and in
fundamental-mode) for performance sake."
  (let* ((filename (buffer-file-name))
         (size (nth 7 (file-attributes filename))))
    (when (and (not (memq major-mode sea-large-file-modes-list))
               size (> size (* 1024 1024 sea-large-file-size))
               (y-or-n-p
                (format (concat "%s is a large file, open literally to "
                                "avoid performance issues?")
                        (file-relative-name filename))))
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode))))
(add-hook 'find-file-hook #'sea|check-large-file)

(push '("/LICENSE$" . text-mode) auto-mode-alist)

;; revert buffers for changed files
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

;; enabled by default in Emacs 25+. No thanks.
(electric-indent-mode -1)

;; savehist / saveplace
(setq savehist-file (concat sea-cache-dir "savehist")
      savehist-save-minibuffer-history t
      savehist-autosave-interval nil ; save on kill only
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
      save-place-file (concat sea-cache-dir "saveplace"))
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'save-place-mode)

 ;; Keep track of recently opened files
(use-package recentf
  :init
  (add-hook 'find-file-hook (lambda ()
                              (unless recentf-mode
                                (recentf-mode)
                                (recentf-track-opened-file))))
  :config
  (setq recentf-save-file (concat sea-cache-dir "recentf")
        recentf-max-menu-items 0
        recentf-max-saved-items 300
        recentf-filename-handlers '(file-truename)
        recentf-exclude
        (list "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
              "^/var/folders/.+$"
              ;; ignore private sea temp files (but not all of them)
              (concat "^" (file-truename sea-cache-dir)))))

(use-package savehist
  :ensure nil
  :init
  (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
        history-length 1000
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
        savehist-autosave-interval 60)
  (add-hook 'after-init-hook #'savehist-mode))

;; History
;; Emacsag 25 has a proper mode for `save-place'
(add-hook 'after-init-hook #'save-place-mode)

;; Show number of matches in mode-line while searching
(use-package visual-regexp-steroids
  :init
  (use-package visual-regexp)
  :bind (([remap query-replace-regexp] . vr/query-replace)))
(use-package anzu
  :diminish anzu-mode
  :bind (([remap query-replace] . anzu-query-replace)
         ;; ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :init (add-hook 'after-init-hook #'global-anzu-mode)
  :config (setq anzu-replace-to-string-separator
                (if (char-displayable-p ?→) " → " " -> ")))

;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :init
  ;; show org ediffs unfolded
  (with-eval-after-load 'outline
    (add-hook 'ediff-prepare-buffer-hook #'show-all))
  ;; restore window layout when done
  (with-eval-after-load 'winner
    (add-hook 'ediff-quit-hook #'winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

(use-package rg)

(use-package aggressive-indent
  :init
  (dolist (hook '(emacs-lisp-mode-hook css-mode-hook))
    (add-hook hook #'aggressive-indent-mode)))

(use-package ace-link
  :commands (ace-link-help ace-link-org))

(use-package avy
  :commands (avy-goto-char-2 avy-goto-line)
  :config
  (setq avy-all-windows nil
        avy-background t))

(use-package command-log-mode
  :commands (command-log-mode global-command-log-mode)
  :config
  (set-popup-rule! "*command-log*"
    :size 40
    :align 'right
    :noselect t)
  (setq command-log-mode-auto-show t
        command-log-mode-open-log-turns-on-mode t))

;; Increase selected region by semantic units
(use-package expand-region
  :commands (er/expand-region er/contract-region er/mark-symbol er/mark-word))

(use-package pcre2el
  :commands rxt-quote-pcre)

(use-package smart-forward
  :commands (smart-up smart-down smart-backward smart-forward))

;; Treat undo history as a tree
(use-package undo-tree
  :diminish undo-tree-mode
  :init (add-hook 'after-init-hook #'global-undo-tree-mode)
  :config
  (setq
   undo-tree-auto-save-history nil
   undo-tree-history-directory-alist `(("." . ,(concat sea-cache-dir "undo/")))))

;; Handles whitespace (tabs/spaces) settings externally. This way projects can
;; specify their own formatting rules.
(use-package editorconfig
  :config
  (add-hook 'after-init-hook #'editorconfig-mode)

  ;; editorconfig cannot procure the correct settings for extension-less files.
  ;; Executable scripts with a shebang line, for example. So why not use Emacs'
  ;; major mode to drop editorconfig a hint? This is accomplished by temporarily
  ;; appending an extension to `buffer-file-name' when we talk to editorconfig.
  (defvar sea-editorconfig-mode-alist
    '((sh-mode     . "sh")
      (python-mode . "py")
      (ruby-mode   . "rb")
      (perl-mode   . "pl")
      (php-mode    . "php"))
    "An alist mapping major modes to extensions. Used by
`sea*editorconfig-smart-detection' to give editorconfig filetype hints.")

  (defun sea*editorconfig-smart-detection (orig-fn &rest args)
    "Retrieve the properties for the current file. If it doesn't have an
extension, try to guess one."
    (let ((buffer-file-name
           (if (file-name-extension buffer-file-name)
               buffer-file-name
             (format "%s%s" buffer-file-name
                     (let ((ext (cdr (assq major-mode sea-editorconfig-mode-alist))))
                       (or (and ext (concat "." ext))
                           ""))))))
      (apply orig-fn args)))
  (advice-add #'editorconfig-call-editorconfig-exec :around #'sea*editorconfig-smart-detection)

  ;; Editorconfig makes indentation too rigid in Lisp modes, so tell
  ;; editorconfig to ignore indentation. I prefer dynamic indentation support
  ;; built into Emacs.
  (dolist (mode '(emacs-lisp-mode lisp-mode))
    (setq editorconfig-indentation-alist
          (assq-delete-all mode editorconfig-indentation-alist)))

  (defvar whitespace-style)
  (defun sea|editorconfig-whitespace-mode-maybe (&rest _)
    "Show whitespace-mode when file uses TABS (ew)."
    (when indent-tabs-mode
      (let ((whitespace-style '(face tabs tab-mark trailing-lines tail)))
        (whitespace-mode +1))))
  (add-hook 'editorconfig-custom-hooks #'sea|editorconfig-whitespace-mode-maybe))


(use-package smartparens
  :init
  (add-hook 'prog-mode-hook #'smartparens-mode)
  :config
  (require 'smartparens-config)
  ;; Smartparens is broken in `cc-mode' as of Emacs 27. See

  ;; <https://github.com/Fuco1/smartparens/issues/963>.
  (unless EMACS27+
    (pushnew! sp--special-self-insert-commands 'c-electric-paren 'c-electric-brace))
  ;; Smartparens' navigation feature is neat, but does not justify how
  ;; expensive it is. It's also less useful for evil users. This may need to
  ;; be reactivated for non-evil users though. Needs more testing!
  (add-hook! 'after-change-major-mode-hook
    (lambda ()
      (setq sp-navigate-skip-match nil
            sp-navigate-consider-sgml-tags nil)))

  ;; Autopair quotes more conservatively; if I'm next to a word/before another
  ;; quote, I likely don't want to open a new pair.
  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))

  ;; Expand {|} => { | }
  ;; Expand {|} => {
  ;;   |
  ;; }
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
             ;; I likely don't want a new pair if adjacent to a word or opening brace
             :unless '(sp-point-before-word-p sp-point-before-same-p)))

  ;; In lisps ( should open a new form if before another parenthesis
  (sp-local-pair sp-lisp-modes "(" ")" :unless '(:rem sp-point-before-same-p))

  ;; Major-mode specific fixes
  (sp-local-pair '(ruby-mode enh-ruby-mode) "{" "}"
                 :pre-handlers '(:rem sp-ruby-pre-handler)
                 :post-handlers '(:rem sp-ruby-post-handler))

  ;; Don't do square-bracket space-expansion where it doesn't make sense to
  (sp-local-pair '(emacs-lisp-mode org-mode markdown-mode gfm-mode)
                 "[" nil :post-handlers '(:rem ("| " "SPC")))

  ;; Reasonable default pairs for HTML-style comments
  (sp-local-pair (append sp--html-modes '(markdown-mode gfm-mode))
                 "<!--" "-->"
                 :unless '(sp-point-before-word-p sp-point-before-same-p)
                 :actions '(insert) :post-handlers '(("| " "SPC")))

  ;; Disable electric keys in C modes because it interferes with smartparens
  ;; and c'ustom bindings. We'll do it ourselves (mostly).
  (after! cc-mode
    (c-toggle-electric-state -1)
    (c-toggle-auto-newline -1)
    (setq c-electric-flag nil)
    (dolist (key '("#" "{" "}" "/" "*" ";" "," ":" "(" ")" "\177"))
      (define-key c-mode-base-map key nil))

    ;; Smartparens and cc-mode both try to autoclose angle-brackets
    ;; intelligently. The result isn't very intelligent (causes redundant
    ;; characters), so just do it ourselves.
    (define-key! c++-mode-map "<" nil ">" nil)

    (defun +default-cc-sp-point-is-template-p (id action context)
      "Return t if point is in the right place for C++ angle-brackets."
      (and (sp-in-code-p id action context)
           (cond ((eq action 'insert)
                  (sp-point-after-word-p id action context))
                 ((eq action 'autoskip)
                  (/= (char-before) 32)))))

    (defun +default-cc-sp-point-after-include-p (id action context)
      "Return t if point is in an #include."
      (and (sp-in-code-p id action context)
           (save-excursion
             (goto-char (line-beginning-position))
             (looking-at-p "[   ]*#include[^<]+"))))

    ;; ...and leave it to smartparens
    (sp-local-pair '(c++-mode objc-mode)
                   "<" ">"
                   :when '(+default-cc-sp-point-is-template-p
                           +default-cc-sp-point-after-include-p)
                   :post-handlers '(("| " "SPC")))

    (sp-local-pair '(c-mode c++-mode objc-mode java-mode)
                   "/*!" "*/"
                   :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC"))))

  ;; Expand C-style doc comment blocks. Must be done manually because some of
  ;; these languages use specialized (and deferred) parsers, whose state we
  ;; can't access while smartparens is doing its thing.
  (defun +default-expand-asterix-doc-comment-block (&rest _ignored)
    (let ((indent (current-indentation)))
      (newline-and-indent)
      (save-excursion
        (newline)
        (insert (make-string indent 32) " */")
        (delete-char 2))))
  (sp-local-pair
   '(js2-mode typescript-mode rjsx-mode rust-mode c-mode c++-mode objc-mode
              csharp-mode java-mode php-mode css-mode scss-mode less-css-mode
              stylus-mode scala-mode)
   "/*" "*/"
   :actions '(insert)
   :post-handlers '(("| " "SPC")
                    ("|\n[i]*/[d-2]" "RET")
                    (+default-expand-asterix-doc-comment-block "*")))

  (after! smartparens-ml
    (sp-with-modes '(tuareg-mode fsharp-mode)
      (sp-local-pair "(*" "*)" :actions nil)
      (sp-local-pair "(*" "*"
                     :actions '(insert)
                     :post-handlers '(("| " "SPC") ("|\n[i]*)[d-2]" "RET")))))

  (after! smartparens-markdown
    (sp-with-modes '(markdown-mode gfm-mode)
      (sp-local-pair "```" "```" :post-handlers '(:add ("||\n[i]" "RET")))

      ;; The original rules for smartparens had an odd quirk: inserting two
      ;; asterixex would replace nearby quotes with asterixes. These two rules
      ;; set out to fix this.
      (sp-local-pair "**" nil :actions :rem)
      (sp-local-pair "*" "*"
                     :actions '(insert skip)
                     :unless '(:rem sp-point-at-bol-p)
                     ;; * then SPC will delete the second asterix and assume
                     ;; you wanted a bullet point. * followed by another *
                     ;; will produce an extra, assuming you wanted **|**.
                     :post-handlers '(("[d1]" "SPC") ("|*" "*"))))

    ;; This keybind allows * to skip over **.
    (map! :map markdown-mode-map
          :ig "*" (λ! (if (looking-at-p "\\*\\* *$")
                          (forward-char 2)
                        (call-interactively 'self-insert-command)))))

  ;; Highjacks backspace to:
  ;;  a) balance spaces inside brackets/parentheses ( | ) -> (|)
  ;;  b) delete up to nearest column multiple of `tab-width' at a time
  ;;  c) close empty multiline brace blocks in one step:
  ;;     {
  ;;     |
  ;;     }
  ;;     becomes {|}
  ;;  d) refresh smartparens' :post-handlers, so SPC and RET expansions work
  ;;     even after a backspace.
  ;;  e) properly delete smartparen pairs when they are encountered, without
  ;;     the need for strict mode.
  ;;  f) do none of this when inside a string
  (advice-add #'delete-backward-char :override #'+default*delete-backward-char)
  ;; Makes `newline-and-indent' continue comments (and more reliably)
  (advice-add #'newline-and-indent :override #'+default*newline-indent-and-continue-comments)
  )

(provide 'init-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
