;;; init-org.el
;;; Code:

(use-package org
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda))

(use-package ob-ipython
  :init
  (with-eval-after-load 'company
    (make-local-variable 'company-backend)
    (cl-pushnew 'company-ob-ipython company-backends)))

;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (plantuml . t)
   (emacs-lisp . t)
   (ipython . t)
   (org . t)
   (latex . t)))
(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)

(add-hook 'org-babel-after-execute-hook #'display-inline-images 'append)
(add-hook 'org-mode-hook #'(lambda ()(setq truncate-lines t)) 'append)
(defun display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

(defvar plantuml-jar-path (expand-file-name "plantuml.jar" sea-etc-dir)
  "plantuml dir")
(defun sea/plantuml-install()
  (let ((url "http://jaist.dl.sourceforge.net/project/plantuml/plantuml.jar"))
    (unless (file-exists-p plantuml-jar-path)
      (url-copy-file url plantuml-jar-path))))
(add-hook 'org-mode-hook #'(lambda () (eval-after-load 'ob-plantuml (sea/plantuml-install))))
(use-package plantuml-mode
  :init
  ;; Enable plantuml-mode for PlantUML files
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  ;; Integration with org-mode
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

(use-package org-projectile
  :config
  (org-projectile:per-repo)
  (setq org-projectile:per-repo-filename "todo.org"
        org-agenda-files (append org-agenda-files (org-projectile:todo-files))))

(use-package org-bullets
  :init
  (setq org-bullets-bullet-list '( "⦿" "○"  "✿" "◆"))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(defun enhance-ui-for-orgmode ()
  "Enhance UI for orgmode."
  (toggle-truncate-lines)
  ;; Beautify Org Checkbox Symbol
  (push '("[ ]" . "☐") prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist)
  (push '("#+BEGIN_SRC" . "⌜" ) prettify-symbols-alist)
  (push '("#+END_SRC" . "⌞" ) prettify-symbols-alist)
  (push '("TODO" . "☐" ) prettify-symbols-alist)
  (push '("WORK" . "⚑" ) prettify-symbols-alist)
  (push '("DONE" . "☑" ) prettify-symbols-alist)
  (prettify-symbols-mode)
  )
(add-hook 'org-mode-hook 'enhance-ui-for-orgmode)

(defface org-checkbox-done-text
  '((t (:foreground "#71696A" :strike-through t)))
  "Face for the text part of a checked org-mode checkbox.")

(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
    1 'org-checkbox-done-text prepend))
 'append)


(setq-default
 org-eldoc-breadcrumb-separator " → "
 org-enforce-todo-dependencies t
 org-entities-user
 '(("flat"  "\\flat" nil "" "" "266D" "♭")
   ("sharp" "\\sharp" nil "" "" "266F" "♯"))
 org-fontify-done-headline t
 org-fontify-quote-and-verse-blocks t
 org-fontify-whole-heading-line t
 org-footnote-auto-label 'plain
 org-hide-leading-stars t
 org-hide-leading-stars-before-indent-mode t
 org-image-actual-width nil
 org-list-description-max-indent 4
 org-priority-faces
 '((?a . error)
   (?b . warning)
   (?c . success))
 org-refile-targets
 '((nil :maxlevel . 3)
   (org-agenda-files :maxlevel . 3))
 org-startup-indented t
 org-todo-keywords
 '((sequence "TODO(t)" "PROJ(p)" "|" "DONE(d)")
   (sequence "[ ](T)" "[-](P)" "[?](M)" "|" "[X](D)")
   (sequence "NEXT(n)" "WAIT(w)" "HOLD(h)" "|" "ABRT(c)"))
 org-todo-keyword-faces
 '(("[-]" :inherit (font-lock-constant-face bold))
   ("[?]" :inherit (warning bold))
   ("PROJ" :inherit (bold default))
   ("HOLD" :inherit (warning bold))
   ("ABRT" :inherit (error bold)))
 org-use-sub-superscripts '{}

 ;; Scale up LaTeX previews a bit (default is too small)
 org-format-latex-options (plist-put org-format-latex-options :scale 1.5))


(defvar org-structure-template-alist)

(defun org+-avoid-old-structure-templates (fun &rest args)
  "Call FUN with ARGS with modified `org-structure-template-alist'.
Use a copy of `org-structure-template-alist' with all
old structure templates removed."
  (let ((org-structure-template-alist
         (cl-remove-if
          (lambda (template)
            (null (stringp (cdr template))))
          org-structure-template-alist)))
    (apply fun args)))

(eval-after-load "org"
  '(when (version<= "9.2" (org-version))
     (defun org-try-structure-completion ()
       "Try to complete a structure template before point.
This looks for strings like \"<e\" on an otherwise empty line and
expands them."
       (let ((l (buffer-substring (point-at-bol) (point)))
             a)
         (when (and (looking-at "[ \t]*$")
                    (string-match "^[ \t]*<\\([a-zA-Z]+\\)$" l)
                    (setq a (assoc (match-string 1 l) org-structure-template-alist))
                    (null (stringp (cdr a))))
           (org-complete-expand-structure-template (+ -1 (point-at-bol)
                                                      (match-beginning 1)) a)
           t)))

     (defun org-complete-expand-structure-template (start cell)
       "Expand a structure template."
       (let ((rpl (nth 1 cell))
             (ind ""))
         (delete-region start (point))
         (when (string-match "\\`[ \t]*#\\+" rpl)
           (cond
            ((bolp))
            ((not (string-match "\\S-" (buffer-substring (point-at-bol) (point))))
             (setq ind (buffer-substring (point-at-bol) (point))))
            (t (newline))))
         (setq start (point))
         (when (string-match "%file" rpl)
           (setq rpl (replace-match
                      (concat
                       "\""
                       (save-match-data
                         (abbreviate-file-name (read-file-name "Include file: ")))
                       "\"")
                      t t rpl)))
         (setq rpl (mapconcat 'identity (split-string rpl "\n")
                              (concat "\n" ind)))
         (insert rpl)
         (when (re-search-backward "\\?" start t) (delete-char 1))))

     (advice-add 'org-tempo-add-templates :around #'org+-avoid-old-structure-templates)

     (add-hook 'org-tab-after-check-for-cycling-hook #'org-try-structure-completion)

     (require 'org-tempo)))



;; Block Template
(use-package hydra :ensure t
  :config
  ;; Define the templates
  (setq org-structure-template-alist
        '(("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
          ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
          ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
          ("v" "#+begin_verse\n?\n#+end_verse" "<verse>\n?\n/verse>")
          ("c" "#+begin_center\n?\n#+end_center" "<center>\n?\n/center>")
          ("l" "#+begin_export latex\n?\n#+end_export" "<literal style=\"latex\">\n?\n</literal>")
          ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
          ("h" "#+begin_export html\n?\n#+end_exrt" "<literal style=\"html\">\n?\n</literal>")
          ("H" "#+html: " "<literal style=\"html\">?</literal>")
          ("a" "#+begin_export ascii\n?\n#+end_export")
          ("A" "#+ascii: ")
          ("i" "#+index: ?" "#+index: ?")
          ("I" "#+include: %file ?" "<include file=%file markup=\"?\">")))

  ;; Shortcuts
  (defun hot-expand (str &optional mod)
    "Expand org template."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
      (insert str)
      (org-try-structure-completion)
      (when mod (insert mod) (forward-line))
      (when text (insert text))))

  (defhydra hydra-org-template (:color blue :hint nil)
    "
     Org template

 block               src block         structure
--------------------------------------------------------------------------------------
_c_: center        _s_: src         _L_: LATEX:
_q_: quote         _e_: emacs lisp  _i_: index:
_E_: example       _p_: python      _I_: INCLUDE:
_v_: verse         _u_: Plantuml    _H_: HTML:
_a_: ascii         _h_: html        _A_: ASCII:
_l_: latex
"
    ("s" (hot-expand "<s"))
    ("E" (hot-expand "<e"))
    ("q" (hot-expand "<q"))
    ("v" (hot-expand "<v"))
    ("c" (hot-expand "<c"))
    ("l" (hot-expand "<l"))
    ("h" (hot-expand "<h"))
    ("a" (hot-expand "<a"))
    ("L" (hot-expand "<L"))
    ("i" (hot-expand "<i"))
    ("e" (hot-expand "<s" "emacs-lisp"))
    ("p" (hot-expand "<s" "ipython :session :exports both :results raw drawer"))
    ("S" (hot-expand "<s" "sh"))
    ("u" (hot-expand "<s" "plantuml :file CHANGE.svg :cache yes :cmdline -charset utf-8"))
    ("I" (hot-expand "<I"))
    ("H" (hot-expand "<H"))
    ("A" (hot-expand "<A"))
    ("<" self-insert-command "ins")
    ("ESC" nil "quit"))

  (define-key org-mode-map "<"
    (lambda () (interactive)
      (if (or (region-active-p) (looking-back "^"))
          (hydra-org-template/body)
        (self-insert-command 1))))
  )

(use-package deft)

(provide 'init-org)
