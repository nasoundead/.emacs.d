;;; init-org.el
;;; Code:
(require 'init-funcs)

(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib
  :pretty-hydra
  ((:title (pretty-hydra-title "Org Template" 'fileicon "org" :face 'all-the-icons-green :height 1.1 :v-adjust 0.0)
           :color blue :quit-key "q")
   ("Basic"
    (("a" (hot-expand "<a") "ascii")
     ("c" (hot-expand "<c") "center")
     ("C" (hot-expand "<C") "comment")
     ("e" (hot-expand "<e") "example")
     ("E" (hot-expand "<E") "export")
     ("h" (hot-expand "<h") "html")
     ("l" (hot-expand "<l") "latex")
     ("n" (hot-expand "<n") "note")
     ("o" (hot-expand "<q") "quote")
     ("v" (hot-expand "<v") "verse"))
    "Head"
    (("i" (hot-expand "<i") "index")
     ("A" (hot-expand "<A") "ASCII")
     ("I" (hot-expand "<I") "INCLUDE")
     ("H" (hot-expand "<H") "HTML")
     ("L" (hot-expand "<L") "LaTeX"))
    "Source"
    (("s" (hot-expand "<s") "src")
     ("m" (hot-expand "<s" "emacs-lisp") "emacs-lisp")
     ("y" (hot-expand "<s" "python :results output") "python")
     ("S" (hot-expand "<s" "sh") "sh")
     ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)") "golang"))
    "Misc"
    (("u" (hot-expand "<s" "plantuml :file CHANGE.png") "plantuml")
     ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0") "ipython")
     ("<" self-insert-command "ins"))))
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ("C-c x" . org-capture)
         :map org-mode-map
         ("<" . (lambda ()
                  "Insert org template."
                  (interactive)
                  (if (or (region-active-p) (looking-back "^\s*" 1))
                      (org-hydra/body)
                    (self-insert-command 1)))))
  :hook (((org-babel-after-execute org-mode) . org-redisplay-inline-images) ; display image
         (org-mode . (lambda ()
                       "Beautify org symbols."
                       (when centaur-prettify-org-symbols-alist
                         (if prettify-symbols-alist
                             (push centaur-prettify-org-symbols-alist prettify-symbols-alist)
                           (setq prettify-symbols-alist centaur-prettify-org-symbols-alist)))
                       (prettify-symbols-mode 1)))
         (org-indent-mode . (lambda()
                              (diminish 'org-indent-mode)
                              ;; HACK: Prevent text moving around while using brackets
                              ;; @see https://github.com/seagle0128/.emacs.d/issues/88
                              (make-variable-buffer-local 'show-paren-mode)
                              (setq show-paren-mode nil))))
  :config
  ;; For hydra
  (defun hot-expand (str &optional mod)
    "Expand org template.

STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
      (insert str)
      (if (fboundp 'org-try-structure-completion)
          (org-try-structure-completion) ; < org 9
        (progn
          ;; New template expansion since org 9
          (require 'org-tempo nil t)
          (org-tempo-complete-tag)))
      (when mod (insert mod) (forward-line))
      (when text (insert text))))

  ;; active Org-babel languages
  ;; ------------------------------------------------------------------------
  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defconst load-language-alist
    '((emacs-lisp . t)
      (perl       . t)
      (python     . t)
      (ruby       . t)
      (js         . t)
      (css        . t)
      (sass       . t)
      (C          . t)
      (java       . t)
      (plantuml   . t))
    "Alist of org ob languages.")

  ;; ob-sh renamed to ob-shell since 26.1.
  (cl-pushnew '(shell . t) load-language-alist)

  (use-package ob-go
    :init (cl-pushnew '(go . t) load-language-alist))

  (use-package ob-ipython
    :init
    (cl-pushnew '(ipython . t) load-language-alist)
    (with-eval-after-load 'company
      (make-local-variable 'company-backend)
      (cl-pushnew 'company-ob-ipython company-backends)))

  (use-package ob-rust
    :init (cl-pushnew '(rust . t) load-language-alist))

  ;; Install: npm install -g @mermaid-js/mermaid-cli
  (use-package ob-mermaid
    :init (cl-pushnew '(mermaid . t) load-language-alist))

  (use-package plantuml-mode
    :init
    ;; Enable plantuml-mode for PlantUML files
    (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
    ;; Integration with org-mode
    (cl-pushnew '(plantuml . t) load-language-alist)
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))
  (defvar plantuml-jar-path (expand-file-name "plantuml.jar" sea-etc-dir)
    "plantuml dir")
  (defun sea/plantuml-install()
    (let ((url "http://jaist.dl.sourceforge.net/project/plantuml/plantuml.jar"))
      (unless (file-exists-p plantuml-jar-path)
        (url-copy-file url plantuml-jar-path))))
  (add-hook 'org-mode-hook #'(lambda () (eval-after-load 'ob-plantuml (sea/plantuml-install))))

  (org-babel-do-load-languages 'org-babel-load-languages load-language-alist)

  ;; Rich text clipboard
  (use-package org-rich-yank
    :bind (:map org-mode-map
            ("C-M-y" . org-rich-yank)))

  ;; Table of contents
  (use-package toc-org
    :hook (org-mode . toc-org-mode))

  ;; Auto-toggle Org LaTeX fragments
  (use-package org-fragtog
    :diminish
    :hook (org-mode . org-fragtog-mode))

  ;; Preview
  (use-package org-preview-html
    :diminish
    :bind (:map org-mode-map
            ("C-c C-h" . org-preview-html-mode))
    :init (when (featurep 'xwidget-internal)
            (setq org-preview-html-viewer 'xwidget)))

  ;; Presentation
  (use-package org-tree-slide
    :diminish
    :functions (org-display-inline-images
                org-remove-inline-images)
    :bind (:map org-mode-map
            ("s-<f7>" . org-tree-slide-mode)
            :map org-tree-slide-mode-map
            ("<left>" . org-tree-slide-move-previous-tree)
            ("<right>" . org-tree-slide-move-next-tree)
            ("S-SPC" . org-tree-slide-move-previous-tree)
            ("SPC" . org-tree-slide-move-next-tree))
    :hook ((org-tree-slide-play . (lambda ()
                                    (text-scale-increase 4)
                                    (org-display-inline-images)
                                    (read-only-mode 1)))
           (org-tree-slide-stop . (lambda ()
                                    (text-scale-increase 0)
                                    (org-remove-inline-images)
                                    (read-only-mode -1))))
    :init (setq org-tree-slide-header nil
                org-tree-slide-slide-in-effect t
                org-tree-slide-heading-emphasis nil
                org-tree-slide-cursor-init t
                org-tree-slide-modeline-display 'outside
                org-tree-slide-skip-done nil
                org-tree-slide-skip-comments t
                org-tree-slide-skip-outline-level 3))


  ;; To speed up startup, don't put to init section
  (setq
   org-modules nil                 ; Faster loading
   org-directory centaur-org-directory
   org-capture-templates
   `(("i" "Idea" entry (file ,(concat org-directory "/idea.org"))
      "*  %^{Title} %?\n%U\n%a\n")
     ("t" "Todo" entry (file ,(concat org-directory "/gtd.org"))
      "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
     ("n" "Note" entry (file ,(concat org-directory "/note.org"))
      "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
     ("j" "Journal" entry (file+olp+datetree
                           ,(concat org-directory "/journal.org"))
      "*  %^{Title} %?\n%U\n%a\n" :clock-in t :clock-resume t)
     ("b" "Book" entry (file+olp+datetree
                        ,(concat org-directory "/book.org"))
      "* Topic: %^{Description}  %^g %? Added: %U"))

   org-todo-keywords
   '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
     (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)"))
   org-todo-keyword-faces '(("HANGUP" . warning)
                            ("❓" . warning))
   org-priority-faces '((?A . error)
                        (?B . warning)
                        (?C . success))

   ;; ;; Agenda styling
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────"

   org-tags-column -80
   org-log-done 'time
   org-catch-invisible-edits 'smart
   org-startup-indented t
   org-ellipsis (if (char-displayable-p ?⏷) "\t⏷" nil)
   org-pretty-entities nil
   org-src-fontify-natively t
   org-eldoc-breadcrumb-separator " → "
   org-confirm-babel-evaluate nil
   org-hide-emphasis-markers t
   )

  ;; Add new template
  (add-to-list 'org-structure-template-alist '("n" . "note"))

  ;; Use embedded webkit browser if possible
  (when (featurep 'xwidget-internal)
    (push '("\\.\\(x?html?\\|pdf\\)\\'"
            .
            (lambda (file _link)
              (centaur-webkit-browse-url (concat "file://" file) t)))
          org-file-apps))

  ;; (defun display-inline-images ()
  ;;   (condition-case nil
  ;;       (org-display-inline-images)
  ;;     (error nil)))
  ;; (add-hook 'org-babel-after-execute-hook #'display-inline-images 'append)
  ;; (add-hook 'org-mode-hook #'(lambda ()(setq truncate-lines t)) 'append)
  )

;; Prettify UI
(if EMACS27+
    (use-package org-modern
      :hook ((org-mode . org-modern-mode)
             (org-agenda-finalize . org-modern-agenda)
             (org-modern-mode . (lambda ()
                                  "Adapt `org-modern-mode'."
                                  ;; Disable Prettify Symbols mode
                                  (setq prettify-symbols-alist nil)
                                  (prettify-symbols-mode -1)))
             ))
  (progn
    (use-package org-superstar
      :if (and (display-graphic-p) (char-displayable-p ?◉))
      :hook (org-mode . org-superstar-mode)
      :init (setq org-superstar-headline-bullets-list '("◉""○""◈""◇""⁕")))
    (use-package org-fancy-priorities
      :diminish
      :hook (org-mode . org-fancy-priorities-mode)
      :init (setq org-fancy-priorities-list
                  (if (and (display-graphic-p) (char-displayable-p ?🅐))
                      '("🅐" "🅑" "🅒" "🅓")
                    '("HIGH" "MEDIUM" "LOW" "OPTIONAL"))))))
;; ui enhance
;; ------------------------------------------------------------------------
;; (defun enhance-ui-for-orgmode ()
;;   "Enhance UI for orgmode."
;;   (toggle-truncate-lines)
;;   ;; Beautify Org Checkbox Symbol
;;   (push '("[ ]" . "☐") prettify-symbols-alist)
;;   (push '("[X]" . "☑" ) prettify-symbols-alist)
;;   (push '("[-]" . "❍" ) prettify-symbols-alist)
;;   (push '("#+BEGIN_SRC" . "⌜" ) prettify-symbols-alist)
;;   (push '("#+begin_src" . "⌜" ) prettify-symbols-alist)
;;   (push '("#+END_SRC" . "⌞" ) prettify-symbols-alist)
;;   (push '("#+end_src" . "⌞" ) prettify-symbols-alist)
;;   (push '("TODO" . "☐" ) prettify-symbols-alist)
;;   (push '("WORK" . "⚑" ) prettify-symbols-alist)
;;   (push '("DONE" . "☑" ) prettify-symbols-alist)
;;   (prettify-symbols-mode))
;; (add-hook 'org-mode-hook 'enhance-ui-for-orgmode)
;; (defface org-checkbox-done-text
;;   '((t (:foreground "#71696A" :strike-through t)))
;;   "Face for the text part of a checked org-mode checkbox.")



(use-package deft)

(provide 'init-org)
