;;; init-org.el
;;; Code:
(require 'init-funcs)

(use-package org
  ;; :mode (("\\.org$" . org-mode))
  ;; :ensure org-plus-contrib
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
    (("u" (hot-expand "<s" "plantuml :cmdline -charset utf-8 :file /images/CHANGE.png") "plantuml")
     ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer") "ipython")
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
  :hook ((org-babel-after-execute org-mode) . org-redisplay-inline-images)
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
    ;; (with-eval-after-load 'company
    ;;   (make-local-variable 'company-backend)
    ;;   (cl-pushnew 'company-ob-ipython company-backends))
    )

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
  (setq org-plantuml-jar-path (expand-file-name "plantuml.jar" sea-etc-dir))
  (defun sea/plantuml-install()
    (let ((url "http://jaist.dl.sourceforge.net/project/plantuml/plantuml.jar"))
      (unless (file-exists-p org-plantuml-jar-path)
        (url-copy-file url org-plantuml-jar-path))))
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
            )
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

  (use-package org-superstar
    :hook (org-mode . org-superstar-mode)
    :init
    (setq org-superstar-headline-bullets-list '("◉""○""◈""◇""⁕")))

  (use-package org-fancy-priorities
    :hook (org-mode . org-fancy-priorities-mode)
    :init (setq org-fancy-priorities-list
                (if (and (display-graphic-p) (char-displayable-p ?🅐))
                    '("🅐" "🅑" "🅒" "🅓")
                  '("high" "medium" "low" "optional"))))
  ;; ui enhance
  (defun enhance-ui-for-orgmode ()
    "enhance ui for orgmode."
    (when centaur-prettify-org-symbols-alist
      (if prettify-symbols-alist
          (push centaur-prettify-org-symbols-alist prettify-symbols-alist)
        (setq prettify-symbols-alist centaur-prettify-org-symbols-alist)))
    (prettify-symbols-mode)
    (toggle-truncate-lines))
  (add-hook 'org-mode-hook #'enhance-ui-for-orgmode)

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

   org-M-RET-may-split-line '((:headline . nil))
   ;; Agenda styling
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
  )


;; snipshort
(defvar clipjar-location (concat sea-bin-dir "Clip.jar"))
(defun org-paste-image ()
  (interactive)
  ;; create images dir
  (setq target-dir (concat (file-name-directory (buffer-file-name)) "images/"))
  (unless (file-exists-p target-dir)
    (make-directory target-dir))
  (setq filename-without-extension
        (make-temp-name
         (concat
          (file-name-nondirectory
           (buffer-file-name))
          (format-time-string "_%Y%m%d_%H%M%S")))  )
  (insert
   (org-make-link-string
    (concat "file:"
            (string-trim
             (shell-command-to-string
              (mapconcat #'identity
                         `("java"
                           "-jar"
                           ,(expand-file-name clipjar-location)
                           "--name"
                           ,(concat filename-without-extension)
                           ,(concat target-dir)
                           )
                         " "
                         ))))))
  (org-display-inline-images))
(defun org-paste-image-ask-dir ()
  (interactive)
  (let* ((dir (read-directory-name "Dir: ")))
    (insert
     (org-make-link-string
      (concat "file:"
              (shell-command-to-string
               (mapconcat #'identity
                          `("java"
                            "-jar"
                            ,(expand-file-name clipjar-location)
                            "--uuid"
                            ,(file-relative-name dir default-directory)
                            )
                          " "
                          )))))))
(defun org-paste-image-ask-name ()
  (interactive)
  (let* ((image-name (string-trim (read-string "Image name: "))))
    (insert
     (org-make-link-string
      (concat "file:"
              (shell-command-to-string
               (mapconcat #'identity
                          `("java"
                            "-jar"
                            ,(expand-file-name clipjar-location)
                            "--name"
                            ,(concat "'" image-name "'")        ;; image name without extension must be quoted
                            "'/images'"               ;; Directory which the image will be saved '/tmp/images scala'
                            )
                          " "
                          )))))))
(defun org-screenshot-and-paste-image-use-powershell()
  "Take a screenshot into a time stamped unique-named file in the
        same directory as the org-buffer and insert a link to this file."
  (interactive)
  ;; create images dir
  (setq target-dir (concat (file-name-directory (buffer-file-name)) "images/"))
  (unless (file-exists-p target-dir)
    (make-directory target-dir))
  (setq filename
        (concat
         (make-temp-name
          (concat
           target-dir
           (file-name-nondirectory
            (buffer-file-name))
           (format-time-string "_%Y%m%d_%H%M%S_")) ) ".png"))
  (shell-command "snippingtool /clip")
  (sleep-for 0.3)
  (shell-command (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms;if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {$image = [System.Windows.Forms.Clipboard]::GetImage();[System.Drawing.Bitmap]$image.Save('" filename "',[System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'clipboard content saved as file'} else {Write-Output 'clipboard does not contain image data'}\""))
  (insert (concat "[[file:" filename "]]"))
  (org-display-inline-images))
;; Use embedded webkit browser if possible
(when (featurep 'xwidget-internal)
  (push '("\\.\\(x?html?\\|pdf\\)\\'"
          .
          (lambda (file _link)
            (centaur-webkit-browse-url (concat "file://" file) t)))
        org-file-apps))

(use-package deft)

(provide 'init-org)
