;;; init-ivy.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(defvar +ivy-project-search-engines '(rg))
(defmacro +ivy-do-action! (action)
  "Returns an interactive lambda that sets the current ivy action and
immediately runs it on the current candidate (ending the ivy session)."
  `(lambda ()
     (interactive)
     (ivy-set-action ,action)
     (setq ivy-exit 'done)
     (exit-minibuffer)))

(use-package ivy
  :defer 1
  :config
  (setq ivy-height 15
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        projectile-completion-system 'ivy
        smex-completion-method 'ivy
        ;; Don't use ^ as initial input
        ivy-initial-inputs-alist nil
        ;; highlight til EOL
        ivy-format-function #'ivy-format-function-line
        ;; disable magic slash on non-match
        ivy-magic-slash-non-match-action nil
        ;; don't show recent files in switch-buffer
        ivy-use-virtual-buffers nil
        ;; ...but if that ever changes, show their full path
        ivy-virtual-abbreviate 'full
        ;; don't quit minibuffer on delete-error
        ivy-on-del-error-function nil
        ;; enable ability to select prompt (alternative to `ivy-immediate-done')
        ivy-use-selectable-prompt t)

  (after! yasnippet
    (add-to-list 'yas-prompt-functions #'+ivy-yas-prompt nil #'eq))

  (define-key! 'global
    [remap switch-to-buffer]       #'ivy-switch-buffer
    [remap persp-switch-to-buffer] #'+ivy/switch-workspace-buffer
    [remap imenu-anywhere]         #'ivy-imenu-anywhere)

  (ivy-mode +1)

  ;; Show more buffer information in switch-buffer commands
  (after! ivy-rich
    (dolist (cmd '(ivy-switch-buffer +ivy/switch-workspace-buffer
                                     counsel-projectile-switch-to-buffer))
      (ivy-set-display-transformer cmd '+ivy-buffer-transformer)))

  (use-package ivy-hydra
    :commands (ivy-dispatching-done-hydra ivy--matcher-desc)
    :init
    (define-key! ivy-minibuffer-map
      "\C-o"      #'+ivy-coo-hydra/body
      (kbd "M-o") #'ivy-dispatching-done-hydra)))


(use-package counsel
  :commands counsel-describe-face
  :init
  (define-key! 'global
    [remap apropos]                  #'counsel-apropos
    [remap bookmark-jump]            #'counsel-bookmark
    [remap describe-face]            #'counsel-describe-face
    [remap describe-function]        #'counsel-describe-function
    [remap describe-variable]        #'counsel-describe-variable
    [remap execute-extended-command] #'counsel-M-x
    [remap find-file]                #'counsel-find-file
    [remap find-library]             #'counsel-find-library
    [remap info-lookup-symbol]       #'counsel-info-lookup-symbol
    [remap imenu]                    #'counsel-imenu
    [remap recentf-open-files]       #'counsel-recentf
    [remap org-capture]              #'counsel-org-capture
    [remap swiper]                   #'counsel-grep-or-swiper)

  :config

  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
        ;; Add smart-casing and compressed archive searching (-zS) to default
        ;; command arguments:
        counsel-rg-base-command "rg -zS --no-heading --line-number --color never %s ."
        counsel-ag-base-command "ag -zS --nocolor --nogroup %s"
        counsel-pt-base-command "pt -zS --nocolor --nogroup -e %s")

  (add-to-list 'swiper-font-lock-exclude #'+doom-dashboard-mode nil #'eq)


  ;; Factories
  (defun +ivy-action-reloading (cmd)
    (lambda (x)
      (funcall cmd x)
      (ivy--reset-state ivy-last)))

  (defun +ivy-action-given-file (cmd prompt)
    (lambda (source)
      (let* ((enable-recursive-minibuffers t)
             (target (read-file-name (format "%s %s to:" prompt source))))
        (funcall cmd source target 1))))

  ;; Configure `counsel-find-file'
  (ivy-add-actions
   'counsel-find-file
   `(("b" counsel-find-file-cd-bookmark-action "cd bookmark")
     ("s" counsel-find-file-as-root "open as root")
     ("m" counsel-find-file-mkdir-action "mkdir")
     ("c" ,(+ivy-action-given-file #'copy-file "Copy file") "copy file")
     ("d" ,(+ivy-action-reloading #'+ivy-confirm-delete-file) "delete")
     ("r" (lambda (path) (rename-file path (read-string "New name: "))) "rename")
     ("R" ,(+ivy-action-reloading (+ivy-action-given-file #'rename-file "Move")) "move")
     ("f" find-file-other-window "other window")
     ("F" find-file-other-frame "other frame")
     ("p" (lambda (path) (with-ivy-window (insert (file-relative-name path default-directory)))) "insert relative path")
     ("P" (lambda (path) (with-ivy-window (insert path))) "insert absolute path")
     ("l" (lambda (path) "Insert org-link with relative path"
            (with-ivy-window (insert (format "[[./%s]]" (file-relative-name path default-directory))))) "insert org-link (rel. path)")
     ("L" (lambda (path) "Insert org-link with absolute path"
            (with-ivy-window (insert (format "[[%s]]" path)))) "insert org-link (abs. path)")))

  (ivy-add-actions
   'counsel-ag ; also applies to `counsel-rg' & `counsel-pt'
   '(("O" +ivy-git-grep-other-window-action "open in other window"))))


(use-package counsel-projectile
  :disabled t
  :commands (counsel-projectile-find-file counsel-projectile-find-dir counsel-projectile-switch-to-buffer
                                          counsel-projectile-grep counsel-projectile-ag counsel-projectile-switch-project)
  :init
  (define-key! 'global
    [remap projectile-find-file]        #'counsel-projectile-find-file
    [remap projectile-find-dir]         #'counsel-projectile-find-dir
    [remap projectile-switch-to-buffer] #'counsel-projectile-switch-to-buffer
    [remap projectile-grep]             #'counsel-projectile-grep
    [remap projectile-ag]               #'counsel-projectile-ag
    [remap projectile-switch-project]   #'counsel-projectile-switch-project)
  :config
  ;; no highlighting visited files; slows down the filtering
  (ivy-set-display-transformer #'counsel-projectile-find-file nil))


(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))


(use-package flx
  :defer t  ; is loaded by ivy
  :init
  (setq ivy-re-builders-alist
        '((counsel-ag . ivy--regex-plus)
          (counsel-grep . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy))
        ivy-initial-inputs-alist nil))


;; Used by `counsel-M-x'
(use-package amx
  :config (setq amx-save-file (concat sea-cache-dir "amx-items")))

;;
;; Evil key fixes

(map!
      :after ivy
      :map ivy-occur-mode-map
      :n [mouse-1]  #'ivy-occur-click
      :n "<return>" #'ivy-occur-press-and-switch
      :m "j"        #'ivy-occur-next-line
      :m "k"        #'ivy-occur-previous-line
      :m "h"        #'evil-backward-char
      :m "l"        #'evil-forward-char
      :m "g"        nil
      :m "gg"       #'evil-goto-first-line
      :n "gf"       #'ivy-occur-press
      :n "ga"       #'ivy-occur-read-action
      :n "go"       #'ivy-occur-dispatch
      :n "gc"       #'ivy-occur-toggle-calling
      :n "gr"       #'ivy-occur-revert-buffer
      :n "q"        #'quit-window

      :map ivy-occur-grep-mode-map
      :v "j"        #'evil-next-line
      :v "k"        #'evil-previous-line
      :n "D"        #'ivy-occur-delete-candidate
      :n "C-d"      #'evil-scroll-down
      :n "d"        #'ivy-occur-delete-candidate
      :n "C-x C-q"  #'ivy-wgrep-change-to-wgrep-mode
      :n "i"        #'ivy-wgrep-change-to-wgrep-mode
      :n "gd"       #'ivy-occur-delete-candidate
      :n [mouse-1]  #'ivy-occur-click
      :n "<return>" #'ivy-occur-press-and-switch
      :m "j"        #'ivy-occur-next-line
      :m "k"        #'ivy-occur-previous-line
      :m "h"        #'evil-backward-char
      :m "l"        #'evil-forward-char
      :m "g"        nil
      :m "gg"       #'evil-goto-first-line
      :n "gf"       #'ivy-occur-press
      :n "gr"       #'ivy-occur-revert-buffer
      :n "ga"       #'ivy-occur-read-action
      :n "go"       #'ivy-occur-dispatch
      :n "gc"       #'ivy-occur-toggle-calling
      ;; quit
      :n "q"        #'quit-window)

(provide 'init-ivy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ivy.el ends here
