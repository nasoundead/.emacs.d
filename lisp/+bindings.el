;;; config/default/+bindings.el -*- lexical-binding: t; -*-

;; This file defines a Spacemacs-esque keybinding scheme

;; expand-region's prompt can't tell what key contract-region is bound to, so we
;; tell it explicitly.
(setq expand-region-contract-fast-key "V")

;;
(map! [remap evil-jump-to-tag] #'projectile-find-tag
      [remap find-tag]         #'projectile-find-tag

      :gni "C-a" #'sea/backward-to-bol-or-indent
      :gni "C-e" #'sea/forward-to-last-non-comment-or-eol
      :gni [C-return]    #'+default/newline-below
      :gni [C-S-return]  #'+default/newline-above

      :gni [F8] #'+treemacs/toggle

      ;; Ensure there are no conflicts
      :nmvo sea-leader-key nil
      :nmvo sea-localleader-key nil

      ;; Swap RET/C-j in insert mode
      :i [remap newline]   #'newline-and-indent
      :i "C-S-j"           #'+default/newline

      :n "s"    #'evil-window-map
      ;; --- Global keybindings ---------------------------
      ;; Make M-x available everywhere
      :gnvime "M-x" #'execute-extended-command
      :gnvime "A-x" #'execute-extended-command

      ;; A little sandbox to run code in
      :gnvime "M-:" #'eval-expression

      ;; Text-scaling
      :n "M-+"   (λ! (text-scale-set 0))
      :n "M-="   #'text-scale-increase
      :n "M--"   #'text-scale-decrease

      :enm "C-h"   #'evil-window-left
      :enm "C-j"   #'evil-window-down
      :enm "C-k"   #'evil-window-up
      :enm "C-l"   #'evil-window-right

      ;; Simple window/frame navigation/manipulation
      :n "M-w"   #'delete-window
      :n "M-W"   #'delete-frame
      :n "C-M-f" #'toggle-frame-fullscreen
      :n "M-n"   #'evil-buffer-new
      :n "M-N"   #'make-frame

      ;; Other sensible, textmate-esque global bindings
      :n "M-b"   #'+default/compile
      :n "M-a"   #'mark-whole-buffer
      :n "M-c"   #'evil-yank
      :n "M-q"   (if (daemonp) #'delete-frame #'evil-quit-all)
      :n "M-f"   #'consult-line
      :n "M-s"   #'save-buffer
      :gnvimr "M-v" #'clipboard-yank

      ;; --- Personal vim-esque bindings ------------------
      :n  "zx" #'kill-this-buffer
      :n  "ZX" #'bury-buffer
      :m  "]a" #'evil-forward-arg
      :m  "[a" #'evil-backward-arg
      :n  "]b" #'next-buffer
      :n  "[b" #'previous-buffer
      :nv  "K" #'+lookup/documentation
      ;; :nv  "gd" #'+lookup/definition
      :nv  "gd" #'xref-find-definitions
      :nv  "gr" #'+lookup/references
      ;; :nv  "gD" #'xref-find-references
      :nv  "gf" #'+lookup/file
      :n  "gp" #'+evil/reselect-paste
      :v  "gp" #'+evil/paste-preserve-register
      ;; repeat in visual mode (FIXME buggy)
      :v  "."  #'evil-repeat
      ;; don't leave visual mode after shifting
      :v  "<"  #'+evil/visual-dedent    ; vnoremap < <gv
      :v  ">"  #'+evil/visual-indent    ; vnoremap > >gv


      ;; --- Plugin bindings ------------------------------
      ;; auto-yasnippet
      :i  [C-tab] #'aya-expand
      :nv [C-tab] #'aya-create

      (:map org-mode-map
        :n "H" #'org-up-element)

      (:map custom-theme-choose-mode-map
        :gvnime "j" #'widget-forward
        :gvnime "k" #'widget-backward)

      (:map evilem-map
        "a" (evilem-create #'evil-forward-arg)
        "A" (evilem-create #'evil-backward-arg)
        "s" #'evil-avy-goto-char-2
        "/" #'evil-avy-goto-char-timer)

      (:after company
        (:map company-active-map
          ;; Don't interfere with `evil-delete-backward-word' in insert mode
          "C-w"     nil
          "C-n"     #'company-select-next
          "C-p"     #'company-select-previous
          "C-j"     #'company-select-next
          "C-k"     #'company-select-previous
          "C-h"     #'company-show-doc-buffer
          "C-u"     #'company-previous-page
          "C-d"     #'company-next-page
          "C-s"     #'company-filter-candidates
          "C-S-s" #'counsel-company
          "C-SPC"   #'company-complete-common
          [tab]     #'company-complete-common-or-cycle
          [backtab] #'company-select-previous)
        ;; Automatically applies to `company-filter-map'
        (:map company-search-map
          "C-n"     #'company-select-next-or-abort
          "C-p"     #'company-select-previous-or-abort
          "C-j"     #'company-select-next-or-abort
          "C-k"     #'company-select-previous-or-abort
          "C-s"     (λ! (company-search-abort) (company-filter-candidates))
          [escape]  #'company-search-abort))

      ;; counsel
      ;; (:after counsel
      ;;   (:map counsel-ag-map
      ;;     [backtab]  #'+ivy/wgrep-occur      ; search/replace on results
      ;;     "C-SPC"    #'ivy-call-and-recenter ; preview
      ;;     "M-RET"    (+ivy-do-action! #'+ivy-git-grep-other-window-action)))


      (:map xref--xref-buffer-mode-map
        :n "RET" #'xref-goto-xref
        :n "j" #'xref-next-line
        :n "k" #'xref-prev-line)
      ;; evil
      (:after evil
        :textobj "x" #'evil-inner-xml-attr               #'evil-outer-xml-attr
        :textobj "a" #'evil-inner-arg                    #'evil-outer-arg
        :textobj "B" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block
        :textobj "i" #'evil-indent-plus-i-indent         #'evil-indent-plus-a-indent
        :textobj "k" #'evil-indent-plus-i-indent-up      #'evil-indent-plus-a-indent-up
        :textobj "j" #'evil-indent-plus-i-indent-up-down #'evil-indent-plus-a-indent-up-down

        (:map evil-window-map           ; prefix "C-w"
          ;; Navigation
          "C-h"     #'evil-window-left
          "C-j"     #'evil-window-down
          "C-k"     #'evil-window-up
          "C-l"     #'evil-window-right
          "C-w"     #'other-window
          "C-S-w"   #'ace-swap-window
          ;; Window undo/redo
          "u"       #'winner-undo
          "C-u"     #'winner-undo
          "C-r"     #'winner-redo
          ;; split
          "s"       #'sea/split-window-horizontally-instead
          "v"       #'sea/split-window-vertically-instead
          ;; Delete window
          "c"     #'delete-window
          "h"     #'shrink-window-horizontally
          "l"     #'enlarge-window-horizontally
          "j"     #'enlarge-window
          "k"     #'shrink-window
          ))

      ;; evil-commentary
      :n  "gc"  #'evil-commentary

      ;; evil-exchange
      :n  "gx"  #'evil-exchange

      ;; evil-magit
      (:after evil-magit
        :map (magit-status-mode-map magit-revision-mode-map)
        :n "C-j" nil
        :n "C-k" nil)

      ;; evil-mc
      (:prefix "gz"
        :nv "m" #'evil-mc-make-all-cursors
        :nv "u" #'evil-mc-undo-all-cursors
        :nv "z" #'+evil/mc-make-cursor-here
        :nv "t" #'+evil/mc-toggle-cursors
        :nv "n" #'evil-mc-make-and-goto-next-cursor
        :nv "p" #'evil-mc-make-and-goto-prev-cursor
        :nv "N" #'evil-mc-make-and-goto-last-cursor
        :nv "P" #'evil-mc-make-and-goto-first-cursor
        :nv "d" #'evil-mc-make-and-goto-next-match
        :nv "D" #'evil-mc-make-and-goto-prev-match
        :nv "j" #'evil-mc-make-cursor-move-next-line
        :nv "k" #'evil-mc-make-cursor-move-prev-line)
      (:after evil-mc
        :map evil-mc-key-map
        :nv "C-n" #'evil-mc-make-and-goto-next-cursor
        :nv "C-N" #'evil-mc-make-and-goto-last-cursor
        :nv "C-p" #'evil-mc-make-and-goto-prev-cursor
        :nv "C-P" #'evil-mc-make-and-goto-first-cursor)

      ;; evil-multiedit
      :v  "R"     #'evil-multiedit-match-all
      :n  "M-d"   #'evil-multiedit-match-symbol-and-next
      :n  "M-D"   #'evil-multiedit-match-symbol-and-prev
      :v  "M-d"   #'evil-multiedit-match-and-next
      :v  "M-D"   #'evil-multiedit-match-and-prev
      :nv "C-M-d" #'evil-multiedit-restore
      (:after evil-multiedit
        (:map evil-multiedit-state-map
          "M-d" #'evil-multiedit-match-and-next
          "M-D" #'evil-multiedit-match-and-prev
          "RET" #'evil-multiedit-toggle-or-restrict-region)
        (:map (evil-multiedit-state-map evil-multiedit-insert-state-map)
          "C-n" #'evil-multiedit-next
          "C-p" #'evil-multiedit-prev))


      ;; evil-surround
      :v  "S"  #'evil-surround-region
      :o  "s"  #'evil-surround-edit
      :o  "S"  #'evil-Surround-edit

      ;; evil-lion
      :n "gl" #'evil-lion-left
      :n "gL" #'evil-lion-right
      :v "gl" #'evil-lion-left
      :v "gL" #'evil-lion-right

      ;; expand-region
      :v  "v"  #'er/expand-region
      :v  "V"  #'er/contract-region

      ;; flycheck
      :m  "]e" #'next-error
      :m  "[e" #'previous-error
      (:after flycheck
        :map flycheck-error-list-mode-map
        :n "C-n" #'flycheck-error-list-next-error
        :n "C-p" #'flycheck-error-list-previous-error
        :n "j"   #'flycheck-error-list-next-error
        :n "k"   #'flycheck-error-list-previous-error
        :n "RET" #'flycheck-error-list-goto-error)

      ;; flyspell
      :m  "]S" #'flyspell-correct-word-generic
      :m  "[S" #'flyspell-correct-previous-word-generic
      (:after flyspell
        ;; Press RET on misspelled words to correct them
        (:map flyspell-mouse-map
          "RET" #'flyspell-correct-word-generic
          "<mouse-1>" #'flyspell-correct-word-generic))

      ;; git-gutter
      :m  "]d" #'git-gutter:next-hunk
      :m  "[d" #'git-gutter:previous-hunk

      ;; git-timemachine
      (:after git-timemachine
        (:map git-timemachine-mode-map
          :n "C-p" #'git-timemachine-show-previous-revision
          :n "C-n" #'git-timemachine-show-next-revision
          :n "[["  #'git-timemachine-show-previous-revision
          :n "]]"  #'git-timemachine-show-next-revision
          :n "q"   #'git-timemachine-quit
          :n "gb"  #'git-timemachine-blame))

      ;; gist
      (:after gist
        :map gist-list-menu-mode-map
        :n "RET" #'+gist/open-current
        :n "b"   #'gist-browse-current-url
        :n "c"   #'gist-add-buffer
        :n "d"   #'gist-kill-current
        :n "f"   #'gist-fork
        :n "q"   #'quit-window
        :n "r"   #'gist-list-reload
        :n "s"   #'gist-star
        :n "S"   #'gist-unstar
        :n "y"   #'gist-print-current-url)
      ;; hl-todo
      :m  "]t" #'hl-todo-next
      :m  "[t" #'hl-todo-previous

      ;; ivy
      (:after ivy
        :map ivy-minibuffer-map
        "C-SPC" #'ivy-call-and-recenter ; preview file
        "C-l"   #'ivy-alt-done
        "C-k"   #'ivy-previous-line
        "C-j"   #'ivy-next-line
        "M-z"   #'undo
        "M-v"   #'yank
        "C-v"   #'yank
        :map ivy-switch-buffer-map
        "C-k"   #'ivy-previous-line
        "C-j"   #'ivy-next-line
        "C-M-k" #'ivy-switch-buffer-kill
        )


      ;; realgud
      (:after realgud
        :map realgud:shortkey-mode-map
        :n "j" #'evil-next-line
        :n "k" #'evil-previous-line
        :n "h" #'evil-backward-char
        :n "l" #'evil-forward-char
        :m "n" #'realgud:cmd-next
        :m "b" #'realgud:cmd-break
        :m "B" #'realgud:cmd-clear
        :n "c" #'realgud:cmd-continue)

      ;; yasnippet
      (:after yasnippet
        (:map yas-keymap
          "C-e"           #'+snippets/goto-end-of-field
          "C-a"           #'+snippets/goto-start-of-field
          "<M-right>"     #'+snippets/goto-end-of-field
          "<M-left>"      #'+snippets/goto-start-of-field
          "<M-backspace>" #'+snippets/delete-to-start-of-field
          ;; [backspace]     #'+snippets/delete-backward-char
          ;; [delete]        #'+snippets/delete-forward-char-or-field
          )
        (:map yas-minor-mode-map
          :ig [tab] yas-maybe-expand
          :v  [tab] #'yas-insert-snippet))

      ;; treemacs
      (:after treemacs
        (:map treemacs-mode-map
          :n "h" #'treemacs-goto-parent-node
          :n "l" #'treemacs-RET-action)
        )
      ;; --- Major mode bindings --------------------------
      (:after markdown-mode
        (:map markdown-mode-map
          ;; fix conflicts with private bindings
          "<backspace>" nil
          "<M-left>"    nil
          "<M-right>"   nil))


      ;; --- Built-in plugins -----------------------------

      (:after comint
        ;; TAB auto-completion in term buffers
        :map comint-mode-map [tab] #'company-complete)

      (:map* (help-mode-map helpful-mode-map)
             :n "o"  #'ace-link-help
             :n "q"  #'quit-window
             :n "Q"  #'ivy-resume
             :n "]l" #'forward-button
             :n "[l" #'backward-button)

      (:after vc-annotate
        :map vc-annotate-mode-map
        [remap quit-window] #'kill-this-buffer))



;; <leader>
;;
(map! :leader
      :desc "Ex command"              :nv ";"  #'evil-ex
      :desc "M-x"                     :nv ":"  #'execute-extended-command
      :desc "Pop up scratch buffer"   :nv "x"  #'sea/open-scratch-buffer
      :desc "Org Capture"             :nv "X"  #'org-capture

      ;; Most commonly used
      :desc "Find file in project"    :n "SPC" #'projectile-find-file
      :desc "Browse files"            :n "."   #'find-file

      :desc "Jump to bookmark"        :n "RET" #'bookmark-jump


      :desc "Switch workspace buffer" :n "," #'persp-switch-to-buffer
      :desc "Switch buffer"           :n "<" #'switch-to-buffer

      ;; C-u is used by evil
      :desc "Universal argument"      :n "u"  #'universal-argument
      :desc "window"                  :n "w"  evil-window-map

      (:desc "previous..." :prefix "["
        :desc "Text size"             :nv "[" #'text-scale-decrease
        :desc "Buffer"                :nv "b" #'previous-buffer
        :desc "Diff Hunk"             :nv "d" #'git-gutter:previous-hunk
        :desc "Todo"                  :nv "t" #'hl-todo-previous
        :desc "Error"                 :nv "e" #'previous-error
        :desc "Workspace"             :nv "w" #'+workspace/switch-left
        :desc "Smart jump"            :nv "h" #'smart-backward
        :desc "Spelling error"        :nv "s" #'evil-prev-flyspell-error
        :desc "Spelling correction"   :n  "S" #'flyspell-correct-previous-word-generic)

      (:desc "next..." :prefix "]"
        :desc "Text size"             :nv "]" #'text-scale-increase
        :desc "Buffer"                :nv "b" #'next-buffer
        :desc "Diff Hunk"             :nv "d" #'git-gutter:next-hunk
        :desc "Todo"                  :nv "t" #'hl-todo-next
        :desc "Error"                 :nv "e" #'next-error
        :desc "Workspace"             :nv "w" #'+workspace/switch-right
        :desc "Spelling error"        :nv "s" #'evil-next-flyspell-error
        :desc "Spelling correction"   :n  "S" #'flyspell-correct-word-generic)

      (:desc "s+default/search-cwd+default/search-cwdearch" :prefix "/"

        :desc "Buffer"                 :nv "b" #'consult-line
        :desc "Project"                :nv "p" #'+vertico/project-search
        :desc "Directory"              :nv "d" #'+vertico/project-search-from-cwd

        :desc "Symbols"                :nv "i" #'consult-imenu
        :desc "Symbols across buffers" :nv "I" #'consult-imenu-multi
        :desc "Online providers"       :nv "o" #'+lookup/online-select)

      (:desc "buffer" :prefix "b"
        :desc "New empty buffer"        :n "n" #'evil-buffer-new

        :desc "Switch workspace buffer" :n "b" #'ivy-switch-buffer
        :desc "Switch buffer"           :n "B" #'switch-to-buffer

        :desc "Kill buffer"             :n "k" #'kill-this-buffer
        :desc "Kill other buffers"      :n "o" #'kill-other-buffers
        :desc "Save buffer"             :n "s" #'save-buffer
        :desc "Bury buffer"             :n "z" #'bury-buffer
        :desc "Next buffer"             :n "]" #'next-buffer
        :desc "Previous buffer"         :n "[" #'previous-buffer
        )

      (:desc "code" :prefix "c"
        :desc "List errors"               :n  "x" #'flycheck-list-errors
        :desc "Evaluate buffer/region"    :n  "e" #'+eval/buffer
        :v  "e" #'+eval/region
        :desc "Evaluate & replace region" :nv "E" #'+eval:replace-region
        :desc "Format buffer/region"      :n  "f" #'+format/buffer
        :v  "f" #'+format/region
        :desc "Build tasks"               :nv "b" #'+eval/build
        :desc "Jump to definition"        :n  "d" #'+lookup/definition
        :desc "Jump to references"        :n  "D" #'+lookup/references
        :desc "Open REPL"                 :n  "r" #'+eval/open-repl
        :v  "r" #'+eval:repl)

      (:desc "file" :prefix "f"
        :desc "Find file"                 :n "f" #'find-file
        :desc "Sudo find file"            :n ">" #'sea/sudo-find-file
        :desc "Find file in project"      :n "/" #'projectile-find-file
        :desc "Find file from here"       :n "?" #'counsel-file-jump
        :desc "Find other file"           :n "a" #'projectile-find-other-file
        :desc "Open project editorconfig" :n "c" #'editorconfig-find-current-editorconfig
        :desc "Find directory"            :n "d" #'dired
        :desc "Find file in emacs.d"      :n "e" #'+default/find-in-emacsd
        :desc "Browse emacs.d"            :n "E" #'+default/browse-emacsd
        :desc "Recent files"              :n "r" #'recentf-open-files
        :desc "Recent project files"      :n "R" #'projectile-recentf
        :desc "Yank filename"             :n "y" #'+default/yank-buffer-filename
        :desc "Find file in private config" :n "p" #'+default/find-in-config
        :desc "Browse private config"       :n "P" #'+default/browse-config
        :desc "Delete this file"            :n "X" #'sea/delete-this-file)

      (:desc "git" :prefix "g"
        :desc "Magit blame"           :n  "b" #'magit-blame
        :desc "Magit commit"          :n  "c" #'magit-commit
        :desc "Magit clone"           :n  "C" #'+magit/clone
        :desc "Magit dispatch"        :n  "d" #'magit-dispatch-popup
        :desc "Magit find-file"       :n  "f" #'magit-find-file
        :desc "Magit status"          :n  "g" #'magit-status
        :desc "Magit file delete"     :n  "x" #'magit-file-delete
        :desc "List gists"            :n  "G" #'+gist:list
        :desc "Initialize repo"       :n  "i" #'magit-init
        :desc "Browse issues tracker" :n  "I" #'+vc/git-browse-issues
        :desc "Magit buffer log"      :n  "l" #'magit-log-buffer-file
        :desc "List repositories"     :n  "L" #'magit-list-repositories
        :desc "Browse remote"         :n  "o" #'+vc/git-browse
        :desc "Magit push popup"      :n  "p" #'magit-push-popup
        :desc "Magit pull popup"      :n  "P" #'magit-pull-popup
        :desc "Git revert hunk"       :n  "r" #'git-gutter:revert-hunk
        :desc "Git revert file"       :n  "R" #'vc-revert
        :desc "Git stage hunk"        :n  "s" #'git-gutter:stage-hunk
        :desc "Git stage file"        :n  "S" #'magit-stage-file
        :desc "Git time machine"      :n  "t" #'git-timemachine-toggle
        :desc "Git unstage file"      :n  "U" #'magit-unstage-file
        :desc "Next hunk"             :nv "]" #'git-gutter:next-hunk
        :desc "Previous hunk"         :nv "[" #'git-gutter:previous-hunk)

      (:desc "help" :prefix "h"
        :n "h" help-map
        :desc "Apropos"               :n  "a" #'apropos
        :desc "Describe char"         :n  "c" #'describe-char
        :desc "Describe function"     :n  "f" #'describe-function
        :desc "Describe face"         :n  "F" #'describe-face
        :desc "Info"                  :n  "i" #'info-lookup-symbol
        :desc "Describe key"          :n  "k" #'describe-key
        :desc "Find library"          :n  "l" #'find-library
        :desc "View *Messages*"       :n  "m" #'view-echo-area-messages
        :desc "Describe mode"         :n  "M" #'describe-mode
        :desc "Describe variable"     :n  "v" #'describe-variable
        :desc "Man pages"             :n  "w" #'+default/man-or-woman
        )

      (:desc "insert" :prefix "i"

        :desc "From kill-ring"        :nv "y" #'counsel-yank-pop
        :desc "From evil registers"   :nv "r" #'counsel-evil-registers
        :desc "From snippet"          :nv "s" #'yas-insert-snippet)

      (:desc "notes" :prefix "n"
        :desc "Find file in notes"    :n  "n" #'+default/find-in-notes
        :desc "Browse notes"          :n  "N" #'+default/browse-notes
        :desc "Org capture"           :n  "x" #'org-capture)

      (:desc "open" :prefix "o"
        :desc "Org agenda"            :n  "a" #'org-agenda
        :desc "Default browser"       :n  "b" #'browse-url-of-file
        :desc "Debugger"              :n  "d" #'+debug/open
        :desc "REPL"                  :n  "r" #'+eval/open-repl
        :v  "r" #'+eval:repl
        :desc "Dired"                 :n  "-" #'dired-jump

        :desc "Project sidebar"       :n  "p" #'treemacs

        :desc "Imenu sidebar"         :nv "i" #'imenu-list-smart-toggle
        :desc "Terminal"              :n  "t" #'+term/open
        :desc "Terminal in popup"     :n  "T" #'+term/open-popup-in-project
        :desc "Eshell"                :n  "e" #'+eshell/open
        :desc "Eshell in popup"       :n  "E" #'+eshell/open-popup)

      (:desc "project" :prefix "p"
        :desc "Browse project"          :n  "." #'+default/browse-project
        :desc "Find file in project"    :n  "/" #'projectile-find-file
        :desc "Run cmd in project root" :nv "!" #'projectile-run-shell-command-in-root
        :desc "Compile project"         :n  "c" #'projectile-compile-project
        :desc "Find other file"         :n  "o" #'projectile-find-other-file
        :desc "Switch project"          :n  "p" #'projectile-switch-project
        :desc "Recent project files"    :n  "r" #'projectile-recentf
        :desc "Invalidate cache"        :n  "x" #'projectile-invalidate-cache)

      (:desc "quit" :prefix "q"
        :desc "Quit Emacs"             :n "q" #'evil-quit-all
        :desc "Save and quit"          :n "Q" #'evil-save-and-quit
        :desc "Restart Doom"           :n "R" #'restart-emacs)


      (:desc "snippets" :prefix "s"
        :desc "New snippet"           :n  "n" #'yas-new-snippet
        :desc "Insert snippet"        :nv "i" #'yas-insert-snippet
        :desc "Find snippet"          :n  "s" #'+default/find-in-snippets
        :desc "Find snippet for mode" :n  "S" #'+default/browse-snippets
        :desc "Find global snippet"   :n  "/" #'yas-visit-snippet-file
        :desc "Reload snippets"       :n  "r" #'yas-reload-all)

      (:desc "toggle" :prefix "t"
        :desc "Flyspell"               :n "s" #'flyspell-mode
        :desc "Flycheck"               :n "f" #'flycheck-mode
        :desc "Frame fullscreen"       :n "F" #'toggle-frame-fullscreen
        :desc "Indent guides"          :n "i" #'highlight-indentation-mode
        :desc "Indent guides (column)" :n "I" #'highlight-indentation-current-column-mode
        :desc "Impatient mode"         :n "h" #'+impatient-mode/toggle
        :desc "Evil goggles"           :n "g" #'evil-goggles-mode
        :desc "org-tree-slide mode"    :n "p" #'+org-present/start))

;;
;; Keybinding fixes

;; This section is dedicated to "fixing" certain keys so that they behave
;; sensibly (and consistently with similar contexts).

;; Make SPC u SPC u possible (#747)
(define-key universal-argument-map
  (kbd (concat sea-leader-key " u")) #'universal-argument-more)


(after! tabulated-list
  (define-key tabulated-list-mode-map "q" #'quit-window))


(evil-define-key* 'insert 'global
  ;; I want C-a and C-e to be a little smarter. C-a will jump to indentation.
  ;; Pressing it again will send you to the true bol. Same goes for C-e,
  ;; except it will ignore comments and trailing whitespace before jumping to
  ;; eol.
  "\C-a" #'sea/backward-to-bol-or-indent
  "\C-e" #'sea/forward-to-last-non-comment-or-eol
  "\C-u" #'sea/backward-kill-to-bol-and-indent
  ;; Emacsien motions for insert mode
  "\M-b" #'backward-word
  "\M-f" #'forward-word
  ;; textmate-esque deletion
  [M-backspace] #'sea/backward-kill-to-bol-and-indent)

(after! man
  (evil-define-key* 'normal Man-mode-map "q" #'kill-this-buffer))

(after! view
  (define-key view-mode-map [escape] #'View-quit-all))


(after! man
  (evil-define-key* 'normal Man-mode-map "q" #'kill-this-buffer))


;; Evil-collection fixes
(setq evil-collection-key-blacklist
      (list "C-j" "C-k" "gd" "gf" "K" "[" "]" "gz"
            sea-leader-key sea-localleader-key))

(evil-ex-define-cmd "mc"      #'+multiple-cursors:evil-mc)
(evil-ex-define-cmd "iedit"   #'evil-multiedit-ex-match)
(evil-ex-define-cmd "git"     #'magit-status)         ; open magit status window
(evil-ex-define-cmd "rg"      #'+vertico/project-search)
(evil-ex-define-cmd "rgc[wd]" #'+vertico/project-search-from-cwd)
