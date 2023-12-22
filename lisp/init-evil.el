;; (use-package evil
;;   :straight (evil
;;             :type git
;;             :host github
;;             :repo "emacs-evil/evil"))
;; (add-hook 'after-init-hook #'(lambda() (evil-mode 1)))
;; Enable Evil
;; Set before requiring evil module
;; (setq evil-magic 'very-magic)
;; (setq evil-search-module 'evil-search)
;; (setq evil-ex-search-vim-style-regexp t)
;; (setq evil-want-keybinding nil)
;; (require 'evil)
;; (evil-mode 1)
(use-package evil
  :straight (evil
             :type git
             :host github
             :repo "emacs-evil/evil")
  :init
  (setq evil-magic 'very-magic)
  (setq evil-search-module 'evil-search)
  (setq evil-ex-search-vim-style-regexp t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-escape
  :straight (evil-escape
             :type git
             :host github
             :repo "syl20bnr/evil-escape")
  :custom
  (evil-escape-key-sequence "jk")
  (evil-escape-delay 0.2)
  :config
  (evil-escape-mode))

(use-package evil-commentary
  :commands (evil-commentary evil-commentary-yank evil-commentary-line)
  :config (evil-commentary-mode 1))

(use-package evil-snipe
  :ensure t
  :diminish
  :init
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
  ;; (evil-define-key '(normal motion) evil-snipe-local-mode-map
  ;;   "s" nil
  ;;   "S" nil)
  (setq evil-snipe-scope 'whole-visible)
  (setq evil-snipe-repeat-scope 'whole-visible)

  (evil-define-key 'operator evil-snipe-local-mode-map
    "z" 'evil-snipe-s
    "Z" 'evil-snipe-S
    "x" 'evil-snipe-x
    "X" 'evil-snipe-X)

  (evil-define-key 'motion evil-snipe-override-local-mode-map
    "f" 'evil-snipe-f
    "F" 'evil-snipe-F
    "t" 'evil-snipe-t
    "T" 'evil-snipe-T)

  (when evil-snipe-override-evil-repeat-keys
    (evil-define-key 'motion map
      ";" 'evil-snipe-repeat
      "," 'evil-snipe-repeat-reverse)) )

(use-package evil-easymotion
  :after evil-snipe
  :commands evilem-create
  :init
  (evilem-default-keybindings "gs")
  )

(use-package evil-embrace
  :after evil-surround
  :config
  (setq evil-embrace-show-help-p nil)
  (evil-embrace-enable-evil-surround-integration)

  (defun +evil--embrace-get-pair (char)
    (if-let* ((pair (cdr-safe (assoc (string-to-char char) evil-surround-pairs-alist))))
        pair
      (if-let* ((pair (assoc-default char embrace--pairs-list)))
          (if-let* ((real-pair (and (functionp (embrace-pair-struct-read-function pair))
                                    (funcall (embrace-pair-struct-read-function pair)))))
              real-pair
            (cons (embrace-pair-struct-left pair) (embrace-pair-struct-right pair)))
        (cons char char))))

  (defun +evil--embrace-escaped ()
    "Backslash-escaped surround character support for embrace."
    (let ((char (read-char "\\")))
      (if (eq char 27)
          (cons "" "")
        (let ((pair (+evil--embrace-get-pair (string char)))
              (text (if (sp-point-in-string) "\\\\%s" "\\%s")))
          (cons (format text (car pair))
                (format text (cdr pair)))))))

  (defun +evil--embrace-latex ()
    "LaTeX command support for embrace."
    (cons (format "\\%s{" (read-string "\\")) "}"))

  (defun +evil--embrace-elisp-fn ()
    "Elisp function support for embrace."
    (cons (format "(%s " (or (read-string "(") "")) ")"))

  ;; Add escaped-sequence support to embrace
  (push (cons ?\\ (make-embrace-pair-struct
                   :key ?\\
                   :read-function #'+evil--embrace-escaped
                   :left-regexp "\\[[{(]"
                   :right-regexp "\\[]})]"))
        (default-value 'embrace--pairs-list))

  ;; Add extra pairs
  (add-hook 'LaTeX-mode-hook #'embrace-LaTeX-mode-hook)
  (add-hook 'org-mode-hook   #'embrace-org-mode-hook)
  (add-hook! emacs-lisp-mode
    (embrace-add-pair ?\` "`" "'"))
  (add-hook! (emacs-lisp-mode lisp-mode)
    (embrace-add-pair-regexp ?f "([^ ]+ " ")" #'+evil--embrace-elisp-fn))
  (add-hook! (org-mode LaTeX-mode)
    (embrace-add-pair-regexp ?l "\\[a-z]+{" "}" #'+evil--embrace-latex)))

(use-package evil-escape
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.25)
  (add-hook 'after-init-hook #'evil-escape-mode)
  :config
  ;; no `evil-escape' in minibuffer
  (push #'minibufferp evil-escape-inhibit-functions))

(use-package evil-exchange
  :commands evil-exchange
  :config
  (defun +evil|escape-exchange ()
    (when evil-exchange--overlays
      (evil-exchange-cancel)
      t))
  (add-hook '+evil-esc-hook #'+evil|escape-exchange))


(use-package evil-matchit
  :commands (evilmi-jump-items evilmi-text-object global-evil-matchit-mode)
  :config
  (global-evil-matchit-mode 1)
  (defun +evil|simple-matchit ()
    "A hook to force evil-matchit to favor simple bracket jumping. Helpful when
the new algorithm is confusing, like in python or ruby."
    (setq-local evilmi-always-simple-jump t))
  (add-hook 'python-mode-hook #'+evil|simple-matchit))

(use-package evil-multiedit
  :commands (evil-multiedit-match-all
             evil-multiedit-match-and-next
             evil-multiedit-match-and-prev
             evil-multiedit-match-symbol-and-next
             evil-multiedit-match-symbol-and-prev
             evil-multiedit-toggle-or-restrict-region
             evil-multiedit-next
             evil-multiedit-prev
             evil-multiedit-abort
             evil-multiedit-ex-match))

(use-package evil-mc
  :commands (evil-mc-make-cursor-here evil-mc-make-all-cursors
                                      evil-mc-undo-all-cursors evil-mc-pause-cursors
                                      evil-mc-resume-cursors evil-mc-make-and-goto-first-cursor
                                      evil-mc-make-and-goto-last-cursor
                                      evil-mc-make-cursor-move-next-line
                                      evil-mc-make-cursor-move-prev-line evil-mc-make-cursor-at-pos
                                      evil-mc-has-cursors-p evil-mc-make-and-goto-next-cursor
                                      evil-mc-skip-and-goto-next-cursor evil-mc-make-and-goto-prev-cursor
                                      evil-mc-skip-and-goto-prev-cursor evil-mc-make-and-goto-next-match
                                      evil-mc-skip-and-goto-next-match evil-mc-skip-and-goto-next-match
                                      evil-mc-make-and-goto-prev-match evil-mc-skip-and-goto-prev-match)
  :init
  (defvar evil-mc-key-map (make-sparse-keymap))
  :config
  (global-evil-mc-mode +1)

  ;; Add custom commands to whitelisted commands
  (dolist (fn '(sea/deflate-space-maybe sea/inflate-space-maybe
                                        sea/backward-to-bol-or-indent sea/forward-to-last-non-comment-or-eol
                                        sea/backward-kill-to-bol-and-indent sea/newline-and-indent))
    (push (cons fn '((:default . evil-mc-execute-default-call)))
          evil-mc-custom-known-commands))

  ;; disable evil-escape in evil-mc; causes unwanted text on invocation
  (push 'evil-escape-mode evil-mc-incompatible-minor-modes)

  (defun +evil|escape-multiple-cursors ()
    "Clear evil-mc cursors and restore state."
    (when (evil-mc-has-cursors-p)
      (evil-mc-undo-all-cursors)
      (evil-mc-resume-cursors)
      t))
  (add-hook '+evil-esc-hook #'+evil|escape-multiple-cursors))

(use-package evil-surround
  :defer t
  :init
  (global-evil-surround-mode)
  :ensure t)
(use-package evil-args)

(use-package evil-lion
  :ensure t
  :config
  (evil-lion-mode))

(use-package evil-collection
  :init
  (add-hook 'after-init-hook  #'evil-collection-init)
  (evil-define-key 'normal dired-mode-map
    (kbd "<RET>") 'dired-find-alternate-file
    (kbd "=") 'sea/dired-diff
    "`" 'dired-open-term
    "o" 'dired-find-file-other-window
    "s" 'dired-sort-toggle-or-edit
    "z" 'dired-get-size
    ")" 'dired-omit-mode)

  (evil-define-key 'normal help-mode-map
    "o" 'link-hint-open-link)
  )


;; key	explanation
;; ------------------
;; gh, gj, gk, gl	navigate between elements
;; vae	select an element

;; Headings and items
;; ------------------
;; key	explanation
;; M-ret	insert heading
;; <tab>, g TAB	fold / unfold headings
;; M-h or <<	promote a heading
;; M-l or >>	demote a heading
;; M-k	move subtree up
;; M-j	move subtree down
;; M-S-h or <aR	promote a subtree
;; M-S-l or >aR	demote a subtree
;; vaR	select a subtree

;; Tables
;; ------------------
;; key	explanation
;; (	previous table cell
;; )	next table cell
;; {	beginning of table
;; }	end of table
;; M-h / M-l	move table column left / right
;; M-k / M-j	move table column up / down
;; vae	select table cell
;; vaE	select table row
;; var	select whole table

;; Agenda
;; ------------------
;; Evil key	Emacs key	explanation
;; gH		Move cursor to the top of window
;; gM		Move cursor to the middle of window
;; gL		Move cursor to the bottom of window
;; <tab>, S-<return>	<tab>	go to the corresponding entry at point
;; g TAB	<tab>	go to the corresponding entry at point
;; <return>	<return>	go to the Org mode file which contains the item at point
;; M-<return>	L	Display Org file and center around the item
;; <space>	<space>	scroll up
;; <delete> or <backspace>	<delete> or <backspace>	scroll down
;; j, k	n, p	next, previous line
;; gj, gk, C-j, C-k	N, P	next, previous item
;; [, ]	b, f	previous, next week
;; J, K	-, +, S-down, S-up	down, up priority
;; H, L	S-left, S-right	modify date to earlier, later
;; t	t	cycle TODO keywords
;; M-j, M-k	M-down, M-up	drag line forward, backward
;; C-S-h, C-S-l	C-S-left, C-S-right	previous, next keyword
;; u	C-_, C-/	undo
;; dd	C-k	delete item
;; da	a	ask and archive item
;; dA	$	archive item
;; ct	:	set tags
;; ce	e	set effort
;; cT	;	set timer
;; i	i	insert entry in diary
;; a	z	add note
;; A	A	append to agenda
;; C	k	capture
;; m	m	mark
;; *	*	toggle all marks
;; %	%	mark regexp
;; M	U	remove all marks
;; x	B	execute action on marks
;; gr	r	refresh agenda
;; gR	g	refresh all agendas
;; ZQ	x	exit agenda
;; ZZ	Q	quit agenda
;; gD	v	tweak display (deadlines, diary, follow/log-mode, entry text, grid, day/week/year
;; ZD	#	dim blocked tasks
;; sc, sr, se, st, s^	<, =, _, /, ^	filter by category, regexp, effort, tag, top headline
;; S	|	remove all filters
;; ss	~	filter/limit interactively
;; I	I	clock in
;; O	O	clock out
;; cg	J	jump to the currently clocked in task within the agenda
;; cc	X	cancel the current running clock
;; cr	R	toggle clocktable mode in an agenda buffer
;; .	.	go to today’s date
;; gc	c	pop up calendar
;; gC	C	pop up date converter
;; p	>	pop up date selector
;; gh	H	pop up holiday calendar
;; gm	M	pop up phases of the moon
;; gs	S	pop up sunrise/sunset times
;; gt	T	pop up tag list
;; +, -	[, ]	manipulate the query by adding a search term with positive or negative selection
(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


(provide 'init-evil)
