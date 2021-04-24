;;; init-smartparens.el --- Configure of smartparens -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'cl)
(use-package smartparens
  :init
  (add-hook 'js-mode-hook #'smartparens-mode)
  ;; (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  (add-hook 'prog-mode-hook #'smartparens-mode)
  ;; (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  ;; (add-hook 'emacs-lisp-mode-hook 'turn-on-smartparens-strict-mode)
  :config
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
  ;; Don't do square-bracket space-expansion where it doesn't make sense to
  (sp-local-pair '(emacs-lisp-mode org-mode markdown-mode gfm-mode)
		 "[" nil :post-handlers '(:rem ("| " "SPC")))

  ;; Reasonable default pairs for comments
  (sp-local-pair (append sp--html-modes '(markdown-mode gfm-mode))
		 "<!--" "-->" :actions '(insert) :post-handlers '(("| " "SPC")))
  (sp-local-pair
   '(js2-mode typescript-mode rjsx-mode rust-mode c-mode c++-mode objc-mode
	      java-mode php-mode css-mode scss-mode less-css-mode stylus-mode)
   "/*" "*/"
   :actions '(insert)
   :post-handlers '(("| " "SPC") ("|\n*/[i][d-2]" "RET") ("\n* ||\n*/[i][d-2]" "*")))

  ;; Highjacks backspace to:
  ;;  a) balance spaces inside brackets/parentheses ( | ) -> (|)
  ;;  b) delete space-indented `tab-width' steps at a time
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
  (defun +default*delete-backward-char (n &optional killflag)
    "Same as `delete-backward-char', but preforms these additional checks:

+ If point is surrounded by (balanced) whitespace and a brace delimiter ({} []
  ()), delete a space on either side of the cursor.
+ If point is at BOL and surrounded by braces on adjacent lines, collapse
  newlines:
  {
  |
  } => {|}
+ Otherwise, resort to `doom--backward-delete-whitespace-to-column'.
+ Resorts to `delete-char' if n > 1"
    (interactive "p\nP")
    (or (integerp n)
	(signal 'wrong-type-argument (list 'integerp n)))
    (cond ((and (use-region-p)
		delete-active-region
		(= n 1))
	   ;; If a region is active, kill or delete it.
	   (if (eq delete-active-region 'kill)
	       (kill-region (region-beginning) (region-end) 'region)
	     (funcall region-extract-function 'delete-only)))
	  ;; In Overwrite mode, maybe untabify while deleting
	  ((null (or (null overwrite-mode)
		     (<= n 0)
		     (memq (char-before) '(?\t ?\n))
		     (eobp)
		     (eq (char-after) ?\n)))
	   (let ((ocol (current-column)))
	     (delete-char (- n) killflag)
	     (save-excursion
	       (insert-char ?\s (- ocol (current-column)) nil))))
	  ;;
	  ((and (= n 1) (bound-and-true-p smartparens-mode))
	   (cond ((and (memq (char-before) (list ?\  ?\t))
		       (save-excursion
			 (and (/= (skip-chars-backward " \t" (line-beginning-position)) 0)
			      (bolp))))
		  (doom--backward-delete-whitespace-to-column))
		 ((let* ((pair (ignore-errors (sp-get-thing)))
			 (op   (plist-get pair :op))
			 (cl   (plist-get pair :cl))
			 (beg  (plist-get pair :beg))
			 (end  (plist-get pair :end)))
		    (cond ((and end beg (= end (+ beg (length op) (length cl))))
			   (sp-backward-delete-char 1))
			  ((doom-surrounded-p pair 'inline 'balanced)
			   (delete-char -1 killflag)
			   (delete-char 1)
			   (when (= (point) (+ (length cl) beg))
			     (sp-backward-delete-char 1)
			     (sp-insert-pair op)))
			  ((and (bolp) (doom-surrounded-p pair nil 'balanced))
			   (delete-region beg end)
			   (sp-insert-pair op)
			   t)
			  ((run-hook-with-args-until-success 'doom-delete-backward-functions))
			  ((doom--backward-delete-whitespace-to-column)))))))
	  ;; Otherwise, do simple deletion.
	  ((delete-char (- n) killflag))))
  (defun +default*newline-indent-and-continue-comments ()
    "A replacement for `newline-and-indent'.

Continues comments if executed from a commented line, with special support for
languages with weak native comment continuation support (like C-family
languages)."
    (interactive)
    (if (and (sp-point-in-comment)
	     comment-line-break-function)
	(funcall comment-line-break-function)
      (newline nil t)
      (indent-according-to-mode)))
  )

(defmacro def-pairs (pairs)
  `(progn
     ,@(loop for (key . val) in pairs
	     collect
	     `(defun ,(read (concat
			     "wrap-with-"
			     (prin1-to-string key)
			     "s"))
		  (&optional arg)
		(interactive "p")
		(sp-wrap-with-pair ,val)))))

(def-pairs ((paren . "(")
	    (bracket . "[")
	    (brace . "{")
	    (single-quote . "'")
	    (double-quote . "\"")
	    (back-quote . "`")))



(bind-keys
 :map smartparens-mode-map
 ("C-M-a" . sp-beginning-of-sexp)
 ("C-M-e" . sp-end-of-sexp)

 ("C-<down>" . sp-down-sexp)
 ("C-<up>"   . sp-up-sexp)

 ("C-M-f" . sp-forward-sexp)
 ("C-M-b" . sp-backward-sexp)

 ("C-M-n" . sp-next-sexp)
 ("C-M-p" . sp-previous-sexp)

 ("C-S-f" . sp-forward-symbol)
 ("C-S-b" . sp-backward-symbol)

 ("C-<right>" . sp-forward-slurp-sexp)
 ("M-<right>" . sp-forward-barf-sexp)
 ("C-<left>"  . sp-backward-slurp-sexp)
 ("M-<left>"  . sp-backward-barf-sexp)

 ("C-k"   . sp-kill-hybrid-sexp)
 ("M-k"   . sp-backward-kill-sexp)
 ("C-M-w" . sp-copy-sexp)

 ("M-[" . sp-backward-unwrap-sexp)
 ("M-]" . sp-unwrap-sexp)
 ("M-S-]" . sp-rewrap-sexp)

 ("C-x C-t" . sp-transpose-hybrid-sexp)

 ("C-c ("  . wrap-with-parens)
 ("C-c ["  . wrap-with-brackets)
 ("C-c {"  . wrap-with-braces)
 ("C-c '"  . wrap-with-single-quotes)
 ("C-c \"" . wrap-with-double-quotes)
 ("C-c _"  . wrap-with-underscores)
 ("C-c `"  . wrap-with-back-quotes))




(provide 'init-smartparens)
;;; init-smartparens.el ends here
