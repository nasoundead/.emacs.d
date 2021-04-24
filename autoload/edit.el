;; Dos2Unix/Unix2Dos
;;;###autoload
(defun sea/dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

;;;###autoload
(defun sea/unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

;; Save a file as utf-8
;;;###autoload
(defun sea/save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))


;; ;;;###autoload
;; (defun sea/backward-to-bol-or-indent ()
;;   "Move back to the current line's indentation. If already there, move to the
;; beginning of the line instead. If at bol, do nothing."
;;   (interactive)
;;   (if (bound-and-true-p visual-line-mode)
;;       (beginning-of-visual-line)
;;     (let ((ci (current-indentation))
;;           (cc (current-column)))
;;       (cond ((or (> cc ci) (= cc 0))
;;              (back-to-indentation))
;;             ((<= cc ci)
;;              (beginning-of-visual-line))))))

;; ;;;###autoload
;; (defun sea/forward-to-last-non-comment-or-eol ()
;;   "Move forward to the last non-blank character in the line, ignoring comments
;; and trailing whitespace. If already there, move to the real end of the line.
;; If already there, do nothing."
;;   (interactive)
;;   (let* ((point (point))
;;          (eol (save-excursion (end-of-visual-line) (point)))
;;          (bol (save-excursion (beginning-of-visual-line) (point)))
;;          (eoc (or (if (not comment-use-syntax)
;;                       (when (re-search-forward comment-start-skip eol t)
;;                         (or (match-end 1) (match-beginning 0)))
;;                     (save-excursion
;;                       (goto-char eol)
;;                       (while (and (sp-point-in-comment)
;;                                   (> (point) point))
;;                         (backward-char))
;;                       (when (> (point) point)
;;                         (skip-chars-backward " " bol)
;;                         (point))))
;;                   eol))
;;          (goto-char-fn (if (featurep 'evil) #'evil-goto-char #'goto-char)))
;;     (if (= eoc point)
;;         (funcall goto-char-fn eol)
;;       (unless (= eol point)
;;         (funcall goto-char-fn eoc)))))

(defun sea--surrounded-p ()
  (and (looking-back "[[{(]\\(\s+\\|\n\\)?\\(\s\\|\t\\)*" (line-beginning-position))
       (let* ((whitespace (match-string 1))
              (match-str (concat whitespace (match-string 2) "[])}]")))
         (looking-at-p match-str))))

;;;###autoload
(defun sea/dumb-indent ()
  "Inserts a tab character (or spaces x tab-width)."
  (interactive)
  (if indent-tabs-mode
      (insert "\t")
    (let* ((movement (% (current-column) tab-width))
           (spaces (if (= 0 movement) tab-width (- tab-width movement))))
      (insert (make-string spaces ? )))))

;;;###autoload
(defun sea/dumb-dedent ()
  "Dedents the current line."
  (interactive)
  (if indent-tabs-mode
      (call-interactively #'backward-delete-char)
    (unless (bolp)
      (save-excursion
        (when (> (current-column) (current-indentation))
          (back-to-indentation))
        (let ((movement (% (current-column) tab-width)))
          (delete-char
           (- (if (= 0 movement)
                  tab-width
                (- tab-width movement)))))))))

;; ;;;###autoload
;; (defun sea/backward-kill-to-bol-and-indent ()
;;   "Kill line to the first non-blank character. If invoked again
;; afterwards, kill line to column 1."
;;   (interactive)
;;   (let ((empty-line-p (save-excursion (beginning-of-line)
;;                                       (looking-at-p "[ \t]*$"))))
;;     (funcall (if (featurep 'evil)
;;                  #'evil-delete
;;                #'delete-region)
;;              (point-at-bol) (point))
;;     (unless empty-line-p
;;       (indent-according-to-mode))))

;; ;;;###autoload
;; (defun sea/backward-delete-whitespace-to-column ()
;;   "Delete back to the previous column of whitespace, or as much whitespace as
;; possible, or just one char if that's not possible."
;;   (interactive)
;;   (let* ((delete-backward-char (if (derived-mode-p 'org-mode)
;;                                    #'org-delete-backward-char
;;                                  #'delete-backward-char))
;;          (context (sp--get-pair-list-context 'navigate))
;;          (open-pair-re (sp--get-opening-regexp context))
;;          (close-pair-re (sp--get-closing-regexp context))
;;          open-len close-len)
;;     (cond ;; When in strings (sp acts weird with quotes; this is the fix)
;;           ;; Also, skip closing delimiters
;;           ((and (and (sp--looking-back open-pair-re)
;;                      (setq open-len (- (match-beginning 0) (match-end 0))))
;;                 (and (looking-at close-pair-re)
;;                      (setq close-len (- (match-beginning 0) (match-end 0))))
;;                 (string= (plist-get (sp-get-thing t) :op)
;;                          (plist-get (sp-get-thing) :cl)))
;;            (delete-char (- 0 open-len))
;;            (delete-char close-len))

;;           ;; Delete up to the nearest tab column IF only whitespace between
;;           ;; point and bol.
;;           ((save-match-data (looking-back "^[\\t ]*" (line-beginning-position)))
;;            (let ((movement (% (current-column) tab-width))
;;                  (p (point)))
;;              (when (= movement 0)
;;                (setq movement tab-width))
;;              (save-match-data
;;                (if (string-match "\\w*\\(\\s-+\\)$"
;;                                  (buffer-substring-no-properties (max (point-min) (- p movement)) p))
;;                    (sp-delete-char
;;                     (- 0 (- (match-end 1)
;;                             (match-beginning 1))))
;;                  (call-interactively delete-backward-char)))))

;;           ;; Otherwise do a regular delete
;;           (t (call-interactively delete-backward-char)))))

;; ;;;###autoload
;; (defun sea/inflate-space-maybe ()
;;   "Checks if point is surrounded by {} [] () delimiters and adds a
;; space on either side of the point if so."
;;   (interactive)
;;   (let ((command (or (command-remapping #'self-insert-command)
;;                      #'self-insert-command)))
;;     (cond ((sea--surrounded-p)
;;            (call-interactively command)
;;            (save-excursion (call-interactively command)))
;;           (t
;;            (call-interactively command)))))


;; ;;;###autoload
;; (defun sea/deflate-space-maybe ()
;;   "Checks if point is surrounded by {} [] () delimiters, and deletes
;; spaces on either side of the point if so. Resorts to
;; `sea/backward-delete-whitespace-to-column' otherwise."
;;   (interactive)
;;   (save-match-data
;;     (if (sea--surrounded-p)
;;         (let ((whitespace-match (match-string 1)))
;;           (cond ((not whitespace-match)
;;                  (call-interactively #'delete-backward-char))
;;                 ((string-match "\n" whitespace-match)
;;                  (funcall (if (featurep 'evil)
;;                               #'evil-delete
;;                             #'delete-region)
;;                           (point-at-bol) (point))
;;                  (call-interactively #'delete-backward-char)
;;                  (save-excursion (call-interactively #'delete-char)))
;;                 (t (just-one-space 0))))
;;       (sea/backward-delete-whitespace-to-column))))

;; ;;;###autoload
;; (defun sea/newline-and-indent ()
;;   "Inserts a newline and possibly indents it. Also continues comments if
;; executed from a commented line; handling special cases for certain languages
;; with weak native support."
;;   (interactive)
;;   (cond ((sp-point-in-string)
;;          (newline))
;;         ((sp-point-in-comment)
;;          (pcase major-mode
;;            ((or 'js2-mode 'rjsx-mode)
;;             (call-interactively #'js2-line-break))
;;            ((or 'java-mode 'php-mode)
;;             (c-indent-new-comment-line))
;;            ((or 'c-mode 'c++-mode 'objc-mode 'css-mode 'scss-mode 'js2-mode)
;;             (newline-and-indent)
;;             (insert "* ")
;;             (indent-according-to-mode))
;;            (_
;;             ;; Fix an off-by-one cursor-positioning issue
;;             ;; with `indent-new-comment-line'
;;             (let ((col (save-excursion (comment-beginning) (current-column))))
;;               (indent-new-comment-line)
;;               (unless (= col (current-column))
;;                 (insert " "))))))
;;         (t
;;          (newline nil t)
;;          (indent-according-to-mode))))

;;;###autoload
(defun +default*newline-indent-and-continue-comments ()
  "A replacement for `newline-and-indent'.

Continues comments if executed from a commented line, with special support for
languages with weak native comment continuation support (like C-family
languages)."
  (interactive)
  (if (and (sp-point-in-comment)
           comment-line-break-function)
      (funcall comment-line-break-function)
    (delete-horizontal-space t)
    (newline nil t)
    (indent-according-to-mode)))

(defun sea--backward-delete-whitespace-to-column ()
  "Delete back to the previous column of whitespace, or as much whitespace as
possible, or just one char if that's not possible."
  (interactive)
  (let* ((context (ignore-errors (sp-get-thing)))
         (op (plist-get context :op))
         (cl (plist-get context :cl))
         open-len close-len)
    (cond ;; When in strings (sp acts weird with quotes; this is the fix)
          ;; Also, skip closing delimiters
          ((and op cl
                (string= op cl)
                (and (string= (char-to-string (or (char-before) 0)) op)
                     (setq open-len (length op)))
                (and (string= (char-to-string (or (char-after) 0)) cl)
                     (setq close-len (length cl))))
           (delete-char (- open-len))
           (delete-char close-len))

          ;; Delete up to the nearest tab column IF only whitespace between
          ;; point and bol.
          ((and (not indent-tabs-mode)
                (not (bolp))
                (not (sp-point-in-string))
                (save-excursion (>= (- (skip-chars-backward " \t")) tab-width)))
           (let ((movement (% (current-column) tab-width)))
             (when (= movement 0)
               (setq movement tab-width))
             (delete-char (- movement)))
           (unless (memq (char-before) (list ?\n ?\ ))
             (insert " ")))

          ;; Otherwise do a regular delete
          ((delete-char -1)))))

;;;###autoload
(defun +default*delete-backward-char (n &optional killflag)
  "Same as `delete-backward-char', but preforms these additional checks:

+ If point is surrounded by (balanced) whitespace and a brace delimiter ({} []
  ()), delete a space on either side of the cursor.
+ If point is at BOL and surrounded by braces on adjacent lines, collapse
  newlines:
  {
  |
  } => {|}
+ Otherwise, resort to `sea--backward-delete-whitespace-to-column'.
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
                (sea--backward-delete-whitespace-to-column))
               ((let* ((pair (ignore-errors (sp-get-thing)))
                       (op   (plist-get pair :op))
                       (cl   (plist-get pair :cl))
                       (beg  (plist-get pair :beg))
                       (end  (plist-get pair :end)))
                  (cond ((and end beg (= end (+ beg (length op) (length cl))))
                         (sp-backward-delete-char 1))
                        ((sea-surrounded-p pair 'inline 'balanced)
                         (delete-char -1 killflag)
                         (delete-char 1)
                         (when (= (point) (+ (length cl) beg))
                           (sp-backward-delete-char 1)
                           (sp-insert-pair op)))
                        ((and (bolp) (sea-surrounded-p pair nil 'balanced))
                         (delete-region beg end)
                         (sp-insert-pair op)
                         t)
                        ((run-hook-with-args-until-success 'sea-delete-backward-functions))
                        ((sea--backward-delete-whitespace-to-column)))))))
        ;; Otherwise, do simple deletion.
        ((delete-char (- n) killflag))))
