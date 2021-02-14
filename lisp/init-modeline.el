;; init-modeline.el --- modeline.	-*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;
;; Modeline
;;
;; (use-package nyan-mode)
;; (add-hook 'sea-init-ui-hook #'nyan-mode)
(setq-default mode-line-format
              (list
               ;; ⚿ for locked buffer. ⛯ for modified buffer. ⛆ is the normal one.
               '((:eval
                  (cond
                   (buffer-read-only
                    (propertize " ⚿ " 'face '(:foreground "red" :weight 'bold)))
                   ((buffer-modified-p)
                    (propertize " ⛯ " 'face '(:foreground "orange")))
                   ((not (buffer-modified-p))
                    (propertize " ⛆ " 'face '(:foreground "gray85"))))))
               ;; Use all-the-icons to display the icon of current major mode
               '(:eval (propertize (all-the-icons-icon-for-mode major-mode
                                                                :height (/ all-the-icons-scale-factor 1.4)
                                                                :v-adjust -0.03)))

               ;; the buffer name; the file name as a tool tip
               ;; '(:eval (buffer-file-name-truncate t))
               '(:eval (propertize " %b "
                                   'face
                                   (let ((face (buffer-modified-p)))
                                     (if face 'font-lock-warning-face
                                       'font-lock-type-face))
                                   'help-echo (buffer-file-name)))

               ;; vcs
               " "
               '(:eval (propertize (substring vc-mode 5)
                                   'face 'font-lock-comment-face))


               ;; line and column
               " ("
               (propertize "%02l" 'face 'font-lock-keyword-face) ","
               (propertize "%02c" 'face 'font-lock-keyword-face)
               ") "

               ;; selected character numbers
               '(:eval (propertize (number-to-string (abs (- (point) (mark))))))

               ;; relative position, size of file
               " ["
               (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
               "/"
               (propertize "%I" 'face 'font-lock-constant-face) ;; size
               "] "

               ;; nyan-mode
               ;; '(:eval (list (nyan-create)))

               ;; flycheck
               '(:eval (custom-modeline-flycheck-status))

               ;; spaces to align right
               '(:eval (propertize
                        " " 'display
                        `((space :align-to (- (+ right right-fringe right-margin)
                                              ,(+ 6 (string-width (let ((sys (coding-system-plist buffer-file-coding-system)))
                                                                    (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                                                                           "UTF-8")
                                                                          (t (upcase (symbol-name (plist-get sys :name))))))))
                                              ,(+ 3 (string-width mode-name)))))))

               ;; misc
               mode-line-misc-info

               ;; "Displays the encoding and eol style of the buffer the same way Atom does."
               '(:eval
                 (propertize
                  (concat (pcase (coding-system-eol-type buffer-file-coding-system)
                            (0 "  LF ")
                            (1 "CRLF ")
                            (2 "  CR "))
                          (let ((sys (coding-system-plist buffer-file-coding-system)))
                            (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                                   "UTF-8")
                                  (t (upcase (symbol-name (plist-get sys :name))))))
                          " ")))

               ;; the current major mode
               '(:eval
                 (propertize
                  (concat (format-mode-line mode-name)
                          (when (stringp mode-line-process)
                            mode-line-process)
                          (and (featurep 'face-remap)
                               (/= text-scale-mode-amount 0)
                               (format " %+d" text-scale-mode-amount)))
                  'face 'font-lock-string-face))
               ;;minor-mode-alist
               ))



(defun buffer-file-name-truncate (&optional truncate-tail)
  "Propertized `buffer-file-name' that truncates every dir along path.
If TRUNCATE-TAIL is t also truncate the parent directory of the file."
  (let ((dirs (shrink-path-prompt (file-name-directory buffer-file-truename))))
    (if (null dirs)
        (propertize "%b"
                    'face (if (buffer-modified-p) 'font-lock-warning-face 'font-lock-type-face)
                    'help-echo (buffer-file-name))
      (let ((modified-faces (if (buffer-modified-p) 'font-lock-warning-face)))
        (let ((dirname (car dirs))
              (basename (cdr dirs))
              (dir-faces (or modified-faces 'font-lock-type-face))
              (file-faces (or modified-faces 'font-lock-type-face)))
          (concat (propertize (concat dirname
                                      (if truncate-tail (substring basename 0 1) basename)
                                      "/")
                              'face (if dir-faces `(:inherit ,dir-faces)))
                  (propertize (file-name-nondirectory buffer-file-name)
                              'face (if file-faces `(:inherit ,file-faces))
                              'help-echo (buffer-file-name))))))))

(defun custom-modeline-flycheck-status ()
  "Custom status for flycheck with icons."
  (let* ((text (pcase flycheck-last-status-change
                 (`finished (if flycheck-current-errors
                                (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                               (+ (or .warning 0) (or .error 0)))))
                                  (format "%s %s" (insert-icon 'all-the-icons-faicon "bug") count))
                              (format "%s" (insert-icon 'all-the-icons-faicon "check"))))
                 (`running  (format "%s Running" (insert-icon 'all-the-icons-faicon "spinner" -0.15)))
                 (`no-checker  (format "%s No Checker" (insert-icon 'all-the-icons-material "warning" -0.15)))
                 (`not-checked "")
                 (`errored     (format "%s Error" (insert-icon 'all-the-icons-material "warning" -0.15)))
                 (`interrupted (format "%s Interrupted" (insert-icon 'all-the-icons-faicon "stop" -0.15)))
                 (`suspicious  ""))))
    (propertize text
                'help-echo "Show Flycheck Errors"
                'mouse-face '(:box 1)
                'local-map (make-mode-line-mouse-map
                            'mouse-1 (lambda () (interactive) (flycheck-list-errors))))))


;; The mode line segment shows current python executable
;; hover text is the full path
;; clicking it opens the customize panel for `python-shell-interpreter'

(defvar moon-python-mode-line-map (let ((map (make-sparse-keymap)))
                                    (define-key map (vector 'mode-line 'down-mouse-1)
                                      (lambda ()
                                        (interactive)
                                        (customize-apropos "python-shell-interpreter")))
                                    map))

(defun moon-python-exec-mode-line ()
  "Return a mode line segment for python executable."
  (propertize (file-name-base python-shell-interpreter)
              'help-echo (executable-find python-shell-interpreter)
              'keymap moon-python-mode-line-map))

(add-to-list 'mode-line-misc-info
             '(:eval (if (eq major-mode 'python-mode)
                         (list "  " (moon-python-exec-mode-line) "  "))
                     ""))

(provide 'init-modeline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-modeline.el ends here
