;; Add your custom functions here
;; (eval-when-compile (require 'cl))
(require 'subr-x)
(eval-and-compile
  (when (version< emacs-version "26")
    (with-no-warnings
      (defalias 'if-let* #'if-let)
      (defalias 'when-let* #'when-let))))
(defun sea-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (if (listp exp) exp (list exp)))

;; Browse URL
(defun centaur-webkit-browse-url (url &optional pop-buffer new-session)
  "Browse URL with xwidget-webkit' and switch or pop to the buffer.

POP-BUFFER specifies whether to pop to the buffer.
NEW-SESSION specifies whether to create a new xwidget-webkit session."
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "xwidget-webkit URL: ")))
  (or (featurep 'xwidget-internal)
      (user-error "Your Emacs was not compiled with xwidgets support"))
  (xwidget-webkit-browse-url url new-session)
  (let ((buf (xwidget-buffer (and (fboundp 'xwidget-webkit-current-session)
                                  (xwidget-webkit-current-session)))))
    (when (buffer-live-p buf)
      (and (eq buf (current-buffer)) (quit-window))
      (if pop-buffer
          (pop-to-buffer buf)
        (switch-to-buffer buf)))))



(defun icon-displayable-p ()
  "Return non-nil if icons are displayable."
  (and (or (display-graphic-p) (daemonp))
       (or (featurep 'all-the-icons)
           (require 'all-the-icons nil t))))


(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

(defmacro def-setting! (keyword arglist &optional docstring &rest forms)
  "Define a setting. Like `defmacro', this should return a form to be executed
when called with `set!'. FORMS are not evaluated until `set!' calls it.

See `sea/describe-setting' for a list of available settings.

Do not use this for configuring Doom core."
  (declare (indent defun) (doc-string 3))
  (or (keywordp keyword)
      (signal 'wrong-type-argument (list 'keywordp keyword)))
  (unless (stringp docstring)
    (push docstring forms)
    (setq docstring nil))
  (let ((alias (plist-get forms :obsolete)))
    (when alias
      (setq forms (plist-put forms :obsolete 'nil)))
    `(fset ',(intern (format "sea--set%s" keyword))
           (lambda ,arglist
             ,(if (and (not docstring) (fboundp alias))
                  (documentation alias t)
                docstring)
             ,(when alias
                `(declare (obsolete ,alias "2.1.0")))
             (prog1 (progn ,@forms)
               ,(when alias
                  `(unless noninteractive
                     (message ,(format "The `%s' setting is deprecated, use `%s' instead"
                                       keyword alias)))))))))

(defmacro set! (keyword &rest values)
  "Set an option defined by `def-setting!'. Skip if doesn't exist. See
`sea/describe-setting' for a list of available settings.

VALUES doesn't get evaluated if the KEYWORD setting doesn't exist."
  (declare (indent defun))
  (let ((fn (intern-soft (format "sea--set%s" keyword))))
    (if (and fn (fboundp fn))
        (apply fn values)
      (when (or sea-debug-mode after-init-time)
        (message "No setting found for %s" keyword)
        nil))))


(defun open-directory-externally (file)
  "Open FILE externally using the default application of the system."
  (interactive "fOpen externally: ")
  (if (and (eq system-type 'windows-nt)
           (fboundp 'w32-shell-execute))
      (shell-command-to-string
       (encode-coding-string
        (replace-regexp-in-string
         "/" "\\\\"
         (format "explorer.exe %s" (file-name-directory (expand-file-name file)))) 'gbk))
    (call-process (pcase system-type
                    ('darwin "open")
                    ('cygwin "cygstart")
                    (_ "xdg-open"))
                  nil 0 nil
                  (file-name-directory (expand-file-name file)))))
;; (define-key embark-file-map (kbd "E") #'consult-directory-externally)
;;打开.emacs.d目录
(defun sea-open-emacsd-directory ()
  (interactive)
  (open-directory-externally "~/.emacs.d/"))
;; 打开当前文件的目录
(defun sea-open-current-directory ()
  (interactive)
  (open-directory-externally default-directory))



(provide 'init-funcs)
