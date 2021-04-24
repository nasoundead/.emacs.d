;; Add your custom functions here
(eval-when-compile (require 'cl))
(require 'subr-x)
(eval-and-compile
  (when (version< emacs-version "26")
    (with-no-warnings
      (defalias 'if-let* #'if-let)
      (defalias 'when-let* #'when-let))))
(defun sea-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (if (listp exp) exp (list exp)))


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


(provide 'init-funcs)
