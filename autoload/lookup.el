;;; tools/lookup/autoload/lookup.el -*- lexical-binding: t; -*-

;;;###autodef
(cl-defun set-lookup-handlers!
    (modes &rest plist &key definition references documentation file xref-backend async)
  "Define jump handlers for major or minor MODES.

A handler is either an interactive command that changes the current buffer
and/or location of the cursor, or a function that takes one argument: the
identifier being looked up, and returns either nil (failed to find it), t
(succeeded at changing the buffer/moving the cursor), or 'deferred (assume this
handler has succeeded, but expect changes not to be visible yet).

There are several kinds of handlers, which can be defined with the following
properties:

:definition FN
  Run when jumping to a symbol's definition. Used by `+lookup/definition'.
:references FN
  Run when looking for usage references of a symbol in the current project. Used
  by `+lookup/references'.
:documentation FN
  Run when looking up documentation for a symbol. Used by
  `+lookup/documentation'.
:file FN
  Run when looking up the file for a symbol/string. Typically a file path. Used
  by `+lookup/file'.
:xref-backend FN
  Defines an xref backend for a major-mode. A :definition and :references
  handler isn't necessary with a :xref-backend, but will have higher precedence
  if they exist.
:async BOOL
  Indicates that *all* supplied FNs are asynchronous. Note: lookups will not try
  any handlers after async ones, due to their nature. To get around this, you
  must write a specialized wrapper to await the async response, or use a
  different heuristic to determine, ahead of time, whether the async call will
  succeed or not.

  If you only want to specify one FN is async, declare it inline instead:

    (set-lookup-handlers! 'rust-mode
      :definition '(racer-find-definition :async t))

Handlers can either be interactive or non-interactive. Non-interactive handlers
must take one argument: the identifier being looked up. This function must
change the current buffer or window or return non-nil when it succeeds.

If it doesn't change the current buffer, or it returns nil, the lookup module
will fall back to the next handler in `c',
`+lookup-references-functions', `+lookup-file-functions' or
`+lookup-documentation-functions'.

Consecutive `set-lookup-handlers!' calls will overwrite previously defined
handlers for MODES. If used on minor modes, they are stacked onto handlers
defined for other minor modes or the major mode it's activated in.

This can be passed nil as its second argument to unset handlers for MODES. e.g.

  (set-lookup-handlers! 'python-mode nil)"
  (declare (indent defun))
  (dolist (mode (sea-enlist modes))
    (let ((hook (intern (format "%s-hook" mode)))
          (fn   (intern (format "+lookup--init-%s-handlers-h" mode))))
      (cond ((null (car plist))
             (remove-hook hook fn)
             (unintern fn nil))
            ((fset
              fn
              (lambda ()
                (cl-mapc #'+lookup--set-handler
                         (list definition
                               references
                               documentation
                               file
                               xref-backend)
                         (list '+lookup-definition-functions
                               '+lookup-references-functions
                               '+lookup-documentation-functions
                               '+lookup-file-functions
                               'xref-backend-functions)
                         (make-list 5 async)
                         (make-list 5 (or (eq major-mode mode)
                                          (and (boundp mode)
                                               (symbol-value mode)))))))
             (add-hook hook fn))))))





;;
;;; Main commands

;;;###autoload
(defun +lookup/definition (identifier &optional arg)
  "Jump to the definition of IDENTIFIER (defaults to the symbol at point).

Each function in `+lookup-definition-functions' is tried until one changes the
point or current buffer. Falls back to dumb-jump, naive
ripgrep/the_silver_searcher text search, then `evil-goto-definition' if
evil-mode is active."
  (interactive (list (+lookup-symbol-or-region)
                     current-prefix-arg))
  (cond ((null identifier) (user-error "Nothing under point"))
        ((+lookup--jump-to :definition identifier nil arg))
        ((error "Couldn't find the definition of %S" identifier))))

;;;###autoload
(defun +lookup/references (identifier &optional arg)
  "Show a list of usages of IDENTIFIER (defaults to the symbol at point)

Tries each function in `+lookup-references-functions' until one changes the
point and/or current buffer. Falls back to a naive ripgrep/the_silver_searcher
search otherwise."
  (interactive (list (+lookup-symbol-or-region)
                     current-prefix-arg))
  (cond ((null identifier) (user-error "Nothing under point"))
        ((+lookup--jump-to :references identifier nil arg))
        ((error "Couldn't find references of %S" identifier))))

;;;###autoload
(defun +lookup/documentation (identifier &optional arg)
  "Show documentation for IDENTIFIER (defaults to symbol at point or selection.

First attempts the :documentation handler specified with `set-lookup-handlers!'
for the current mode/buffer (if any), then falls back to the backends in
`+lookup-documentation-functions'."
  (interactive (list (+lookup-symbol-or-region)
                     current-prefix-arg))
  (cond ((+lookup--jump-to :documentation identifier #'pop-to-buffer arg))
        ((user-error "Couldn't find documentation for %S" identifier))))

(defvar ffap-file-finder)
;;;###autoload
(defun +lookup/file (path)
  "Figure out PATH from whatever is at point and open it.

Each function in `+lookup-file-functions' is tried until one changes the point
or the current buffer.

Otherwise, falls back on `find-file-at-point'."
  (interactive
   (progn
     (require 'ffap)
     (list
      (or (ffap-guesser)
          (ffap-read-file-or-url
           (if ffap-url-regexp "Find file or URL: " "Find file: ")
           (+lookup-symbol-or-region))))))
  (require 'ffap)
  (cond ((not path)
         (call-interactively #'find-file-at-point))

        ((ffap-url-p path)
         (find-file-at-point path))

        ((not (+lookup--jump-to :file path))
         (let ((fullpath (sea-path path)))
           (when (and buffer-file-name (file-equal-p fullpath buffer-file-name))
             (user-error "Already here"))
           (let* ((insert-default-directory t)
                  (project-root (sea-project-root))
                  (ffap-file-finder
                   (cond ((not (sea-glob fullpath))
                          #'find-file)
                         ((ignore-errors (file-in-directory-p fullpath project-root))
                          (lambda (dir)
                            (let* ((default-directory dir)
                                   projectile-project-name
                                   projectile-project-root
                                   (projectile-project-root-cache (make-hash-table :test 'equal))
                                   (file (projectile-completing-read "Find file: "
                                                                     (projectile-current-project-files)
                                                                     :initial-input path)))
                              (find-file (expand-file-name file (sea-project-root)))
                              (run-hooks 'projectile-find-file-hook))))
                         (#'sea-project-browse))))
             (find-file-at-point path))))))
