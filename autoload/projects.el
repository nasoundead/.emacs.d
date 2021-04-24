;;; core/autoload/projects.el -*- lexical-binding: t; -*-

;;
;; Macros

;;;###autoload
(defmacro without-project-cache! (&rest body)
  "Run BODY with projectile's project-root cache disabled. This is necessary if
you want to interactive with a project other than the one you're in."
  `(let (projectile-project-name
         projectile-require-project-root
         projectile-cached-buffer-file-name
         projectile-cached-project-root)
     ,@body))

;;;###autoload
(defmacro project-file-exists-p! (files)
  "Checks if the project has the specified FILES.
Paths are relative to the project root, unless they start with ./ or ../ (in
which case they're relative to `default-directory'). If they start with a slash,
they are absolute."
  `(file-exists-p! ,files (sea-project-root)))


;;
;; Commands

;;;###autoload
(defun sea/reload-project ()
  "Reload the project root cache."
  (interactive)
  (projectile-invalidate-cache nil)
  (projectile-reset-cached-project-root)
  (dolist (fn projectile-project-root-files-functions)
    (remhash (format "%s-%s" fn default-directory) projectile-project-root-cache)))


;;
;; Library

;;;###autoload
(defun sea-project-p (&optional nocache)
  "Return t if this buffer is currently in a project.
If NOCACHE, don't fetch a cached answer."
  (if nocache
      (without-project-cache! (sea-project-p nil))
    (let ((projectile-require-project-root t))
      (and (projectile-project-p) t))))

;;;###autoload
(defun sea-project-name (&optional nocache)
  "Return the name of the current project.
If NOCACHE, don't fetch a cached answer."
  (if nocache
      (without-project-cache! (sea-project-name nil))
    (projectile-project-name)))

;;;###autoload
(defun sea-project-root (&optional nocache)
  "Returns the root of your project, or `default-directory' if none was found.
If NOCACHE, don't fetch a cached answer."
  (if nocache
      (without-project-cache! (sea-project-root nil))
    (let (projectile-require-project-root)
      (projectile-project-root))))

;;;###autoload
(defalias 'sea-project-expand #'projectile-expand-root)

;;;###autoload
(defun sea-project-find-file (dir)
  "Fuzzy-find a file under DIR."
  (without-project-cache!
   (let* ((default-directory (file-truename dir))
          (projectile-project-root default-directory))
     (call-interactively
      ;; completion modules may remap this command
      (or (command-remapping #'projectile-find-file)
          #'projectile-find-file)))))

;;;###autoload
(defun sea-project-browse (dir)
  "Traverse a file structure starting linearly from DIR."
  (let ((default-directory (file-truename dir)))
    (call-interactively
     ;; completion modules may remap this command
     (or (command-remapping #'find-file)
         #'find-file))))
