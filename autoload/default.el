;; config/default/autoload/default.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +default/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let* ((filename (or buffer-file-name (bound-and-true-p list-buffers-directory))))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

;;;###autoload
(defun +default/browse-project ()
  (interactive) (sea-project-browse (sea-project-root)))
;; NOTE No need for find-in-project, use `projectile-find-file'

;;;###autoload
(defun +default/browse-templates ()
  (interactive) (sea-project-browse +file-templates-dir))
;;;###autoload
(defun +default/find-in-templates ()
  (interactive) (sea-project-find-file +file-templates-dir))

;;;###autoload
(defun +default/browse-emacsd ()
  (interactive) (sea-project-browse sea-emacs-dir))
;;;###autoload
(defun +default/find-in-emacsd ()
  (interactive) (sea-project-find-file sea-emacs-dir))

;;;###autoload
(defun +default/browse-notes ()
  (interactive) (sea-project-browse org-directory))
;;;###autoload
(defun +default/find-in-notes ()
  (interactive) (sea-project-find-file org-directory))

;;;###autoload
(defun +default/browse-snippets ()
  (interactive) (sea-project-browse +snippets-dir))
;;;###autoload
(defun +default/find-in-snippets ()
  (interactive) (sea-project-find-file +snippets-dir))

;;;###autoload
(defun +default/find-in-config ()
  "Open a file somewhere in `sea-private-dir' via a fuzzy filename search."
  (interactive)
  (sea-project-find-file sea-private-dir))

;;;###autoload
(defun +default/browse-config ()
  "Browse the files in `sea-private-dir'."
  (interactive)
  (sea-project-browse sea-private-dir))

;;;###autoload
(defun +default/compile (arg)
  "Runs `compile' from the root of the current project.

If a compilation window is already open, recompile that instead.

If ARG (universal argument), runs `compile' from the current directory."
  (interactive "P")
  (if (and (bound-and-true-p compilation-in-progress)
           (buffer-live-p compilation-last-buffer))
      (recompile)
    (call-interactively
     (if arg
         #'projectile-compile-project
       #'compile))))

;;;###autoload
(defun +default/man-or-woman ()
  "Invoke `man' if man is installed, otherwise use `woman'."
  (interactive)
  (call-interactively
   (if (executable-find "man")
       #'man
     #'woman)))

;;;###autoload
(defalias '+default/newline #'newline)

;;;###autoload
(defun +default/newline-above ()
  "Insert an indented new line before the current one."
  (interactive)
  (if (featurep 'evil)
      (call-interactively 'evil-open-above)
    (beginning-of-line)
    (save-excursion (newline))
    (indent-according-to-mode)))

;;;###autoload
(defun +default/newline-below ()
  "Insert an indented new line after the current one."
  (interactive)
  (if (featurep 'evil)
      (call-interactively 'evil-open-below)
    (end-of-line)
    (newline-and-indent)))
