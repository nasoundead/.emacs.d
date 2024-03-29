;;; core/autoload/buffers.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar sea-real-buffer-functions
  '(sea-dired-buffer-p)
  "A list of predicate functions run to determine if a buffer is real, unlike
`sea-unreal-buffer-functions'. They are passed one argument: the buffer to be
tested.

Should any of its function returns non-nil, the rest of the functions are
ignored and the buffer is considered real.

See `sea-real-buffer-p' for more information.")

;;;###autoload
(defvar sea-unreal-buffer-functions
  '(minibufferp sea-special-buffer-p sea-non-file-visiting-buffer-p)
  "A list of predicate functions run to determine if a buffer is *not* real,
unlike `sea-real-buffer-functions'. They are passed one argument: the buffer to
be tested.

Should any of these functions return non-nil, the rest of the functions are
ignored and the buffer is considered unreal.

See `sea-real-buffer-p' for more information.")

;;;###autoload
(defvar-local sea-real-buffer-p nil
  "If non-nil, this buffer should be considered real no matter what. See
`sea-real-buffer-p' for more information.")

;;;###autoload
(defvar sea-fallback-buffer-name "*scratch*"
  "The name of the buffer to fall back to if no other buffers exist (will create
it if it doesn't exist).")


;;
;; Functions

;;;###autoload
(defun sea/rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))


;;;###autoload
(defun sea-buffer-frame-predicate (buf)
  "To be used as the default frame buffer-predicate parameter. Returns nil if
BUF should be skipped over by functions like `next-buffer' and `other-buffer'."
  (or (sea-real-buffer-p buf)
      (eq buf (sea-fallback-buffer))))

;;;###autoload
(defun sea-fallback-buffer ()
  "Returns the fallback buffer, creating it if necessary. By default this is the
scratch buffer. See `sea-fallback-buffer-name' to change this."
  (let (buffer-list-update-hook)
    (get-buffer-create sea-fallback-buffer-name)))

;;;###autoload
(defalias 'sea-buffer-list #'buffer-list)

;;;###autoload
(defun sea-project-buffer-list (&optional project)
  "Return a list of buffers belonging to the specified PROJECT.

If PROJECT is nil, default to the current project.

If no project is active, return all buffers."
  (let ((buffers (sea-buffer-list)))
    (if-let* ((project-root
               (if project (expand-file-name project)
                 (sea-project-root))))
        (cl-loop for buf in buffers
                 if (projectile-project-buffer-p buf project-root)
                 collect buf)
      buffers)))

;;;###autoload
(defun sea-open-projects ()
  "Return a list of projects with open buffers."
  (cl-loop with projects = (make-hash-table :test 'equal :size 8)
           for buffer in (sea-buffer-list)
           if (buffer-live-p buffer)
           if (sea-real-buffer-p buffer)
           if (with-current-buffer buffer (sea-project-root))
           do (puthash (abbreviate-file-name it) t projects)
           finally return (hash-table-keys projects)))

;;;###autoload
(defun sea-dired-buffer-p (buf)
  "Returns non-nil if BUF is a dired buffer."
  (with-current-buffer buf (derived-mode-p 'dired-mode)))

;;;###autoload
(defun sea-special-buffer-p (buf)
  "Returns non-nil if BUF's name starts and ends with an *."
  (equal (substring (buffer-name buf) 0 1) "*"))

;;;###autoload
(defun sea-temp-buffer-p (buf)
  "Returns non-nil if BUF is temporary."
  (equal (substring (buffer-name buf) 0 1) " "))

;;;###autoload
(defun sea-visible-buffer-p (buf)
  "Return non-nil if BUF is visible."
  (get-buffer-window buf))

;;;###autoload
(defun sea-buried-buffer-p (buf)
  "Return non-nil if BUF is not visible."
  (not (sea-visible-buffer-p buf)))

;;;###autoload
(defun sea-non-file-visiting-buffer-p (buf)
  "Returns non-nil if BUF does not have a value for `buffer-file-name'."
  (not (buffer-file-name buf)))

;;;###autoload
(defun sea-real-buffer-list (&optional buffer-list)
  "Return a list of buffers that satify `sea-real-buffer-p'."
  (cl-remove-if-not #'sea-real-buffer-p (or buffer-list (sea-buffer-list))))

;;;###autoload
(defun sea-real-buffer-p (buffer-or-name)
  "Returns t if BUFFER-OR-NAME is a 'real' buffer.

A real buffer is a useful buffer; a first class citizen in sea. Real ones
should get special treatment, because we will be spending most of our time in
them. Unreal ones should be low-profile and easy to cast aside, so we can focus
on real ones.

The exact criteria for a real buffer is:

  1. A non-nil value for the buffer-local value of the `sea-real-buffer-p'
     variable OR
  2. Any function in `sea-real-buffer-functions' returns non-nil OR
  3. None of the functions in `sea-unreal-buffer-functions' must return
     non-nil.

If BUFFER-OR-NAME is omitted or nil, the current buffer is tested."
  (or (bufferp buffer-or-name)
      (stringp buffer-or-name)
      (signal 'wrong-type-argument (list '(bufferp stringp) buffer-or-name)))
  (when-let (buf (get-buffer buffer-or-name))
    (and (buffer-live-p buf)
         (not (sea-temp-buffer-p buf))
         (or (buffer-local-value 'sea-real-buffer-p buf)
             (run-hook-with-args-until-success 'sea-real-buffer-functions buf)
             (not (run-hook-with-args-until-success 'sea-unreal-buffer-functions buf))))))

;;;###autoload
(defun sea-unreal-buffer-p (buffer-or-name)
  "Return t if BUFFER-OR-NAME is an 'unreal' buffer.

See `sea-real-buffer-p' for details on what that means."
  (not (sea-real-buffer-p buffer-or-name)))

;;;###autoload
(defun sea-buffers-in-mode (modes &optional buffer-list derived-p)
  "Return a list of buffers whose `major-mode' is `eq' to MODE(S).

If DERIVED-P, test with `derived-mode-p', otherwise use `eq'."
  (let ((modes (sea-enlist modes)))
    (cl-remove-if-not (if derived-p
                          (lambda (buf)
                            (with-current-buffer buf
                              (apply #'derived-mode-p modes)))
                        (lambda (buf)
                          (memq (buffer-local-value 'major-mode buf) modes)))
                      (or buffer-list (sea-buffer-list)))))

;;;###autoload
(defun sea-visible-windows (&optional window-list)
  "Return a list of the visible, non-popup (dedicated) windows."
  (cl-loop for window in (or window-list (window-list))
           when (or (window-parameter window 'visible)
                    (not (window-dedicated-p window)))
           collect window))

;;;###autoload
(defun sea-visible-buffers (&optional buffer-list)
  "Return a list of visible buffers (i.e. not buried)."
  (if buffer-list
      (cl-remove-if-not #'get-buffer-window buffer-list)
    (delete-dups (mapcar #'window-buffer (window-list)))))

;;;###autoload
(defun sea-buried-buffers (&optional buffer-list)
  "Get a list of buffers that are buried."
  (cl-remove-if #'get-buffer-window (or buffer-list (sea-buffer-list))))

;;;###autoload
(defun sea-matching-buffers (pattern &optional buffer-list)
  "Get a list of all buffers that match the regex PATTERN."
  (cl-loop for buf in (or buffer-list (sea-buffer-list))
           when (string-match-p pattern (buffer-name buf))
           collect buf))

;;;###autoload
(defun sea-set-buffer-real (buffer flag)
  "Forcibly mark BUFFER as FLAG (non-nil = real)."
  (with-current-buffer buffer
    (setq sea-real-buffer-p flag)))

;;;###autoload
(defun sea-kill-buffer-and-windows (buffer)
  "Kill the buffer and delete all the windows it's displayed in."
  (dolist (window (get-buffer-window-list buffer))
    (unless (one-window-p t)
      (delete-window window)))
  (kill-buffer buffer))

;;;###autoload
(defun sea-fixup-windows (windows)
  "Ensure that each of WINDOWS is showing a real buffer or the fallback buffer."
  (dolist (window windows)
    (with-selected-window window
      (when (sea-unreal-buffer-p (window-buffer))
        (previous-buffer)
        (when (sea-unreal-buffer-p (window-buffer))
          (switch-to-buffer (sea-fallback-buffer)))))))

;;;###autoload
(defun sea-kill-buffer-fixup-windows (buffer)
  "Kill the BUFFER and ensure all the windows it was displayed in have switched
to a real buffer or the fallback buffer."
  (let ((windows (get-buffer-window-list buffer)))
    (kill-buffer buffer)
    (sea-fixup-windows (cl-remove-if-not #'window-live-p windows))))

;;;###autoload
(defun sea-kill-buffers-fixup-windows (buffers)
  "Kill the BUFFERS and ensure all the windows they were displayed in have
switched to a real buffer or the fallback buffer."
  (let ((seen-windows (make-hash-table :test 'eq :size 8)))
    (dolist (buffer buffers)
      (let ((windows (get-buffer-window-list buffer)))
        (kill-buffer buffer)
        (dolist (window (cl-remove-if-not #'window-live-p windows))
          (puthash window t seen-windows))))
    (sea-fixup-windows (hash-table-keys seen-windows))))

;;;###autoload
(defun sea-kill-matching-buffers (pattern &optional buffer-list)
  "Kill all buffers (in current workspace OR in BUFFER-LIST) that match the
regex PATTERN. Returns the number of killed buffers."
  (let ((buffers (sea-matching-buffers pattern buffer-list)))
    (dolist (buf buffers (length buffers))
      (kill-buffer buf))))


;;
;; Hooks

;;;###autoload
(defun sea-mark-buffer-as-real-h ()
  "Hook function that marks the current buffer as real."
  (sea-set-buffer-real (current-buffer) t))


;;
;; Interactive commands

;;;###autoload
(defun sea/kill-this-buffer-in-all-windows (buffer &optional dont-save)
  "Kill BUFFER globally and ensure all windows previously showing this buffer
have switched to a real buffer or the fallback buffer.

If DONT-SAVE, don't prompt to save modified buffers (discarding their changes)."
  (interactive
   (list (current-buffer) current-prefix-arg))
  (cl-assert (bufferp buffer) t)
  (when (and (buffer-modified-p buffer) dont-save)
    (with-current-buffer buffer
      (set-buffer-modified-p nil)))
  (sea-kill-buffer-fixup-windows buffer))


(defun sea--message-or-count (interactive message count)
  (if interactive
      (message message count)
    count))

;;;###autoload
(defun sea/kill-all-buffers (&optional buffer-list interactive)
  "Kill all buffers and closes their windows.

If the prefix arg is passed, doesn't close windows and only kill buffers that
belong to the current project."
  (interactive
   (list (if current-prefix-arg
             (sea-project-buffer-list)
           (sea-buffer-list))
         t))
  (if (null buffer-list)
      (message "No buffers to kill")
    (save-some-buffers)
    (delete-other-windows)
    (when (memq (current-buffer) buffer-list)
      (switch-to-buffer (sea-fallback-buffer)))
    (mapc #'kill-buffer buffer-list)
    (sea--message-or-count
     interactive "Killed %d buffers"
     (- (length buffer-list)
        (length (cl-remove-if-not #'buffer-live-p buffer-list))))))

;;;###autoload
(defun sea/kill-other-buffers (&optional buffer-list interactive)
  "Kill all other buffers (besides the current one).

If the prefix arg is passed, kill only buffers that belong to the current
project."
  (interactive
   (list (delq (current-buffer)
               (if current-prefix-arg
                   (sea-project-buffer-list)
                 (sea-buffer-list)))
         t))
  (mapc #'sea-kill-buffer-and-windows buffer-list)
  (sea--message-or-count
   interactive "Killed %d other buffers"
   (- (length buffer-list)
      (length (cl-remove-if-not #'buffer-live-p buffer-list)))))

;;;###autoload
(defun sea/kill-matching-buffers (pattern &optional buffer-list interactive)
  "Kill buffers that match PATTERN in BUFFER-LIST.

If the prefix arg is passed, only kill matching buffers in the current project."
  (interactive
   (list (read-regexp "Buffer pattern: ")
         (if current-prefix-arg
             (sea-project-buffer-list)
           (sea-buffer-list))
         t))
  (sea-kill-matching-buffers pattern buffer-list)
  (when interactive
    (message "Killed %d buffer(s)"
             (- (length buffer-list)
                (length (cl-remove-if-not #'buffer-live-p buffer-list))))))

;;;###autoload
(defun sea/kill-buried-buffers (&optional buffer-list interactive)
  "Kill buffers that are buried.

If PROJECT-P (universal argument), only kill buried buffers belonging to the
current project."
  (interactive
   (list (sea-buried-buffers
          (if current-prefix-arg (sea-project-buffer-list)))
         t))
  (mapc #'kill-buffer buffer-list)
  (sea--message-or-count
   interactive "Killed %d buried buffers"
   (- (length buffer-list)
      (length (cl-remove-if-not #'buffer-live-p buffer-list)))))

;;;###autoload
(defun sea/kill-project-buffers (project &optional interactive)
  "Kill buffers for the specified PROJECT."
  (interactive
   (list (if-let (open-projects (sea-open-projects))
             (completing-read
              "Kill buffers for project: " open-projects
              nil t nil nil
              (if-let* ((project-root (sea-project-root))
                        (project-root (abbreviate-file-name project-root))
                        ((member project-root open-projects)))
                  project-root))
           (message "No projects are open!")
           nil)
         t))
  (when project
    (let ((buffer-list (sea-project-buffer-list project)))
      (sea-kill-buffers-fixup-windows buffer-list)
      (sea--message-or-count
       interactive "Killed %d project buffers"
       (- (length buffer-list)
          (length (cl-remove-if-not #'buffer-live-p buffer-list)))))))


;;;###autoload
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
