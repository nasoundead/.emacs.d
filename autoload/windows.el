;;;###autoload
(defun sea/split-window-func-with-other-buffer (split-function)
  (lexical-let ((s-f split-function))
    (lambda (&optional arg)
      "Split this window and switch to the new window unless ARG is provided."
      (interactive "P")
      (funcall s-f)
      (let ((target-window (next-window)))
        (set-window-buffer target-window (other-buffer))
        (unless arg
          (select-window target-window))))))
;;;###autoload
(defun sea/split-window-horizontally-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (sea/split-window-func-with-other-buffer 'split-window-horizontally))))
;;;###autoload
(defun sea/split-window-vertically-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (sea/split-window-func-with-other-buffer 'split-window-vertically))))

;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
;;;###autoload
(defun sea/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'sea/split-window)
      (progn
        (jump-to-register :sea/split-window)
        (setq this-command 'sea/unsplit-window))
    (window-configuration-to-register :sea/split-window)
    (switch-to-buffer-other-window nil)))

;;;###autoload
(defun sea/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))
