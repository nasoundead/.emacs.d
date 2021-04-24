;;; ui/popup/autoload/popup.el -*- lexical-binding: t; -*-

(defun +popup--remember (windows)
  "Remember WINDOWS (a list of windows) for later restoration."
  (cl-assert (cl-every #'windowp windows) t)
  (setq +popup--last
        (cl-loop for w in windows
                 collect (cons (window-buffer w)
                               (window-state-get w)))))

(defun +popup--kill-buffer (buffer ttl)
  "Tries to kill BUFFER, as was requested by a transient timer. If it fails, eg.
the buffer is visible, then set another timer and try again later."
  (when (buffer-live-p buffer)
    (let ((inhibit-quit t)
          (kill-buffer-hook (remq '+popup|kill-buffer-hook kill-buffer-hook)))
      (cond ((get-buffer-window buffer)
             (with-current-buffer buffer
               (setq +popup--timer
                     (run-at-time ttl nil #'+popup--kill-buffer buffer ttl))))
            ((eq ttl 0)
             (kill-buffer buffer))
            ((with-demoted-errors "Error killing transient buffer: %s"
               (with-current-buffer buffer
                 (let (confirm-kill-processes)
                   (when-let* ((process (get-buffer-process buffer)))
                     (kill-process process))
                   (let (kill-buffer-hook kill-buffer-query-functions)
                     (kill-buffer buffer))))))))))

(defun +popup--delete-window (window)
  "Do housekeeping before destroying a popup window.

+ Disables `+popup-buffer-mode' so that any hooks attached to it get a chance to
  run and do cleanup of its own.
+ Either kills the buffer or sets a transient timer, if the window has a
  `transient' window parameter (see `+popup-window-parameters').
+ And finally deletes the window!"
  (let ((buffer (window-buffer window))
        (inhibit-quit t))
    (and (buffer-file-name buffer)
         (buffer-modified-p buffer)
         (let ((autosave (+popup-parameter 'autosave window)))
           (cond ((eq autosave 't))
                 ((null autosave)
                  (y-or-n-p "Popup buffer is modified. Save it?"))
                 ((functionp autosave)
                  (funcall autosave buffer))))
         (with-current-buffer buffer (save-buffer)))
    (let ((ignore-window-parameters t))
      (if-let* ((wconf (window-parameter window 'saved-wconf)))
          (set-window-configuration wconf)
        (delete-window window)))
    (unless (window-live-p window)
      (with-current-buffer buffer
        (set-buffer-modified-p nil)
        (+popup-buffer-mode -1)
        (unless +popup--inhibit-transient
          (let ((ttl (+popup-parameter 'ttl window)))
            (when (eq ttl 't)
              (setq ttl (plist-get +popup-defaults :ttl)))
            (cond ((null ttl))
                  ((functionp ttl)
                   (funcall ttl buffer))
                  ((not (integerp ttl))
                   (signal 'wrong-type-argument (list 'integerp ttl)))
                  ((= ttl 0)
                   (+popup--kill-buffer buffer 0))
                  ((add-hook 'kill-buffer-hook #'+popup|kill-buffer-hook nil t)
                   (setq +popup--timer
                         (run-at-time ttl nil #'+popup--kill-buffer
                                      buffer ttl))))))))))

(defun +popup--delete-other-windows (window)
  "Called in lieu of `delete-other-windows' in popup windows.

Raises WINDOW (assumed to be a popup), then deletes other windows."
  (when-let* ((window (+popup/raise window)))
    (delete-other-windows window))
  nil)

(defun +popup--normalize-alist (alist)
  "Merge `+popup-default-alist' and `+popup-default-parameters' with ALIST."
  (when alist
    (let ((alist  ; handle defaults
           (cl-remove-duplicates
            (append alist +popup-default-alist)
            :key #'car-safe :from-end t))
          (parameters
           (cl-remove-duplicates
            (append (cdr (assq 'window-parameters alist))
                    +popup-default-parameters)
            :key #'car-safe :from-end t)))
      ;; handle `size'
      (when-let* ((size  (cdr (assq 'size alist)))
                  (side  (or (cdr (assq 'side alist)) 'bottom))
                  (param (if (memq side '(left right))
                             'window-width
                           'window-height)))
        (setq list (assq-delete-all 'size alist))
        (setf (alist-get param alist) size))
      (setf (alist-get 'window-parameters alist)
            parameters)
      alist)))

;;;###autoload
(defun +popup--init (window &optional alist)
  "Initializes a popup window. Run any time a popup is opened. It sets the
default window parameters for popup windows, clears leftover transient timers
and enables `+popup-buffer-mode'."
  (with-selected-window window
    (setq alist (delq (assq 'actions alist) alist))
    (when (and alist +popup--populate-wparams)
      ;; Emacs 26+ will automatically map the window-parameters alist entry to
      ;; the popup window, so we need this for Emacs 25.x users
      (dolist (param (cdr (assq 'window-parameters alist)))
        (set-window-parameter window (car param) (cdr param))))
    (set-window-parameter window 'popup t)
    (set-window-parameter window 'delete-window #'+popup--delete-window)
    (set-window-parameter window 'delete-other-windows #'+popup--delete-other-windows)
    (set-window-dedicated-p window 'popup)
    (window-preserve-size
     window (memq (window-parameter window 'window-side)
                  '(left right))
     t)
    (+popup-buffer-mode +1)
    (run-hooks '+popup-create-window-hook)))


;;
;; Public library

;;;###autoload
(defun +popup-buffer-p (&optional buffer)
  "Return non-nil if BUFFER is a popup buffer. Defaults to the current buffer."
  (when +popup-mode
    (let ((buffer (or buffer (current-buffer))))
      (and (bufferp buffer)
           (buffer-live-p buffer)
           (buffer-local-value '+popup-buffer-mode buffer)
           buffer))))

;;;###autoload
(defun +popup-window-p (&optional window)
  "Return non-nil if WINDOW is a popup window. Defaults to the current window."
  (when +popup-mode
    (let ((window (or window (selected-window))))
      (and (windowp window)
           (window-live-p window)
           (window-parameter window 'popup)
           window))))

;;;###autoload
(defun +popup-buffer (buffer &optional alist)
  "Open BUFFER in a popup window. ALIST describes its features."
  (let* ((origin (selected-window))
         (window-min-height 3)
         (alist (+popup--normalize-alist alist))
         (actions (or (cdr (assq 'actions alist))
                      +popup-default-display-buffer-actions)))
    (when-let* ((popup (cl-loop for func in actions
                                if (funcall func buffer alist)
                                return it)))
      (+popup--init popup alist)
      (unless +popup--inhibit-select
        (let ((select (+popup-parameter 'select popup)))
          (if (functionp select)
              (funcall select popup origin)
            (select-window (if select popup origin)))))
      popup)))

;;;###autoload
(defun +popup-parameter (parameter &optional window)
  "Fetch the window PARAMETER (symbol) of WINDOW"
  (window-parameter (or window (selected-window)) parameter))

;;;###autoload
(defun +popup-parameter-fn (parameter &optional window &rest args)
  "Fetch the window PARAMETER (symbol) of WINDOW. If it is a function, run it
with ARGS to get its return value."
  (let ((val (+popup-parameter parameter window)))
    (if (functionp val)
        (apply val args)
      val)))

;;;###autoload
(defun +popup-windows ()
  "Returns a list of all popup windows."
  (cl-remove-if-not #'+popup-window-p (window-list)))

;;;###autoload
(defun +popup-shrink-to-fit (&optional window)
  "Shrinks WINDOW to fit the buffer contents, if the buffer isn't empty.

Uses `shrink-window-if-larger-than-buffer'."
  (unless window
    (setq window (selected-window)))
  (unless (= (- (point-max) (point-min)) 0)
    (shrink-window-if-larger-than-buffer window)))

;;;###autoload
(defun +popup-alist-from-window-state (state)
  "Convert window STATE (from `window-state-get') to a `display-buffer' alist."
  (let* ((params (alist-get 'parameters state)))
    `((side          . ,(alist-get 'window-side params))
      (window-width  . ,(alist-get 'total-width state))
      (window-height . ,(alist-get 'total-height state))
      (window-parameters ,@params))))


;;
;; Hooks

;;;###autoload
(defun +popup|adjust-fringes ()
  "Hides the fringe in popup windows, restoring them if `+popup-buffer-mode' is
disabled."
  (let ((f (if (bound-and-true-p +popup-buffer-mode) 0)))
    (set-window-fringes nil f f fringes-outside-margins)))

;;;###autoload
(defun +popup|adjust-margins ()
  "Creates padding for the popup window determined by `+popup-margin-width',
restoring it if `+popup-buffer-mode' is disabled."
  (when +popup-margin-width
    (unless (memq (window-parameter nil 'window-side) '(left right))
      (let ((m (if (bound-and-true-p +popup-buffer-mode) +popup-margin-width)))
        (set-window-margins nil m m)))))

(defvar hide-mode-line-format)
;;;###autoload
(defun +popup|set-modeline-on-enable ()
  "Don't show modeline in popup windows without a `modeline' window-parameter.
Possible values for this parameter are:

  t            show the mode-line as normal
  nil          hide the modeline entirely (the default)
  a function   `mode-line-format' is set to its return value

Any non-nil value besides the above will be used as the raw value for
`mode-line-format'."
  (when (bound-and-true-p +popup-buffer-mode)
    (let ((modeline (+popup-parameter 'modeline)))
      (cond ((eq modeline 't))
            ((null modeline)
             ;; TODO use `mode-line-format' window parameter instead (emacs 26+)
             (hide-mode-line-mode +1))
            ((let ((hide-mode-line-format
                    (if (functionp modeline)
                        (funcall modeline)
                      modeline)))
               (hide-mode-line-mode +1)))))))
(put '+popup|set-modeline-on-enable 'permanent-local-hook t)

;;;###autoload
(defun +popup|unset-modeline-on-disable ()
  "Restore the modeline when `+popup-buffer-mode' is deactivated."
  (when (and (not (bound-and-true-p +popup-buffer-mode))
             (bound-and-true-p hide-mode-line-mode))
    (hide-mode-line-mode -1)))

;;;###autoload
(defun +popup|close-on-escape ()
  "If called inside a popup, try to close that popup window (see
`+popup/close'). If called outside, try to close all popup windows (see
`+popup/close-all')."
  (if (+popup-window-p)
      (+popup/close)
    (+popup/close-all)))

;;;###autoload
(defun +popup|cleanup-rules ()
  "Cleans up any duplicate popup rules."
  (interactive)
  (setq +popup--display-buffer-alist
        (cl-delete-duplicates +popup--display-buffer-alist
                              :key #'car :test #'equal :from-end t))
  (when +popup-mode
    (setq display-buffer-alist +popup--display-buffer-alist)))

;;;###autoload
(defun +popup|kill-buffer-hook ()
  "TODO"
  (when-let* ((window (get-buffer-window)))
    (when (+popup-window-p window)
      (let ((+popup--inhibit-transient t))
        (+popup--delete-window window)))))


;;
;; Commands

;;;###autoload
(defalias 'other-popup #'+popup/other)

;;;###autoload
(defun +popup/buffer ()
  "Open this buffer in a popup window."
  (interactive)
  (let ((+popup-default-display-buffer-actions
         '(+popup-display-buffer-stacked-side-window))
        (display-buffer-alist +popup--display-buffer-alist)
        (buffer (current-buffer)))
    (push (+popup--make "." +popup-defaults) display-buffer-alist)
    (bury-buffer)
    (pop-to-buffer buffer)))

;;;###autoload
(defun +popup/other ()
  "Cycle through popup windows, like `other-window'. Ignores regular windows."
  (interactive)
  (let ((popups (+popup-windows))
        (window (selected-window)))
    (unless popups
      (user-error "No popups are open"))
    (select-window (if (+popup-window-p)
                       (or (car-safe (cdr (memq window popups)))
                           (car (delq window popups))
                           (car popups))
                     (car popups)))))

;;;###autoload
(defun +popup/close (&optional window force-p)
  "Close WINDOW, if it's a popup window.

This will do nothing if the popup's `quit' window parameter is either nil or
'other. This window parameter is ignored if FORCE-P is non-nil."
  (interactive
   (list (selected-window)
         current-prefix-arg))
  (let ((window (or window (selected-window))))
    (when (and (+popup-window-p window)
               (or force-p
                   (memq (+popup-parameter-fn 'quit window window)
                         '(t current))))
      (when +popup--remember-last
        (+popup--remember (list window)))
      (delete-window window)
      t)))

;;;###autoload
(defun +popup/close-all (&optional force-p)
  "Close all open popup windows.

This will ignore popups with an `quit' parameter that is either nil or 'current.
This window parameter is ignored if FORCE-P is non-nil."
  (interactive "P")
  (let (targets +popup--remember-last)
    (dolist (window (+popup-windows))
      (when (or force-p
                (memq (+popup-parameter-fn 'quit window window)
                      '(t other)))
        (push window targets)))
    (when targets
      (+popup--remember targets)
      (mapc #'delete-window targets)
      t)))

;;;###autoload
(defun +popup/toggle ()
  "If popups are open, close them. If they aren't, restore the last one or open
the message buffer in a popup window."
  (interactive)
  (let ((+popup--inhibit-transient t))
    (cond ((+popup-windows) (+popup/close-all t))
          ((ignore-errors (+popup/restore)))
          ((display-buffer (get-buffer "*Messages*"))))))

;;;###autoload
(defun +popup/restore ()
  "Restore the last popups that were closed, if any."
  (interactive)
  (unless +popup--last
    (error "No popups to restore"))
  (cl-loop for (buffer . state) in +popup--last
           if (buffer-live-p buffer)
           do (+popup-buffer buffer (+popup-alist-from-window-state state)))
  (setq +popup--last nil)
  t)

;;;###autoload
(defun +popup/raise (window)
  "Raise the current popup window into a regular window."
  (interactive (list (selected-window)))
  (cl-check-type window window)
  (unless (+popup-window-p window)
    (user-error "Cannot raise a non-popup window"))
  (let ((buffer (current-buffer))
        +popup--remember-last)
    (set-window-parameter window 'ttl nil)
    (+popup/close window 'force)
    (display-buffer-pop-up-window buffer nil)))


;;
;; Advice

;;;###autoload
(defun +popup*close (&rest _)
  "TODO"
  (+popup/close nil t))

;;;###autoload
(defun +popup*save (orig-fn &rest args)
  "Sets aside all popups before executing the original function, usually to
prevent the popup(s) from messing up the UI (or vice versa)."
  (save-popups! (apply orig-fn args)))

;;;###autoload
(defun +popup-display-buffer-fullframe (buffer alist)
  "Displays the buffer fullscreen."
  (let ((wconf (current-window-configuration)))
    (when-let (window (or (display-buffer-reuse-window buffer alist)
                          (display-buffer-same-window buffer alist)
                          (display-buffer-pop-up-window buffer alist)
                          (display-buffer-use-some-window buffer alist)))
      (set-window-parameter window 'saved-wconf wconf)
      (add-to-list 'window-persistent-parameters '(saved-wconf . t))
      (delete-other-windows window)
      window)))

;;;###autoload
(defun +popup-display-buffer-stacked-side-window (buffer alist)
  "A `display-buffer' action that serves as an alternative to
`display-buffer-in-side-window', but allows for stacking popups with the `vslot'
alist entry.

Accepts the same arguments as `display-buffer-in-side-window'. You must set
`window--sides-inhibit-check' to non-nil for this work properly."
  (let* ((side  (or (cdr (assq 'side alist)) 'bottom))
         (slot  (or (cdr (assq 'slot alist))  0))
         (vslot (or (cdr (assq 'vslot alist)) 0))
         (left-or-right (memq side '(left right)))
         (dedicated (or display-buffer-mark-dedicated 'popup)))

    (cond ((not (memq side '(top bottom left right)))
           (error "Invalid side %s specified" side))
          ((not (numberp slot))
           (error "Invalid slot %s specified" slot))
          ((not (numberp vslot))
           (error "Invalid vslot %s specified" vslot)))

    (let* ((major (get-window-with-predicate
                   (lambda (window)
                     (and (eq (window-parameter window 'window-side) side)
                          (eq (window-parameter window 'window-vslot) vslot)))
                   nil t))
           (reversed (window--sides-reverse-on-frame-p (selected-frame)))
           (windows
            (cond ((window-live-p major)
                   (list major))
                  ((window-valid-p major)
                   (let* ((first (window-child major))
                          (next (window-next-sibling first))
                          (windows (list next first)))
                     (setq reversed (> (window-parameter first 'window-slot)
                                       (window-parameter next 'window-slot)))
                     (while (setq next (window-next-sibling next))
                       (setq windows (cons next windows)))
                     (if reversed windows (nreverse windows))))))
           (slots (if major (max 1 (window-child-count major))))
           (max-slots
            (nth (plist-get '(left 0 top 1 right 2 bottom 3) side)
                 window-sides-slots))
           (window--sides-inhibit-check t)
           window this-window this-slot prev-window next-window
           best-window best-slot abs-slot)

      (cond ((and (numberp max-slots) (<= max-slots 0))
             nil)
            ((not windows)
             (cl-letf (((symbol-function 'window--make-major-side-window-next-to)
                        (lambda (_side) (frame-root-window (selected-frame)))))
               (when-let* ((window (window--make-major-side-window buffer side slot alist)))
                 (set-window-parameter window 'window-vslot vslot)
                 (add-to-list 'window-persistent-parameters '(window-vslot . writable))
                 window)))
            (t
             ;; Scan windows on SIDE.
             (catch 'found
               (dolist (window windows)
                 (setq this-slot (window-parameter window 'window-slot))
                 (cond ((not (numberp this-slot)))
                       ((= this-slot slot) ; A window with a matching slot found
                        (setq this-window window)
                        (throw 'found t))
                       (t
                        ;; Check if this window has a better slot value wrt the
                        ;; slot of the window we want.
                        (setq abs-slot
                              (if (or (and (> this-slot 0) (> slot 0))
                                      (and (< this-slot 0) (< slot 0)))
                                  (abs (- slot this-slot))
                                (+ (abs slot) (abs this-slot))))
                        (unless (and best-slot (<= best-slot abs-slot))
                          (setq best-window window)
                          (setq best-slot abs-slot))
                        (if reversed
                            (cond
                             ((<= this-slot slot)
                              (setq next-window window))
                             ((not prev-window)
                              (setq prev-window window)))
                          (cond
                           ((<= this-slot slot)
                            (setq prev-window window))
                           ((not next-window)
                            (setq next-window window))))))))

             ;; `this-window' is the first window with the same SLOT.
             ;; `prev-window' is the window with the largest slot < SLOT. A new
             ;; window will be created after it.
             ;; `next-window' is the window with the smallest slot > SLOT. A new
             ;; window will be created before it.
             ;; `best-window' is the window with the smallest absolute
             ;; difference of its slot and SLOT.
             (or (and this-window
                      ;; Reuse `this-window'.
                      (with-current-buffer buffer
                        (setq window--sides-shown t))
                      (window--display-buffer
                       buffer this-window 'reuse alist dedicated))
                 (and (or (not max-slots) (< slots max-slots))
                      (or (and next-window
                               ;; Make new window before `next-window'.
                               (let ((next-side (if left-or-right 'above 'left))
                                     (window-combination-resize 'side))
                                 (setq window
                                       (ignore-errors (split-window next-window nil next-side)))))
                          (and prev-window
                               ;; Make new window after `prev-window'.
                               (let ((prev-side (if left-or-right 'below 'right))
                                     (window-combination-resize 'side))
                                 (setq window
                                       (ignore-errors (split-window prev-window nil prev-side))))))
                      (set-window-parameter window 'window-slot slot)
                      (with-current-buffer buffer
                        (setq window--sides-shown t))
                      (window--display-buffer
                       buffer window 'window alist dedicated))
                 (and best-window
                      ;; Reuse `best-window'.
                      (progn
                        ;; Give best-window the new slot value.
                        (set-window-parameter best-window 'window-slot slot)
                        (with-current-buffer buffer
                          (setq window--sides-shown t))
                        (window--display-buffer
                         buffer best-window 'reuse alist dedicated)))))))))


;;
;; Emacs backwards compatibility

(unless EMACS26+
  (defvar window-sides-reversed nil)

  (defun window--sides-reverse-on-frame-p (frame)
    "Return non-nil when side windows should appear reversed on FRAME.
This uses some heuristics to guess the user's intentions when the
selected window of FRAME is a side window ."
    (cond
     ;; Reverse when `window-sides-reversed' is t.  Do not reverse when
     ;; `window-sides-reversed' is nil.
     ((memq window-sides-reversed '(nil t))
      window-sides-reversed)
     ;; Reverse when FRAME's selected window shows a right-to-left buffer.
     ((let ((window (frame-selected-window frame)))
        (when (and (not (window-parameter window 'window-side))
                   (or (not (window-minibuffer-p window))
                       (setq window (minibuffer-selected-window))))
          (with-current-buffer (window-buffer window)
            (eq bidi-paragraph-direction 'right-to-left)))))
     ;; Reverse when FRAME's `window-sides-main-selected-window' parameter
     ;; specifies a live window showing a right-to-left buffer.
     ((let ((window (frame-parameter
                     frame 'window-sides-main-selected-window)))
        (when (window-live-p window)
          (with-current-buffer (window-buffer window)
            (eq bidi-paragraph-direction 'right-to-left)))))
     ;; Reverse when all windows in FRAME's main window show right-to-left
     ;; buffers.
     (t
      (catch 'found
        (walk-window-subtree
         (lambda (window)
           (with-current-buffer (window-buffer window)
             (when (eq bidi-paragraph-direction 'left-to-right)
               (throw 'found nil))))
         (window-main-window frame))
        t))))

  (defun window--make-major-side-window (buffer side slot &optional alist)
    "Display BUFFER in a new major side window on the selected frame.
SIDE must be one of `left', `top', `right' or `bottom'.  SLOT
specifies the slot to use.  ALIST is an association list of
symbols and values as passed to `display-buffer-in-side-window'.
Return the new window, nil if its creation failed.

This is an auxiliary function of `display-buffer-in-side-window'
and may be called only if no window on SIDE exists yet."
    (let* ((left-or-right (memq side '(left right)))
           (next-to (window--make-major-side-window-next-to side))
           (on-side (cond
                     ((eq side 'top) 'above)
                     ((eq side 'bottom) 'below)
                     (t side)))
           (window--sides-inhibit-check t)
           ;; The following two bindings will tell `split-window' to take
           ;; the space for the new window from the selected frame's main
           ;; window and not make a new parent window unless needed.
           (window-combination-resize 'side)
           (window-combination-limit nil)
           (window (ignore-errors (split-window next-to nil on-side))))
      (when window
        ;; Initialize `window-side' parameter of new window to SIDE and
        ;; make that parameter persistent.
        (set-window-parameter window 'window-side side)
        (add-to-list 'window-persistent-parameters '(window-side . writable))
        ;; Install `window-slot' parameter of new window and make that
        ;; parameter persistent.
        (set-window-parameter window 'window-slot slot)
        (add-to-list 'window-persistent-parameters '(window-slot . writable))
        ;; Auto-adjust height/width of new window unless a size has been
        ;; explicitly requested.
        (unless (if left-or-right
                    (cdr (assq 'window-width alist))
                  (cdr (assq 'window-height alist)))
          (setq alist
                (cons
                 (cons
                  (if left-or-right 'window-width 'window-height)
                  (/ (window-total-size (frame-root-window) left-or-right)
                     ;; By default use a fourth of the size of the frame's
                     ;; root window.
                     4))
                 alist)))
        (with-current-buffer buffer
          (setq window--sides-shown t))
        ;; Install BUFFER in new window and return WINDOW.
        (window--display-buffer buffer window 'window alist 'side))))

  (advice-add #'window--sides-check :override #'ignore))
  
  ;;; ui/popup/autoload/settings.el -*- lexical-binding: t; -*-

(defvar +popup--display-buffer-alist nil)

;;;###autoload
(defvar +popup-defaults
  (list :side   'bottom
        :height 0.16
        :width  40
        :quit   t
        :select #'ignore
        :ttl    5)
  "Default properties for popup rules defined with `set-popup-rule!'.")

;;;###autoload
(defun +popup--make (predicate plist)
  (cond ((and plist (not (keywordp (car plist))))
         ;; FIXME deprecated popup rule support
         (message "Warning: the old usage of `set-popup-rule!' is deprecated; update the rule for '%s'"
                  predicate)
         (cl-destructuring-bind (condition &optional alist parameters)
             (list predicate (car plist) (cadr plist))
           (if (eq alist :ignore)
               (list condition nil)
             `(,condition (+popup-buffer)
                          ,@alist
                          (window-parameters ,@parameters)))))
        ((plist-get plist :ignore)
         (list predicate nil))
        ((let* ((plist (append plist +popup-defaults))
                (alist
                 `((actions       . ,(plist-get plist :actions))
                   (side          . ,(plist-get plist :side))
                   (size          . ,(plist-get plist :size))
                   (window-width  . ,(plist-get plist :width))
                   (window-height . ,(plist-get plist :height))
                   (slot          . ,(plist-get plist :slot))
                   (vslot         . ,(plist-get plist :vslot))))
                (params
                 `((ttl      . ,(plist-get plist :ttl))
                   (quit     . ,(plist-get plist :quit))
                   (select   . ,(plist-get plist :select))
                   (modeline . ,(plist-get plist :modeline))
                   (autosave . ,(plist-get plist :autosave))
                   ,@(plist-get plist :parameters))))
           `(,predicate (+popup-buffer)
                        ,@alist
                        (window-parameters ,@params))))))

;;;###autodef
(defun set-popup-rule! (predicate &rest plist)
  "Define a popup rule.

These rules affect buffers displayed with `pop-to-buffer' and `display-buffer'
(or their siblings). Buffers displayed with `switch-to-buffer' (and its
variants) will not be affected by these rules (as they are unaffected by
`display-buffer-alist', which powers the popup management system).

PREDICATE can be either a) a regexp string (matched against the buffer's name)
or b) a function that takes no arguments and returns a boolean.

PLIST can be made up of any of the following properties:

:ignore BOOL
  If BOOL is non-nil, popups matching PREDICATE will not be handled by the popup
  system. Use this for buffers that have their own window management system like
  magit or helm.

:actions ACTIONS
  ACTIONS is a list of functions or an alist containing (FUNCTION . ALIST). See
  `display-buffer''s second argument for more information on its format and what
  it accepts. If omitted, `+popup-default-display-buffer-actions' is used.

:side 'bottom|'top|'left|'right
  Which side of the frame to open the popup on. This is only respected if
  `+popup-display-buffer-stacked-side-window' or `display-buffer-in-side-window'
  is in :actions or `+popup-default-display-buffer-actions'.

:size/:width/:height FLOAT|INT|FN
  Determines the size of the popup. If more tha one of these size properties are
  given :size always takes precedence, and is mapped with window-width or
  window-height depending on what :side the popup is opened. Setting a height
  for a popup that opens on the left or right is harmless, but comes into play
  if two popups occupy the same :vslot.

  If a FLOAT (0 < x < 1), the number represents how much of the window will be
    consumed by the popup (a percentage).
  If an INT, the number determines the size in lines (height) or units of
    character width (width).
  If a function, it takes one argument: the popup window, and can do whatever it
    wants with it, typically resize it, like `+popup-shrink-to-fit'.

:slot/:vslot INT
  (This only applies to popups with a :side and only if :actions is blank or
  contains the `+popup-display-buffer-stacked-side-window' action) These control
  how multiple popups are laid out. INT can be any integer, positive and
  negative.

  :slot controls lateral positioning (e.g. the horizontal positioning for
    top/bottom popups, or vertical positioning for left/right popups).
  :vslot controls popup stacking (from the edge of the frame toward the center).

  Let's assume popup A and B are opened with :side 'bottom, in that order.
    If they possess the same :slot and :vslot, popup B will replace popup A.
    If popup B has a higher :slot, it will open to the right of popup A.
    If popup B has a lower :slot, it will open to the left of popup A.
    If popup B has a higher :vslot, it will open above popup A.
    If popup B has a lower :vslot, it will open below popup A.

:ttl INT|BOOL|FN
  Stands for time-to-live. It can be t, an integer, nil or a function. This
  controls how (and if) the popup system will clean up after the popup.

  If any non-zero integer, wait that many seconds before killing the buffer (and
    any associated processes).
  If 0, the buffer is immediately killed.
  If nil, the buffer won't be killed and is left to its own devices.
  If t, resort to the default :ttl in `+popup-defaults'. If none exists, this is
    the same as nil.
  If a function, it takes one argument: the target popup buffer. The popup
    system does nothing else and ignores the function's return value.

:quit FN|BOOL|'other|'current
  Can be t, 'other, 'current, nil, or a function. This determines the behavior
  of the ESC/C-g keys in or outside of popup windows.

  If t, close the popup if ESC/C-g is pressed anywhere.
  If 'other, close this popup if ESC/C-g is pressed outside of any popup. This
    is great for popups you may press ESC/C-g a lot in.
  If 'current, close the current popup if ESC/C-g is pressed from inside of the
    popup. This makes it harder to accidentally close a popup until you really
    want to.
  If nil, pressing ESC/C-g will never close this popup.
  If a function, it takes one argument: the to-be-closed popup window, and is
    run when ESC/C-g is pressed while that popup is open. It must return one of
    the other values to determine the fate of the popup.

:select BOOL|FN
  Can be a boolean or function. The boolean determines whether to focus the
  popup window after it opens (non-nil) or focus the origin window (nil).

  If a function, it takes two arguments: the popup window and originating window
    (where you were before the popup opened). The popup system does nothing else
    and ignores the function's return value.

:modeline BOOL|FN|LIST
  Can be t (show the default modeline), nil (show no modeline), a function that
  returns a modeline format or a valid value for `mode-line-format' to be used
  verbatim. The function takes no arguments and is run in the context of the
  popup buffer.

:autosave BOOL|FN
  This parameter determines what to do with modified buffers when closing popup
  windows. It accepts t, 'ignore, a function or nil.

  If t, no prompts. Just save them automatically (if they're file-visiting
    buffers). Same as 'ignore for non-file-visiting buffers.
  If nil (the default), prompt the user what to do if the buffer is
    file-visiting and modified.
  If 'ignore, no prompts, no saving. Just silently kill it.
  If a function, it is run with one argument: the popup buffer, and must return
    non-nil to save or nil to do nothing (but no prompts).

:parameters ALIST
  An alist of custom window parameters. See `(elisp)Window Parameters'.

If any of these are omitted, defaults derived from `+popup-defaults' will be
used."
  (declare (indent defun))
  (push (+popup--make predicate plist) +popup--display-buffer-alist)
  (when (bound-and-true-p +popup-mode)
    (setq display-buffer-alist +popup--display-buffer-alist))
  +popup--display-buffer-alist)

;;;###autoload
(defun set-popup-rules! (&rest rulesets)
  "Defines multiple popup rules.

Every entry in RULESETS should be a list of alists where the CAR is the
predicate and CDR is a plist. See `set-popup-rule!' for details on the predicate
and plist.

Example:

  (set-popup-rules!
    '((\"^ \\*\" :slot 1 :vslot -1 :size #'+popup-shrink-to-fit)
      (\"^\\*\"  :slot 1 :vslot -1 :select t))
    '((\"^\\*Completions\" :slot -1 :vslot -2 :ttl 0)
      (\"^\\*Compil\\(?:ation\\|e-Log\\)\" :size 0.3 :ttl 0 :quit t)))"
  (declare (indent 0))
  (dolist (rules rulesets)
    (dolist (rule rules)
      (push (+popup--make (car rule) (cdr rule))
            +popup--display-buffer-alist)))
  (when (bound-and-true-p +popup-mode)
    (setq display-buffer-alist +popup--display-buffer-alist))
  +popup--display-buffer-alist)


;;
;; Obsolete settings

;; FIXME obsolete :popup
;;;###autoload
(def-setting! :popup (condition &optional alist parameters)
  :obsolete set-popup-rule!
  `(set-popup-rule! ,condition ,alist ,parameters))

;; FIXME obsolete :popups
;;;###autoload
(def-setting! :popups (&rest rulesets)
  :obsolete set-popup-rules!
  `(set-popup-rules! ,@rulesets))

