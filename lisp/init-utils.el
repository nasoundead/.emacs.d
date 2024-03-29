;; init-utils.el --- Initialize ultilities.	-*- lexical-binding: t -*-
;;; Code:


;; Context-sensitive external browse URL or Internet search
(use-package browse-url-dwim
  :init (add-hook 'after-init-hook #'browse-url-dwim-mode))

(use-package copyit)
(use-package restart-emacs)

(defun sea-lookup-wikipedia ()
  "Look up the word under cursor in Wikipedia.
If there is a text selection (a phrase), use that.

This command switches to browser."
  (interactive)
  (let (word)
    (setq word
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (current-word)))
    (setq word (replace-regexp-in-string " " "_" word))
    (browse-url (concat "http://en.wikipedia.org/wiki/" word))
    ;; (eww myUrl) ; emacs's own browser
    ))

(defun sea-delete-process-interactive ()
  "Delete process started in Emacs."
  (interactive)
  (let ((pname (ido-completing-read "Process Name: "
                                    (mapcar 'process-name (process-list)))))
    (delete-process (get-process pname))))


(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
