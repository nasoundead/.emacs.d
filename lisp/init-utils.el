;; init-utils.el --- Initialize ultilities.	-*- lexical-binding: t -*-
;;; Code:


;; Context-sensitive external browse URL or Internet search
(use-package browse-url-dwim
  :init (add-hook 'after-init-hook #'browse-url-dwim-mode))

;; Youdao Dictionay
;; (use-package youdao-dictionary
;;   :bind (("C-c y" . youdao-dictionary-search-at-point)
;;          ("C-c Y" . youdao-dictionary-search-at-point-tooltip))
;;   :config
;;   ;; Cache documents
;;   (setq url-automatic-caching t)

;;   ;; Enable Chinese word segmentation support (支持中文分词)
;;   (setq youdao-dictionary-use-chinese-word-segmentation t))

;; Search utils: `ag', `rg', `pt'
(use-package ag
  :init
  (with-eval-after-load 'projectile
    (bind-key "C-c p s s" 'ag-project projectile-mode-map))
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t))

(use-package wgrep-ag
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))


;; Log keyboard commands to buffer
(use-package command-log-mode
  :diminish (command-log-mode . "¢")
  :init (setq command-log-mode-auto-show t))

;; restart emacs
(use-package restart-emacs)

(use-package dictionary)

(require 'browse-url) ; part of gnu emacs

(defun my-lookup-wikipedia ()
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

(defun delete-process-interactive ()
  "Delete process started in Emacs."
  (interactive)
  (let ((pname (ido-completing-read "Process Name: "
                                    (mapcar 'process-name (process-list)))))

    (delete-process (get-process pname))))

(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
