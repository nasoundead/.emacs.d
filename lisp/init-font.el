(use-package unicode-fonts)
(setq fonts
      (cond ((eq system-type 'darwin)     '("Monaco"    "STHeiti"))
            ((eq system-type 'gnu/linux)  '("Ubuntu Mono"     "WenQuanYi Micro Hei Mono"))
            ((eq system-type 'windows-nt) '("JetBrains Mono"  "宋体"))
            ;; ((eq system-type 'windows-nt) '("FiraCode"  "宋体"))
            ;; ((eq system-type 'windows-nt) '("Iosevka"  "宋体"))
            ;; ((eq system-type 'windows-nt) '("JetBrainsMono Nerd Font"  "宋体"))
            ;; ((eq system-type 'windows-nt) '("FiraCode Nerd Font"  "宋体"))
            ;; ((eq system-type 'windows-nt) '("Inconsolata NFM"  "宋体"))
            ;; ;; ((eq system-type 'windows-nt) '("Source Code Pro"  "宋体"))
            ;; ((eq system-type 'windows-nt) '("Cascadia Code"  "宋体"))
            ;; ((eq system-type 'windows-nt) '("SauceCodePro Nerd Font"  "宋体"))
            ))
(set-face-attribute 'default nil :font
                    (format "%s:pixelsize=%d" (car fonts) 16))
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
                    (font-spec :family (car (cdr fonts)))))
;; Fix chinese font width and rescale
(setq face-font-rescale-alist '(("宋体". 1.0) ("Microsoft Yahei" . 1.2) ("WenQuanYi Micro Hei Mono" . 1.2) ("STHeiti". 1.2)))

;; (custom-set-faces
;;  '(org-table ((t (:family "Ubuntu Mono")))))


;; (defvar meomacs-font-size 14
;;     "Current font size.")

;; (defvar meomacs-fonts '((default . "Cascadia Code")
;;                         (cjk . "Microsoft Yahei")
;;                         (symbol . "Symbola")
;;                         (fixed . "Fira Code")
;;                         (fixed-serif . "Fira Code")
;;                         ;; (fixed-serif . "Monospace Serif")
;;                         (variable . "Fira Code")
;;                         ;; (variable . "Sans Serif")
;;                         (wide . "Fira Code")
;;                         ;; (wide . "Monospace")
;;                         (tall . "Fira Code"))
;;                         ;; (tall . "Monospace"))
;; "Fonts to use.")

;; (defun meomacs--get-font-family (key)
;;     (let ((font (alist-get key meomacs-fonts)))
;;         (if (string-empty-p font)
;;             (alist-get 'default meomacs-fonts)
;;         font)))

;; (defun meomacs-load-default-font ()
;;     "Load default font configuration."
;;     (let ((default-font (format "%s-%s"
;;                                 (meomacs--get-font-family 'default)
;;                                 meomacs-font-size)))
;;         (add-to-list 'default-frame-alist (cons 'font default-font))))

;; (defun meomacs-load-face-font ()
;; "Load face font configuration."
;; (let ((variable-font (meomacs--get-font-family 'variable))
;;         (fixed-font (meomacs--get-font-family 'fixed))
;;         (fixed-serif-font (meomacs--get-font-family 'fixed-serif)))
;;     (set-face-attribute 'variable-pitch nil :family variable-font)
;;     (set-face-attribute 'fixed-pitch nil :family fixed-font)
;;     (set-face-attribute 'fixed-pitch-serif nil :family fixed-serif-font)))

;; (defun meomacs-load-charset-font (&optional font)
;;     "Load charset font configuration."
;;     (let ((default-font (or font (format "%s-%s"
;;                                             (meomacs--get-font-family 'default)
;;                                             meomacs-font-size)))
;;             (cjk-font (meomacs--get-font-family 'cjk))
;;             (symbol-font (meomacs--get-font-family 'symbol)))
;;         (set-frame-font default-font)
;;         (dolist (charset '(kana han hangul cjk-misc bopomofo))
;;         (set-fontset-font t charset cjk-font))
;;         (set-fontset-font t 'symbol symbol-font)))

;; (meomacs-load-default-font)
;; (meomacs-load-face-font)

;; (defvar meomacs-font-current-variant nil)

;; (defun meomacs-dynamic-set-font (&rest ignore)
;; (interactive)
;; (when window-system
;;     (when (or (frame-root-window-p (get-buffer-window))
;;             (frame-root-window-p (window-parent)))
;;     (let* ((prev-font-style meomacs-font-current-variant)
;;             (wl (seq-filter (lambda (w) (not (string-prefix-p " " (buffer-name (window-buffer w))))) (window-list)))
;;             (def (meomacs--get-font-family 'default))
;;             (new-variant (cond
;;                             ((= 1 (length wl))
;;                             (meomacs--get-font-family 'default))

;;                             ((window-combined-p)
;;                             (meomacs--get-font-family 'tall))

;;                             (t
;;                             (meomacs--get-font-family 'wide)))))
;;     (unless (equal prev-font-style new-variant)
;;         (setq meomacs-font-current-variant new-variant)
;;         (set-frame-font new-variant)
;;         (meomacs-load-charset-font new-variant))))))

;; (setq frame-inhibit-implied-resize t)
;; (add-hook 'window-state-change-hook 'meomacs-dynamic-set-font)

(use-package all-the-icons
  :if (display-graphic-p)
  :commands (all-the-icons-octicon all-the-icons-faicon all-the-icons-fileicon
                                   all-the-icons-wicon all-the-icons-material all-the-icons-alltheicon)
  :init
  (defun sea*disable-all-the-icons-in-tty (orig-fn &rest args)
    (when (display-graphic-p)
      (apply orig-fn args)))
  :config
  (setq inhibit-compacting-font-caches t)
  ;; all-the-icons doesn't work in the terminal, so we "disable" it.
  (dolist (fn '(all-the-icons-octicon all-the-icons-material
                                      all-the-icons-faicon all-the-icons-fileicon
                                      all-the-icons-wicon all-the-icons-alltheicon))
    (advice-add fn :around #'sea*disable-all-the-icons-in-tty))
    )


(provide 'init-font)
