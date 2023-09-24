(use-package unicode-fonts)
(setq fonts
      (cond ((eq system-type 'darwin)     '("Monaco"    "STHeiti"))
            ((eq system-type 'gnu/linux)  '("Ubuntu Mono"     "WenQuanYi Micro Hei Mono"))
            ;; ((eq system-type 'windows-nt) '("JetBrains Mono"  "宋体"))
            ((eq system-type 'windows-nt) '("Iosevka"  "宋体"))
            ;; ((eq system-type 'windows-nt) '("JetBrainsMono Nerd Font"  "宋体"))
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