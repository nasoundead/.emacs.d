;;; init-lookup.el
;; Author: Haibo Wang <nasoundead@163.com>
;; Version: 0.0.1
;; URL: https://github.com/nasoundead/.emacs.d
;; Keywords:
;; Compatibility:
;; Reference:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Package configurations.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;
;;; Helpers

(defun +lookup--set-handler (spec functions-var &optional async enable)
  (when spec
    (cl-destructuring-bind (fn . plist)
        (sea-enlist spec)
      (if (not enable)
          (remove-hook functions-var fn 'local)
        (put fn '+lookup-async (or (plist-get plist :async) async))
        (add-hook functions-var fn nil 'local)))))

(defun +lookup--run-handler (handler identifier)
  (if (commandp handler)
      (call-interactively handler)
    (funcall handler identifier)))

(defun +lookup--run-handlers (handler identifier origin)
  (sea-log "Looking up '%s' with '%s'" identifier handler)
  (condition-case-unless-debug e
      (let ((wconf (current-window-configuration))
            (result (condition-case-unless-debug e
                        (+lookup--run-handler handler identifier)
                      (error
                       (sea-log "Lookup handler %S threw an error: %s" handler e)
                       'fail))))
        (cond ((eq result 'fail)
               (set-window-configuration wconf)
               nil)
              ((or (get handler '+lookup-async)
                   (eq result 'deferred)))
              ((or result
                   (null origin)
                   (/= (point-marker) origin))
               (prog1 (point-marker)
                 (set-window-configuration wconf)))))
    ((error user-error)
     (message "Lookup handler %S: %s" handler e)
     nil)))

(defun +lookup--jump-to (prop identifier &optional display-fn arg)
  (let* ((origin (point-marker))
         (handlers (plist-get (list :definition '+lookup-definition-functions
                                    :references '+lookup-references-functions
                                    :documentation '+lookup-documentation-functions
                                    :file '+lookup-file-functions)
                              prop))
         (result
          (if arg
              (if-let*
                  ((handler (intern-soft
                             (completing-read "Select lookup handler: "
                                              (remq t (append (symbol-value handlers)
                                                              (default-value handlers)))
                                              nil t))))
                  (+lookup--run-handlers handler identifier origin)
                (user-error "No lookup handler selected"))
            (run-hook-wrapped handlers #'+lookup--run-handlers identifier origin))))
    (when (cond ((null result)
                 (message "No lookup handler could find %S" identifier)
                 nil)
                ((markerp result)
                 (funcall (or display-fn #'switch-to-buffer)
                          (marker-buffer result))
                 (goto-char result)
                 result)
                (result))
      (with-current-buffer (marker-buffer origin)
        (better-jumper-set-jump (marker-position origin)))
      result)))


(defun +lookup-symbol-or-region (&optional initial)
  "Grab the symbol at point or selected region."
  (cond ((stringp initial)
         initial)
        ((use-region-p)
         (buffer-substring-no-properties (region-beginning)
                                         (region-end)))
        ((require 'xref nil t)
         ;; A little smarter than using `symbol-at-point', though in most cases,
         ;; xref ends up using `symbol-at-point' anyway.
         (xref-backend-identifier-at-point (xref-find-backend)))))


;;
;;; Lookup backends

(defun +lookup--xref-show (fn identifier)
  (let ((xrefs (funcall fn
                        (xref-find-backend)
                        identifier)))
    (when xrefs
      (xref--show-xrefs xrefs nil)
      (if (cdr xrefs)
          'deferred
        t))))

(defun +lookup-xref-definitions-backend-fn (identifier)
  "Non-interactive wrapper for `xref-find-definitions'"
  (+lookup--xref-show 'xref-backend-definitions identifier))

(defun +lookup-xref-references-backend-fn (identifier)
  "Non-interactive wrapper for `xref-find-references'"
  (+lookup--xref-show 'xref-backend-references identifier))

(defun +lookup-dumb-jump-backend-fn (_identifier)
  "Look up the symbol at point (or selection) with `dumb-jump', which conducts a
project search with ag, rg, pt, or git-grep, combined with extra heuristics to
reduce false positives.

This backend prefers \"just working\" over accuracy."
  (and (require 'dumb-jump nil t)
       (dumb-jump-go)))

(defun +lookup-project-search-backend-fn (identifier)
  "Conducts a simple project text search for IDENTIFIER.

Uses and requires `+ivy-file-search' or `+helm-file-search'. Will return nil if
neither is available. These search backends will use ag, rg, or pt (in an order
dictated by `+ivy-project-search-engines' or `+helm-project-search-engines',
falling back to git-grep)."
  (unless identifier
    (let ((query (rxt-quote-pcre identifier)))
      (ignore-errors
        (cond ((featurep! :completion ivy)
               (+ivy-file-search nil :query query)
               t)
              ((featurep! :completion helm)
               (+helm-file-search nil :query query)
               t))))))

(defun +lookup-evil-goto-definition-backend-fn (_identifier)
  "Uses `evil-goto-definition' to conduct a text search for IDENTIFIER in the
current buffer."
  (and (fboundp 'evil-goto-definition)
       (ignore-errors
         (cl-destructuring-bind (beg . end)
             (bounds-of-thing-at-point 'symbol)
           (evil-goto-definition)
           (let ((pt (point)))
             (not (and (>= pt beg)
                       (<  pt end))))))))
                       
;; "What am I looking at?" This module helps you answer this question.
;;
;;   + `+lookup/definition': a jump-to-definition that should 'just work'
;;   + `+lookup/references': find a symbol's references in the current project
;;   + `+lookup/file': open the file referenced at point
;;   + `+lookup/online'; look up a symbol on online resources
;;   + `+lookup/in-docsets': look up in Dash docsets
;;
;; This module uses `xref', an experimental new library in Emacs. It may change
;; in the future. When xref can't be depended on it will fall back to
;; `dumb-jump' to find what you want.

(defvar +lookup-provider-url-alist
  (append '(("Google"            . "https://google.com/search?q=%s")
            ("Google images"     . "https://www.google.com/images?q=%s")
            ("Google maps"       . "https://maps.google.com/maps?q=%s")
            ("Project Gutenberg" . "http://www.gutenberg.org/ebooks/search/?query=%s")
            ("DuckDuckGo"        . "https://duckduckgo.com/?q=%s")
            ("DevDocs.io"        . "https://devdocs.io/#q=%s")
            ("StackOverflow"     . "https://stackoverflow.com/search?q=%s")
            ("Github"            . "https://github.com/search?ref=simplesearch&q=%s")
            ("Youtube"           . "https://youtube.com/results?aq=f&oq=&search_query=%s")
            ("Wolfram alpha"     . "https://wolframalpha.com/input/?i=%s")
            ("Wikipedia"         . "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
            ("Rust Docs"         . "https://doc.rust-lang.org/edition-guide/?search=%s")))
  "An alist that maps online resources to their search url or a function that
produces an url. Used by `+lookup/online'.")

(defvar +lookup-open-url-fn #'browse-url
  "Function to use to open search urls.")

(defvar +lookup-definition-functions
  '(+lookup-xref-definitions-backend-fn
    +lookup-dumb-jump-backend-fn
    +lookup-project-search-backend-fn
    +lookup-evil-goto-definition-backend-fn)
  "Functions for `+lookup/definition' to try, before resorting to `dumb-jump'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-references-functions
  '(+lookup-xref-references-backend-fn
    +lookup-project-search-backend-fn)
  "Functions for `+lookup/references' to try, before resorting to `dumb-jump'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-documentation-functions
  '(+lookup-online-backend-fn)
  "Functions for `+lookup/documentation' to try, before resorting to
`dumb-jump'. Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-file-functions ()
  "Function for `+lookup/file' to try, before restoring to `find-file-at-point'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")


;; Jump to definition via `ag'/`rg'/`grep'
(use-package dumb-jump
  :init (add-hook 'after-init-hook #'dumb-jump-mode)
  :config
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-default-project sea-emacs-dir
        dumb-jump-aggressive nil)
  (with-eval-after-load 'ivy
    (setq dumb-jump-selector 'ivy))
  (add-hook 'dumb-jump-after-jump-hook #'better-jumper-set-jump))

;;
;;; xref

;; The lookup commands are superior, and will consult xref if there are no
;; better backends available.
(global-set-key [remap xref-find-definitions] #'+lookup/definition)
(global-set-key [remap xref-find-references]  #'+lookup/references)

(after! xref
  ;; We already have `projectile-find-tag' and `evil-jump-to-tag', no need for
  ;; xref to be one too.
  (remove-hook 'xref-backend-functions #'etags--xref-backend)
  ;; ...however, it breaks `projectile-find-tag', unless we put it back.
  (defadvice! +lookup--projectile-find-tag-a (orig-fn)
    :around #'projectile-find-tag
    (let ((xref-backend-functions '(etags--xref-backend t)))
      (funcall orig-fn)))

  ;; Use `better-jumper' instead of xref's marker stack
  (advice-add #'xref-push-marker-stack :around #'doom-set-jump-a)

  (use-package ivy-xref
    :config
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
    (set-popup-rule! "^\\*xref\\*$" :ignore t))
  )



(provide 'init-lookup)
;;; init-lookup ends here
