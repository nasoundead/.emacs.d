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
  (append '(("Google"            +lookup--online-backend-google "https://google.com/search?q=%s")
            ("Google images"     "https://www.google.com/images?q=%s")
            ("Google maps"       "https://maps.google.com/maps?q=%s")
            ("Project Gutenberg" "http://www.gutenberg.org/ebooks/search/?query=%s")
            ("DuckDuckGo"        +lookup--online-backend-duckduckgo "https://duckduckgo.com/?q=%s")
            ("DevDocs.io"        "https://devdocs.io/#q=%s")
            ("StackOverflow"     "https://stackoverflow.com/search?q=%s")
            ("Github"            "https://github.com/search?ref=simplesearch&q=%s")
            ("Youtube"           "https://youtube.com/results?aq=f&oq=&search_query=%s")
            ("Wolfram alpha"     "https://wolframalpha.com/input/?i=%s")
            ("Wikipedia"         "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
            ("Rust Docs" "https://doc.rust-lang.org/edition-guide/?search=%s"))
          )
  "An alist that maps online resources to either:

  1. A search url (needs on '%s' to substitute with an url encoded query),
  2. A non-interactive function that returns the search url in #1,
  3. An interactive command that does its own search for that provider.

Used by `+lookup/online'.")

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
    +lookup-dumb-jump-backend-fn
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

(defvar +lookup-dictionary-enable-online t
  "If non-nil, look up dictionaries online.

Setting this to nil will force it to use offline backends, which may be less
than perfect, but available without an internet connection.

Used by `+lookup/word-definition' and `+lookup/word-synonyms'.

For `+lookup/word-definition', this is ignored on Mac, where Emacs users
Dictionary.app behind the scenes to get definitions.")


;;
;;; dumb-jump

(use-package dumb-jump
  :commands dumb-jump-result-follow
  :config
  (setq dumb-jump-default-project sea-emacs-dir
        dumb-jump-aggressive nil
        dumb-jump-selector 'ivy)
  (add-hook 'dumb-jump-after-jump-hook #'better-jumper-set-jump))

(defadvice! sea-set-jump-a (orig-fn &rest args)
  "Set a jump point and ensure ORIG-FN doesn't set any new jump points."
  (better-jumper-set-jump (if (markerp (car args)) (car args)))
  (let ((evil--jumps-jumping t)
        (better-jumper--jumping t))
    (apply orig-fn args)))

(defadvice! sea-set-jump-maybe-a (orig-fn &rest args)
  "Set a jump point if ORIG-FN returns non-nil."
  (let ((origin (point-marker))
        (result
         (let* ((evil--jumps-jumping t)
                (better-jumper--jumping t))
           (apply orig-fn args))))
    (unless result
      (with-current-buffer (marker-buffer origin)
        (better-jumper-set-jump
         (if (markerp (car args))
             (car args)
           origin))))
    result))
(use-package better-jumper
  :preface
  ;; REVIEW Suppress byte-compiler warning spawning a *Compile-Log* buffer at
  ;; startup. This can be removed once gilbertw1/better-jumper#2 is merged.
  (defvar better-jumper-local-mode nil)
  :init
  (global-set-key [remap evil-jump-forward]  #'better-jumper-jump-forward)
  (global-set-key [remap evil-jump-backward] #'better-jumper-jump-backward)
  (global-set-key [remap xref-pop-marker-stack] #'better-jumper-jump-backward)
  :config
  (better-jumper-mode +1)
  (add-hook 'better-jumper-post-jump-hook #'recenter)

  (defun sea-set-jump-h ()
    "Run `better-jumper-set-jump' but return nil, for short-circuiting hooks."
    (better-jumper-set-jump)
    nil)

  ;; Creates a jump point before killing a buffer. This allows you to undo
  ;; killing a buffer easily (only works with file buffers though; it's not
  ;; possible to resurrect special buffers).
  (advice-add #'kill-current-buffer :around #'sea-set-jump-a)

  ;; Create a jump point before jumping with imenu.
  (advice-add #'imenu :around #'sea-set-jump-a))
;;
;;; xref

;; The lookup commands are superior, and will consult xref if there are no
;; better backends available.
(global-set-key [remap xref-find-definitions] #'+lookup/definition)
;; (global-set-key [remap xref-find-references]  #'+lookup/references)

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
  (advice-add #'xref-push-marker-stack :around #'sea-set-jump-a)

  (use-package ivy-xref
    :config
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
    (set-popup-rule! "^\\*xref\\*$" :ignore t))

  )


;;
;;; Dash docset integration

(use-package dash-docs
  :defer t
  :init
  (add-hook '+lookup-documentation-functions #'+lookup-dash-docsets-backend-fn)
  :config
  (setq dash-docs-enable-debugging sea-debug-mode
        dash-docs-docsets-path (concat sea-etc-dir "docsets/")
        dash-docs-min-length 2
        dash-docs-browser-func #'eww)

  ;; Before `gnutls' is loaded, `gnutls-algorithm-priority' is treated as a
  ;; lexical variable, which breaks `+lookup*fix-gnutls-error'
  (defvar gnutls-algorithm-priority)
  (defadvice! +lookup--fix-gnutls-error-a (orig-fn url)
    "Fixes integer-or-marker-p errors emitted from Emacs' url library,
particularly, the `url-retrieve-synchronously' call in
`dash-docs-read-json-from-url'. This is part of a systemic issue with Emacs 26's
networking library (fixed in Emacs 27+, apparently).

See https://github.com/magit/ghub/issues/81"
    :around #'dash-docs-read-json-from-url
    (let ((gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
      (funcall orig-fn url)))

  (use-package counsel-dash))


;;
;;; Dictionary integration


(define-key! text-mode-map
    [remap +lookup/definition] #'+lookup/word-definition
    [remap +lookup/references] #'+lookup/word-synonyms)



(provide 'init-lookup)
;;; init-lookup ends here
