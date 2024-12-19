;;; librarian-docsets.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'dash-docs)
  (require 'librarian--util)
  (require 'counsel-dash)
  )

(unless (boundp 'dash-docs-docsets)
  (defvar dash-docs-docsets nil))

(defvar lid-defaults '(librarian--backend--docsets-dash
                       librarian--backend--docsets-online
                       )
  )

(defvar lid-path nil)

(defun lid-consult-search (sync cb)
  (lambda (action)
    (pcase action
      ((pred stringp)
       (when-let (cands (with-current-buffer cb
                          (dash-docs-search action)))
         (funcall sync 'flush)
         (funcall sync cands)))
      (_ (funcall sync action)))))

;;;###autoload (defalias 'librarian-docsets-consult #'librarian--docsets-consult)
;;;###autoload (autoload 'librarian--docsets-consult "librarian--docsets")
(defun lid-consult (arg &optional query docsets)
  "Librarian query relevant docsets

QUERY is a string and docsets in an array of strings, each a name of a Dash
docset. Requires either helm or ivy.

If prefix ARG is supplied, search all installed installed docsets. They can be
installed with `dash-docs-install-docset'."
  (interactive "P")
  (require 'dash-docs)
  (let ((dash-docs-common-docsets)
        (dash-docs-docsets (if arg
                               (dash-docs-installed-docsets)
                             (cl-remove-if-not #'dash-docs-docset-path (or docsets dash-docs-docsets))))
        (query (librarian--util-get query))
        )
    (message "Searching docsets %s" dash-docs-docsets)
    (counsel-dash query))
)

;;;###autoload (autoload 'evil-librarian-docset-consult "librarian--docsets")
(evil-define-command evil-librarian-docset-consult (query &optional bang)
  "Look up QUERY in your dash docsets. If BANG, prompt to select a docset (and
install it if necessary)."
  (interactive "<a><!>")
  (let (selected)
    (when bang
      (setq selected (helm-dash-read-docset "Select docset" (helm-dash-official-docsets)))
      (unless (dash-docs-docset-path selected)
        (librarian-install-docset selected)))
    (librarian-docset-consult query selected)))

;;; public aliases

;;;###autoload
(defalias 'librarian-docsets-install #'counsel-dash-install-docset)

(provide 'librarian--docsets)
;; Local Variables:
;; read-symbol-shorthands: (
;; ("lid-" . "librarian--docsets-")
;; )
;; End:
