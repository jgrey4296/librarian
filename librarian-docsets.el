;;; tools/lookup/autoload/docsets.el -*- lexical-binding: t; -*-

(defun librarian-dash-docsets-backend-fn (identifier)
  "look up identifier

This backend is meant for `librarian-documentation-functions'.
Docsets can be searched directly via `librarian-in-docsets'."
  (when (fboundp 'dash-docs-docset-path)
    (when-let (docsets (cl-remove-if-not #'dash-docs-docset-path (dash-docs-buffer-local-docsets)))
      (librarian-in-docsets nil identifier docsets)
      'deferred))
  )

(defun librarian--consult-search (sync cb)
  (lambda (action)
    (pcase action
      ((pred stringp)
       (when-let (cands (with-current-buffer cb
                          (dash-docs-search action)))
         (funcall sync 'flush)
         (funcall sync cands)))
      (_ (funcall sync action)))))

(defun librarian-in-docsets (arg &optional query docsets)
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
        (query (librarian-get query))
        )
    (message "Searching docsets %s" dash-docs-docsets)
    (counsel-dash query))
)
