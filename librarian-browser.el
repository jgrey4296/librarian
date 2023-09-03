;;; browse.el -*- lexical-binding: t; -*-

(defun librarian-browser-load-variants ()
  " Get a list of possible browsers to use from persistent file"
  (with-temp-buffer
    (insert-file (expand-file-name librarian-browser-variants-file))
    (setq librarian-browser-variants (split-string (buffer-string) "\n" t " +"))
    )
  )

(defun librarian-browser-select ()
  (interactive)
  (ivy-read (format "(%s) Select Browser: " librarian-default-browser)
            librarian-browser-variants
            :require-match t
            :action #'(lambda (x) (setq librarian-default-browser x))
            )
  )

(defun librarian-browser-file-preview ()
  "Toggle the use of preview for pdfs"
  (interactive)
  (message "Using Preview for pdfs: %s"
           (setq librarian-browser-use-preview (not librarian-browser-use-preview)))
  )

(provide 'browse-select)
