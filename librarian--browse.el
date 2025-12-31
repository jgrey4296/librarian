;;; browse.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'eww)
  )

(defconst librarian--browse-buffer-name "* librarian browser*")

(defvar librarian--browse-default "eww")

(defvar librarian--browse-use-preview t)

(defvar librarian--browse-variants ())

(defvar librarian--browse-variants-file "~/.browsers")

(defvar librarian--browse-pdf-args  '("-a" "Preview" "-nF"))

(defvar librarian--browse-epub-args '("-a" "ebook-viewer"))

(defun librarian--browse-load-variants ()
  " Get a list of possible browsers to use from persistent file"
  (interactive)
  (with-temp-buffer
    (insert-file-contents (expand-file-name librarian--browse-variants-file))
    (mapc #'(lambda (x) (add-to-list 'librarian--browse-variants x))
           (split-string (buffer-string) "\n" t " +"))
    )
  )

(defun librarian--browse-select ()
  (interactive)
  (ivy-read (format "(%s) Select Browser: " librarian--browse-default)
            librarian--browse-variants
            :require-match t
            :action #'(lambda (x) (setq librarian--browse-default x))
            )
  )

(defun librarian--browse-file-preview ()
  "Toggle the use of preview for pdfs"
  (interactive)
  (message "Using Preview for pdfs: %s"
           (setq librarian--browse-use-preview (not librarian--browse-use-preview)))
  )

;;;###autoload (defalias 'librarian-browse-open #'librarian--browse-open-url)

;;;###autoload (defalias 'librarian-browse-select #'librarian--browse-select)

;;;###autoload (autoload 'librarian--browse-open-url "librarian--browse")
(defun librarian--browse-open-url (url &rest args)
  " Find and call the appropriate browser program,
after `browse-url-handlers` have processed the url
"
  (interactive)
  (cond ((-contains? args 'quicklook)
         (start-process "open-ql" librarian--browse-buffer-name "qlmanage" "-p" (shell-quote-argument url)))
        ((and (-contains? args 'local) (f-ext? url "epub"))
         (apply 'start-process "open-epub" librarian--browse-buffer-name "open" url librarian--browse-epub-args)
         )
        ((and (-contains? args 'local) (f-ext? url "pdf") librarian--browse-use-preview)
         (apply 'start-process "open-pdf" librarian--browse-buffer-name "open" url librarian--browse-pdf-args)
         )
        ((not (s-equals? librarian--browse-default "eww"))
         (message "Using %s" librarian--browse-default)
         (start-process "open-url" librarian--browse-buffer-name librarian--browse-default url)
         )
        (t
         (eww-browse-url url args))
        )

  (sleep-for 2)
  ;; (librarian--browse-regain-focus)
  )

(provide 'librarian--browse)
;;; librarian--browse.el ends here
