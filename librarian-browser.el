;;; browse.el -*- lexical-binding: t; -*-

(require 'eww)

(defconst librarian-browser-buffer-name "*librarian browser*")

(defvar librarian-default-browser "firefox")

(defvar librarian-browser-use-preview t)

(defvar librarian-browser-variants ())

(defvar librarian-browser-variants-file "~/.browsers")

(defvar librarian-pdf-args  '("-a" "Preview" "-nF"))

(defvar librarian-epub-args '("-a" "ebook-viewer"))

(defun librarian-browser-load-variants ()
  " Get a list of possible browsers to use from persistent file"
  (with-temp-buffer
    (insert-file (expand-file-name librarian-browser-variants-file))
    (mapc #'(lambda (x) (add-to-list 'librarian-browser-variants x))
           (split-string (buffer-string) "\n" t " +"))
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

(defun librarian-browser--open-url (url &rest args)
  " Find and call the appropriate browser program,
after `browse-url-handlers` have processed the url
"
  (cond ((-contains? args 'quicklook)
         (start-process "open-ql" librarian-browser-buffer-name "qlmanage" "-p" (shell-quote-argument url)))
        ((and (-contains? args 'local) (f-ext? url "epub"))
         (apply 'start-process "open-epub" librarian-browser-buffer-name "open" url librarian-epub-args)
         )
        ((and (-contains? args 'local) (f-ext? url "pdf") librarian-browser-use-preview)
         (apply 'start-process "open-pdf" librarian-browser-buffer-name "open" url librarian-pdf-args)
         )
        ((not (s-equals? librarian-default-browser "eww"))
         (message "Using %s" librarian-default-browser)
         (start-process "open-url" librarian-browser-buffer-name librarian-default-browser url)
         )
        (t
         (eww-browse-url url args))
        )

  (sleep-for 2)
  ;; (librarian-browser-regain-focus)
  )

(provide 'librarian-browser)
