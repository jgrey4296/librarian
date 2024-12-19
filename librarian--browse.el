;;; browse.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'eww)
  )

(defconst lib-buffer-name "*librarian browser*")

(defvar lib-default "eww")

(defvar lib-use-preview t)

(defvar lib-variants ())

(defvar lib-variants-file "~/.browsers")

(defvar lib-pdf-args  '("-a" "Preview" "-nF"))

(defvar lib-epub-args '("-a" "ebook-viewer"))

(defun lib-load-variants ()
  " Get a list of possible browsers to use from persistent file"
  (with-temp-buffer
    (insert-file-contents (expand-file-name lib-variants-file))
    (mapc #'(lambda (x) (add-to-list 'lib-variants x))
           (split-string (buffer-string) "\n" t " +"))
    )
  )

(defun lib-select ()
  (interactive)
  (ivy-read (format "(%s) Select Browser: " lib-default)
            lib-variants
            :require-match t
            :action #'(lambda (x) (setq lib-default x))
            )
  )

(defun lib-file-preview ()
  "Toggle the use of preview for pdfs"
  (interactive)
  (message "Using Preview for pdfs: %s"
           (setq lib-use-preview (not lib-use-preview)))
  )


;;;###autoload (defalias 'librarian-browse-open #'librarian--browse-open-url)
;;;###autoload (autoload 'librarian--browse-open-url "librarian--browse")
(defun lib-open-url (url &rest args)
  " Find and call the appropriate browser program,
after `browse-url-handlers` have processed the url
"
  (interactive)
  (cond ((-contains? args 'quicklook)
         (start-process "open-ql" lib-buffer-name "qlmanage" "-p" (shell-quote-argument url)))
        ((and (-contains? args 'local) (f-ext? url "epub"))
         (apply 'start-process "open-epub" lib-buffer-name "open" url lib-epub-args)
         )
        ((and (-contains? args 'local) (f-ext? url "pdf") lib-use-preview)
         (apply 'start-process "open-pdf" lib-buffer-name "open" url lib-pdf-args)
         )
        ((not (s-equals? lib-default "eww"))
         (message "Using %s" lib-default)
         (start-process "open-url" lib-buffer-name lib-default url)
         )
        (t
         (eww-browse-url url args))
        )

  (sleep-for 2)
  ;; (lib-regain-focus)
  )


(provide 'librarian--browse)
;; Local Variables:
;; read-symbol-shorthands: (
;; ("lib-" . "librarian--browse-")
;; )
;; End:
