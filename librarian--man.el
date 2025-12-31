;;; man.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'f)
  (require 'man)
  )

(defvar librarian--man--cmd (executable-find "man"))

(defvar librarian--man-path nil)

(defvar librarian--man-config nil)

(defvar librarian--man-cache-dir nil)

(defun librarian-man ()
  " Man, but with cached completion "
  (interactive)
  (let ((Man-switches (string-join (list "-C" librarian--man-config)))
        (manual-program librarian--man--cmd)
        result
        )
    (unless (f-exists? (f-join librarian--man-cache-dir "man-completions"))
      (message "Building Man Completion List")
      (call-process "man" nil (list :file (f-join librarian--man-cache-dir "man-completions")) nil "-k" "."))
    (with-temp-buffer
      (insert-file-contents (f-join librarian--man-cache-dir "man-completions"))
      (setq result (ivy-read "Man Page: " (s-split "\n" (buffer-string) t)))
      )
    (man (car (s-split "\\( \\|,\\)" result)))
    )
  )

(provide 'librarian--man)
;;; librarian--man.el ends here
