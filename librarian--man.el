;;; man.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'f)
  (require 'man)
  )

(defvar lim--cmd (executable-find "man"))

(defvar lim-path nil)

(defvar lim-config nil)

(defvar lim-cache-dir nil)

(defun librarian-man ()
  " Man, but with cached completion "
  (interactive)
  (let ((Man-switches (string-join (list "-C" lim-config)))
        (manual-program lim--cmd)
        result
        )
    (unless (f-exists? (f-join lim-cache-dir "man-completions"))
      (message "Building Man Completion List")
      (call-process "man" nil (list :file (f-join lim-cache-dir "man-completions")) nil "-k" "."))
    (with-temp-buffer
      (insert-file-contents (f-join lim-cache-dir "man-completions"))
      (setq result (ivy-read "Man Page: " (s-split "\n" (buffer-string) t)))
      )
    (man (car (s-split "\\( \\|,\\)" result)))
    )
  )

(provide 'librarian--man)
;;; librarian--man.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("lim-" . "librarian--man-")
;; )
;; End:
