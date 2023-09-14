;;; man.el -*- lexical-binding: t; -*-

(defvar librarian-man--path nil)

;;;###autoload
(defun librarian-man--call ()
  (interactive)
  (with-file-contents! (f-join librarian-man--path "man-completions")
    (let ((result (ivy-read "Man Page: " (s-split "\n" (buffer-string) t))))
      (man (car (s-split "\\( \\|,\\)" result)))
      )
    )
  )

;;;###autoload
(defun librarian-man--completion ()
  (call-process "man" nil `(:file ,(f-join librarian-man--path "man-completions")) nil "-k" ".")
  )

(provide 'librarian-man)
