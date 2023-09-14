;;; man.el -*- lexical-binding: t; -*-

(defvar librarian-man--path nil)

(setq Man-switches (string-join (list "-C"
                                      (expand-file-name "templates/tools/man.conf" doom-user-dir))
                                " ")
      manual-program (executable-find "man")
      )

(defun librarian-man--call ()
  (interactive)
  ;; TODO refactor this
  (with-file-contents! (f-join librarian-man--path "man-completions")
    (let ((result (ivy-read "Man Page: " (s-split "\n" (buffer-string) t))))
      (man (car (s-split "\\( \\|,\\)" result)))
      )
    )
  )

(defun librarian-man--completion ()
  (call-process "man" nil `(:file ,(f-join librarian-man--path "man-completions")) nil "-k" ".")
  )

(provide 'librarian-man)
