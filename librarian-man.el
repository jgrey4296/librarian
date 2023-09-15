;;; man.el -*- lexical-binding: t; -*-
(require 'f)

(defvar librarian-man--cmd (executable-find "man"))
(defvar librarian-man--path nil)
(defvar librarian-man--config nil)


(defun librarian-man--call ()
  (interactive)
  (let ((Man-switches (string-join (list "-C" librarian-man--config)))
        (manual-program librarian-man--cmd)
        result
        )
    (unless (f-exists? (f-join librarian-man--path "man-completions"))
      (message "Building Man Completion List")
      (call-process "man" nil (list :file (f-join librarian-man--path "man-completions")) nil "-k" "."))
    (with-temp-buffer
      (insert-file-contents (f-join librarian-man--path "man-completions"))
      (setq result (ivy-read "Man Page: " (s-split "\n" (buffer-string) t)))
      )
    (man (car (s-split "\\( \\|,\\)" result)))
    )
  )



(provide 'librarian-man)
