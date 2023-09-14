;;; ivy.el -*- lexical-binding: t; -*-

(defvar librarian-configs--modules-dir nil)

(defvar librarian-configs--modules-cache nil)

(defvar librarian-configs--binding-file-name "+bindings.el")

(defvar librarian-configs--config-file-name "config.el")

(defvar librarian-configs--var-file-name "+vars.el")

(defvar librarian-configs--spec-def-file-name "+spec-defs.el")

(defun librarian-configs--cache-modules ()
  (let* ((root librarian-configs--modules-dir)
         (groups (f-directories (f-join librarian-configs--modules-dir "modules")))
         (modules (cl-loop for group in groups
                           append (mapcar #'(lambda (x) (f-join (f-base group) (f-base x)))
                                          (f-directories group))))
         )
    (setq librarian-configs--modules-cache modules)
    )
  )

;;;###autoload
(defun librarian-configs--edit-bindings ()
  (interactive)
  (unless librarian-configs--modules-cache (librarian-configs--cache-modules))
  (let* ((bindings (-select #'(lambda (x) (f-exists? (f-join librarian-configs--modules-dir "modules" x librarian-configs--binding-file-name))) librarian-configs--modules-cache))
         (chosen (ivy-read "Select Module Bindings: " librarian-configs--modules-cache :require-match t))
         (binding (f-join librarian-configs--modules-dir "modules" chosen librarian-configs--binding-file-name))
        )
    (if (f-exists? binding)
      (find-file binding)
      (message "Doesnt Exist: %s" binding)
      )
    )
  )

;;;###autoload
(defun librarian-configs--edit-vars ()
  (interactive)
  (unless librarian-configs--modules-cache (librarian-configs--cache-modules))
  (let* ((vars (-select #'(lambda (x) (f-exists? (f-join librarian-configs--modules-dir "modules" x librarian-configs--var-file-name))) librarian-configs--modules-cache))
         (chosen (ivy-read "Select Module Vars: "  librarian-configs--modules-cache :require-match t))
         (varf (f-join librarian-configs--modules-dir "modules" chosen librarian-configs--var-file-name))
        )
    (if (f-exists? varf)
      (find-file varf)
      (message "Doesnt Exist: %s" varf)
      )
    )
  )

;;;###autoload
(defun librarian-configs--edit-config ()
  (interactive)
  (unless librarian-configs--modules-cache (librarian-configs--cache-modules))
  (let* ((config (-select #'(lambda (x) (f-exists? (f-join librarian-configs--modules-dir "modules" x librarian-configs--config-file-name))) librarian-configs--modules-cache))
         (chosen (ivy-read "Select Module Config: " librarian-configs--modules-cache :require-match t))
         (binding (f-join librarian-configs--modules-dir "modules" chosen librarian-configs--config-file-name))
        )
    (if (f-exists? binding)
      (find-file binding)
      (message "Doesnt Exist: %s" binding)
      )
    )
  )

;;;###autoload
(defun librarian-configs--edit-spec-defs ()
  (interactive)
  (unless librarian-configs--modules-cache (librarian-configs--cache-modules))
  (let* ((config (-select #'(lambda (x) (f-exists? (f-join librarian-configs--modules-dir "modules" x librarian-configs--config-file-name))) librarian-configs--modules-cache))
         (chosen (ivy-read "Select Module Config: " librarian-configs--modules-cache :require-match t))
         (binding (f-join librarian-configs--modules-dir "modules" chosen librarian-configs--spec-def-file-name))
        )
    (if (f-exists? binding)
      (find-file binding)
      (message "Doesnt Exist: %s" binding)
      )
    )
  )

(provide 'librarian-configs)
