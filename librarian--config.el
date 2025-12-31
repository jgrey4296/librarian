;;; ivy.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib)
  (require 'f)
  (declare-function ivy-read "ivy")
  )

(defvar librarian--config-modules-dir nil)

(defvar librarian--config-modules-cache nil)

(defvar librarian--config-binding-file-name "+bindings.el")

(defvar librarian--config-config-file-name "config.el")

(defvar librarian--config-var-file-name "+vars.el")

(defvar librarian--config-spec-def-file-name "+spec-defs.el")

(defun librarian--config--build-modules-cache ()
  (let* ((root librarian--config-modules-dir)
         (groups (f-directories librarian--config-modules-dir))
         (modules (cl-loop for group in groups
                           append (mapcar #'(lambda (x) (f-join (f-base group) (f-base x)))
                                          (f-directories group))))
         )
         (setq librarian--config-modules-cache modules)
         )
    )

(defun librarian-configs--edit-bindings ()
  (interactive)
  (unless librarian--config-modules-cache (librarian--config--build-modules-cache))
  (let* ((bindings (-select #'(lambda (x) (f-exists? (f-join librarian--config-modules-dir  x librarian--config-binding-file-name))) librarian--config-modules-cache))
         (chosen (ivy-read "Select Module Bindings: " librarian--config-modules-cache :require-match t))
         (binding (f-join librarian--config-modules-dir chosen librarian--config-binding-file-name))
        )
    (if (f-exists? binding)
      (find-file binding)
      (message "Doesnt Exist: %s" binding)
      )
    )
  )

(defun librarian-configs--edit-vars ()
  (interactive)
  (unless librarian--config-modules-cache (librarian--config--build-modules-cache))
  (let* ((vars (-select #'(lambda (x) (f-exists? (f-join librarian--config-modules-dir  x librarian--config-var-file-name))) librarian--config-modules-cache))
         (chosen (ivy-read "Select Module Vars: "  librarian--config-modules-cache :require-match t))
         (varf (f-join librarian--config-modules-dir  chosen librarian--config-var-file-name))
        )
    (if (f-exists? varf)
      (find-file varf)
      (message "Doesnt Exist: %s" varf)
      )
    )
  )

(defun librarian-configs--edit-config ()
  (interactive)
  (unless librarian--config-modules-cache (librarian--config--build-modules-cache))
  (let* ((config (-select #'(lambda (x) (f-exists? (f-join librarian--config-modules-dir  x librarian--config-config-file-name))) librarian--config-modules-cache))
         (chosen (ivy-read "Select Module Config: " librarian--config-modules-cache :require-match t))
         (binding (f-join librarian--config-modules-dir  chosen librarian--config-config-file-name))
        )
    (if (f-exists? binding)
      (find-file binding)
      (message "Doesnt Exist: %s" binding)
      )
    )
  )

(defun librarian-configs--edit-spec-defs ()
  (interactive)
  (unless librarian--config-modules-cache (librarian--config--build-modules-cache))
  (let* ((config (-select #'(lambda (x) (f-exists? (f-join librarian--config-modules-dir  x librarian--config-config-file-name))) librarian--config-modules-cache))
         (chosen (ivy-read "Select Module Config: " librarian--config-modules-cache :require-match t))
         (binding (f-join librarian--config-modules-dir  chosen librarian--config-spec-def-file-name))
        )
    (if (f-exists? binding)
      (find-file binding)
      (message "Doesnt Exist: %s" binding)
      )
    )
  )

(provide 'librarian--config)
;;; librarian--config.el ends here
