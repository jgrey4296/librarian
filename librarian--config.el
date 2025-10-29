;;; ivy.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib)
  (require 'f)
  (declare-function ivy-read "ivy")
  )

(defvar lic-modules-dir nil)

(defvar lic-modules-cache nil)

(defvar lic-binding-file-name "+bindings.el")

(defvar lic-config-file-name "config.el")

(defvar lic-var-file-name "+vars.el")

(defvar lic-spec-def-file-name "+spec-defs.el")

(defun lic--build-modules-cache ()
  (let* ((root lic-modules-dir)
         (groups (f-directories lic-modules-dir))
         (modules (cl-loop for group in groups
                           append (mapcar #'(lambda (x) (f-join (f-base group) (f-base x)))
                                          (f-directories group))))
         )
         (setq lic-modules-cache modules)
         )
    )

(defun librarian-configs--edit-bindings ()
  (interactive)
  (unless lic-modules-cache (lic--build-modules-cache))
  (let* ((bindings (-select #'(lambda (x) (f-exists? (f-join lic-modules-dir  x lic-binding-file-name))) lic-modules-cache))
         (chosen (ivy-read "Select Module Bindings: " lic-modules-cache :require-match t))
         (binding (f-join lic-modules-dir chosen lic-binding-file-name))
        )
    (if (f-exists? binding)
      (find-file binding)
      (message "Doesnt Exist: %s" binding)
      )
    )
  )

(defun librarian-configs--edit-vars ()
  (interactive)
  (unless lic-modules-cache (lic--build-modules-cache))
  (let* ((vars (-select #'(lambda (x) (f-exists? (f-join lic-modules-dir  x lic-var-file-name))) lic-modules-cache))
         (chosen (ivy-read "Select Module Vars: "  lic-modules-cache :require-match t))
         (varf (f-join lic-modules-dir  chosen lic-var-file-name))
        )
    (if (f-exists? varf)
      (find-file varf)
      (message "Doesnt Exist: %s" varf)
      )
    )
  )

(defun librarian-configs--edit-config ()
  (interactive)
  (unless lic-modules-cache (lic--build-modules-cache))
  (let* ((config (-select #'(lambda (x) (f-exists? (f-join lic-modules-dir  x lic-config-file-name))) lic-modules-cache))
         (chosen (ivy-read "Select Module Config: " lic-modules-cache :require-match t))
         (binding (f-join lic-modules-dir  chosen lic-config-file-name))
        )
    (if (f-exists? binding)
      (find-file binding)
      (message "Doesnt Exist: %s" binding)
      )
    )
  )

(defun librarian-configs--edit-spec-defs ()
  (interactive)
  (unless lic-modules-cache (lic--build-modules-cache))
  (let* ((config (-select #'(lambda (x) (f-exists? (f-join lic-modules-dir  x lic-config-file-name))) lic-modules-cache))
         (chosen (ivy-read "Select Module Config: " lic-modules-cache :require-match t))
         (binding (f-join lic-modules-dir  chosen lic-spec-def-file-name))
        )
    (if (f-exists? binding)
      (find-file binding)
      (message "Doesnt Exist: %s" binding)
      )
    )
  )

(provide 'librarian--config)
;;; librarian--config.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("lic-" . "librarian--config-")
;; )
;; End:
