;;; librarian--structs.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defconst lis-types '(:online :doc :tag))

(cl-defstruct lis-backend
  "A Registered backend"
  (:id   nil  :type 'symbol)
  (:type nil  :type 'list-types)
  (:mod  nil  :type 'symbol)
  (:fn   nil  :type 'lambda)
  )

(provide 'librarian--structs)
;;; librarian--structs.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("lis-" . "librarian--structs-")
;; ("lib-" . "librarian-")
;; )
;; End:
