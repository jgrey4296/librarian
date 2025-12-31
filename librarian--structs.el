;;; librarian--structs.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defconst librarian--structs-types '(:online :doc :tag))

(cl-defstruct librarian--structs-backend
  "A Registered backend"
  (:id   nil  :type 'symbol)
  (:type nil  :type 'list-types)
  (:mod  nil  :type 'symbol)
  (:fn   nil  :type 'lambda)
  )

(provide 'librarian--structs)
;;; librarian--structs.el ends here
