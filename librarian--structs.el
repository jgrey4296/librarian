;;; librarian--structs.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(defconst lis-types '(:online :doc :tag))

(cl-defstruct lis-backend
  "A Registered backend"
  (:id   nil  :type 'symbol)
  (:type nil  :type 'list-types)
  (:mod  nil  :type 'symbol)
  (:fn   nil  :type 'lambda)
  )

(provide 'librarian--structs)

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    December 15, 2024
;; Modified:   December 15, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; librarian--structs.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("lis-" . "librarian--structs-")
;; ("lib-" . "librarian-")
;; )
;; End:
