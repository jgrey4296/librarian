;;; libarian-file-management.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey@Johns-Mac-mini.local>
;; Created: September 04, 2023
;; Modified: September 04, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.2))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  TODO for editing various librarian file formats
;;  ie: bibtex, regular-url files, bookmarks
;;
;;; Code:
;;-- end header

(defvar lif-defaults '(librarian--backend--bug-reference
                       librarian--backend--ffap))

;; TODO add management of gtags


(provide 'librarian--file)
;;; libarian-file-management.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("lif-" . "librarian--file-")
;; )
;; End:
