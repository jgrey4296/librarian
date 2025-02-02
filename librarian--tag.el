 ;;; librarian-tag.el -*- lexical-binding: t; no-byte-compile: t;-*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: March 23, 2023
;; Modified: March 23, 2023
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
;;  A minor mode that allows tagging constructs in any mode it has handlers for
;;
;;; Code:

;;-- end header

(eval-when-compile
  (require 'cl-lib)
  (require 'evil)
  (require 'f)
  )

(defvar lit-global-tags      (make-hash-table :test 'equal))

(defvar-local lit-local-tags (make-hash-table :test 'equal))

(defvar lit-marker           (make-marker) "a marker for where the region to tag ends")

(defvar lit--current-entry-tags nil)

(defvar lit--current-buffer-tags nil)

;;-- api
(cl-defgeneric librarian-set-tags (mode add sub keep)
  "The generic method, called in the current buffer, with save-excursion,
   of new::list tags to add to the current entry,
   and the current::list tags the entry already has.
   returns success::bool"
)

(cl-defgeneric librarian-set-new-tags (mode new)
  "The generic method, called in the current buffer, with-save-excursion,
 to add tags to an entry with no tags currently
  returns success::bool")

(cl-defgeneric librarian-get-tags (mode)
  "The generic method, called in the current buffer, with-save-excursion,
  to get all used tags for the current entry
  return tags::list[str]
  ")

(cl-defgeneric librarian-get-buffer-tags (mode)
  "The generic method, called with the relevant buffer as current, with save-excursion,
  to get that buffers entire used tag set
  returns tags::list")

(cl-defgeneric librarian-normalize-tags (mode tags)
  "Generic method to normalize a list tags,
  returns a list of tags.
  default implementation trims, replaces whitespaces,
  and removes duplicates
")

(cl-defgeneric librarian-cache-tags (mode new)
  "Mode specific tag caching"
  )

(cl-defgeneric librarian-backward-entry (mode)
  "method to move back to the next entry to tag,
  by default is `evil-backward-section-begin' twice"
  )
;;-- end api

;;-- defaults

(cl-defmethod librarian-set-tags (mode add sub keep)
  (warn "No Librarian Set Tags handler defined for: %s" mode))

(cl-defmethod librarian-set-new-tags (mode new)
  (warn "No Librarian-set-new-tag handler defined for: %s" mode))

(cl-defmethod librarian-get-tags (_)
  nil)

(cl-defmethod librarian-get-buffer-tags (mode)
  (warn "No Librarian-get-buffer-tags handler defined for: %s" mode))

(cl-defmethod librarian-normalize-tags (_ tags)
  "default implementation of tag normalization.
  trims whitespace, replaces internal whitespace with underscores,
 sorts alphabetically, and removes duplicates
"
  (seq-uniq
   (sort
    (mapcar #'(lambda (x)
                (s-replace-regexp "\s+" "_"
                                  (string-trim x)))
            tags)
    #'string-lessp)
   )
  )

(cl-defmethod librarian-cache-tags (_ new)
  nil
  )

(cl-defmethod librarian-backward-entry (_)
  (evil-backward-section-begin 2)
  )
;;-- end defaults

(defun lit-get-delta (new)
  "Given a list of normalized change tags,
returns a triple of (add sub keep), against the current entry tags "
  (let ((add (seq-difference new lit--current-entry-tags))
        (sub (seq-intersection new lit--current-entry-tags))
        (keep (seq-difference lit--current-entry-tags new))
        )
    (list add sub keep)
    )
  )

(provide 'librarian--tag)
;;; librarian-tag-mode.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("lit-" . "librarian--tag-")
;; )
;; End:
