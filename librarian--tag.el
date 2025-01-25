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

(defvar litm-substitution-sources  nil)

(defvar litm-main-loc         nil)

(defvar litm-all-tags         nil)

(defvar lit--current-entry-tags nil)

(defvar lit--current-buffer-tags nil)

;;-- mode def

(define-minor-mode librarian-tag-mode
  "  "
  :init-value nil
  :lighter "LibTag"
  ;; :global t
  ;; :keymap nil

  )

(defun lit-mode/turn-on ()
  (unless (minibufferp)
    (librarian-tag-mode 1)
    )
  )

(define-globalized-minor-mode global-librarian-tagging-mode librarian-tag-mode lit-mode/turn-on)

;;-- end mode def

(defun litm-random-selection  (n)
  (interactive "nHow many tags? ")
  (let* ((tags (hash-table-keys lit-global-tags))
         (selection (mapcar (lambda (x) (seq-random-elt tags)) (make-list n ?a)))
         )
    (with-temp-buffer-window "*Rand Tags*"
                             'display-buffer-pop-up-frame
                             nil
                             (mapc (lambda (x) (princ x ) (princ "\n")) selection)
                             )
    )
  )

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

(defun litm-cache-global-tags (new)
  "Called with new tags to update the global tags hashtable"
  (let ((delta (litm--get-delta new)))
    (cl-loop for tag in (car delta)
             do
             (puthash tag (1+ (gethash tag lit-global-tags 0)) lit-global-tags)
             )
    (cl-loop for tag in (cadr delta)
             do
             (puthash tag (1- (gethash tag lit-global-tags 1)) lit-global-tags)
             )
    )
  )

(defun litm--get-delta (new)
  "Given a list of normalized change tags,
returns a triple of (add sub keep), against the current entry tags "
  (let ((add (seq-difference new lit--current-entry-tags))
        (sub (seq-intersection new lit--current-entry-tags))
        (keep (seq-difference lit--current-entry-tags new))
        )
    (list add sub keep)
    )
  )

(defun litm-set-tags (new)
  "Utility action to set tags.
Implement a cl-defmethod `librarian-set-tags' ((mode (eql '{}))) to use,
and `librarian-set-new-tags'.

Can set multiple sections of entries, moving by `evil-backward-section-begin'

 "
  (save-excursion
    (let ((new (librarian-normalize-tags major-mode new))
          start-pos
          )
      (cond ((eq evil-state 'visual)
             (setq start-pos evil-visual-beginning)
             (move-marker lit-marker evil-visual-end))
            (t
             (setq start-pos (line-beginning-position))
             (move-marker lit-marker (line-end-position)))
            )
      (goto-char lit-marker)
      (while (< start-pos (point))
        (cond ((null (save-excursion (litm-get-tags)))
               (librarian-set-new-tags major-mode new))
              (t
               (apply #'librarian-set-tags
                        major-mode
                        (litm--get-delta new)
                        ))
              )
        (librarian-cache-tags major-mode new)
        (litm-cache-global-tags new)
        (librarian-backward-entry major-mode)
        )
      )
    )
)

(defun litm-get-tags ()
  "Utility action to get tags for current entry.
updates `lit--current-entry-tags'
Implement a cl-defmethod `librarian-get-tags` ((mode (eql '{}))) to use
returns the list of tags extracted
"
  (save-excursion
    (setq lit--current-entry-tags
          (librarian-normalize-tags major-mode (librarian-get-tags major-mode)))
       )
  lit--current-entry-tags
  )

(defun litm-get-buffer-tags (&optional buffer)
  " sets `lit--current-buffer-tags'
Implement a cl-defmethod `librarian-get-buffer-tags` ((mode (eql '{}))) to use
returns the list of tags extracted
"
  (setq lit--current-buffer-tags
        (with-current-buffer (or buffer (current-buffer))
          (save-excursion
            (librarian-normalize-tags major-mode (librarian-get-buffer-tags major-mode))))
        )
  lit--current-buffer-tags
  )

(defun litm-parse-tag-file (path)
  " parse a file of tags and insert them into the global tag hash "
  (with-temp-buffer
    (insert-file path)
    (goto-char (point-min))
    (while (< (point) (point-max))
      (let ((tagline (split-string (buffer-substring (line-beginning-position) (line-end-position))
                                   ":" nil " +")))
        (unless (or (> (length tagline) 2) (string-empty-p (car tagline)))
          (puthash (car tagline) (string-to-number (or (cadr tagline) "1"))
                   lit-global-tags)))
      (forward-line)
      )
    )
  )

(defun litm-rebuild-tag-database ()
  "Rebuild the tag database from litm-main-loc"
  (interactive)
  (clrhash lit-global-tags)
  (cond ((not litm-main-loc)
         (message "no tags location is specified"))
        ((not (f-exists? litm-main-loc))
         (message "tags location does not exist : %s" litm-main-loc))
        ((f-dir? litm-main-loc)
         (let ((files (f-entries litm-main-loc
                                 (-rpartial 'f-ext? "sub")
                                 t)))
           (message "Tags location is a directory, reading files")
           (cl-loop for file in files
                    do
                    (litm-parse-tag-file file))
           ))
        ((f-file? litm-main-loc)
         (litm-parse-tag-file litm-main-loc))
        (t (error "Unkown tag rebuild state"))
        )
  )


;; Public Aliases

(provide 'librarian--tag)
;;; librarian-tag-mode.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("lit-" . "librarian--tag-")
;; ("litm-" . "librarian-tag-mode-")
;; )
;; End:
