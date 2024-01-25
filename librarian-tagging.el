 ;;; librarian-tagging-mode.el -*- lexical-binding: t; no-byte-compile: t;-*-
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

(require 'evil)
(require 'f)

(defvar librarian-tagging-mode-global-tags      (make-hash-table :test 'equal))

(defvar-local librarian-tagging-mode-local-tags (make-hash-table :test 'equal))

(defvar librarian-tagging-mode-marker           (make-marker))

(defvar librarian-tagging-mode-substitution-sources  nil)

(defvar librarian-tagging-mode-main-loc         nil)

(defvar librarian-tagging-mode-all-tags         nil)

(defvar-local librarian-tagging-mode-handlers '(:new librarian-tagging-mode-new-tag-default
                                                :set librarian-tagging-mode-set-tags-default
                                                :get librarian-tagging-mode-get-tag-default
                                                :buff librarian-tagging-mode-buffer-tags-default
                                                )
  "A plist of handlers for tagging")

;;-- mode def

(define-minor-mode librarian-tagging-mode
  "  "
  :init-value nil
  :lighter "tagging"
  ;; :global t
  ;; :keymap nil

  )

(defun librarian-tagging-mode/turn-on ()
  (unless (minibufferp)
    (librarian-tagging-mode 1)
    )
  )

(define-globalized-minor-mode global-librarian-tagging-mode librarian-tagging-mode librarian-tagging-mode/turn-on)

;;-- end mode def

(defun librarian-tagging-mode-random-selection  (n)
  (interactive "nHow many tags? ")
  (let* ((tags (hash-table-keys librarian-tagging-mode-global-tags))
         (selection (mapcar (lambda (x) (seq-random-elt tags)) (make-list n ?a)))
         )
    (with-temp-buffer-window "*Rand Tags*"
                             'display-buffer-pop-up-frame
                             nil
                             (mapc (lambda (x) (princ x ) (princ "\n")) selection)
                             )
    )
  )

;;-- defaults

(defun librarian-tagging-mode-set-tags-default (x)
  (warn "Default Set Tags")
  )

(defun librarian-tagging-mode-new-tag-default (x)
  (warn "Default New Tag")
  )

(defun librarian-tagging-mode-get-tag-default ()
  (warn "Default Get Tag")
  )

(defun librarian-tagging-mode-buffer-tags-default ()
  (warn "Default Tag Bugger")
  )

;;-- end defaults


(defun librarian-tagging-mode-set-tags (x)
  "Utility action to set tags. Works in org *and* bibtex files"
  (save-excursion
    (funcall (plist-get (buffer-local-value 'librarian-tagging-mode-handlers (current-buffer)) :set) x)
    )
  )

(defun librarian-tagging-mode-set-new-tag (x)
  "Utility action to add a new tag. Works for org *and* bibtex"
    (save-excursion
      (funcall (plist-get (buffer-local-value 'librarian-tagging-mode-handlers (current-buffer)) :new) x)
    )
  )

(defun librarian-tagging-mode-get-tags ()
  "Utility action to get tags for current entry"
     (save-excursion
       (funcall (plist-get (buffer-local-value 'librarian-tagging-mode-handlers (current-buffer)) :get))
       )
   )

(defun librarian-tagging-mode-get-buffer-tags ()
     (save-excursion
       (funcall (plist-get (buffer-local-value 'librarian-tagging-mode-handlers (current-buffer)) :buff))
       )
   )

(defun librarian-tagging-mode-parse-tag-file (path)
  " parse a file of tags and insert them into the global tag hash "
  (with-temp-buffer
    (insert-file path)
    (goto-char (point-min))
    (while (< (point) (point-max))
      (let ((tagline (split-string (buffer-substring (line-beginning-position) (line-end-position))
                                   ":" nil " +")))
        (unless (or (> (length tagline) 2) (string-empty-p (car tagline)))
          (puthash (car tagline) (string-to-number (or (cadr tagline) "1"))
                   librarian-tagging-mode-global-tags)))
      (forward-line)
      )
    )
  )

(defun librarian-tagging-mode-rebuild-tag-database ()
  "Rebuild the tag database from librarian-tagging-mode-main-loc"
  (interactive)
  (clrhash librarian-tagging-mode-global-tags)
  (cond ((not (f-exists? librarian-tagging-mode-main-loc))
         (error "ERROR: GLOBAL-TAGS-LOCATION IS EMPTY")
         )
        ((f-dir? librarian-tagging-mode-main-loc)
         (let ((files (f-entries librarian-tagging-mode-main-loc
                                 (-rpartial 'f-ext? "sub")
                                 t)))
           (message "Got Dir")
           (cl-loop for file in files
                    do
                    (librarian-tagging-mode-parse-tag-file file))
           ))
        ((f-file? librarian-tagging-mode-main-loc)
         (librarian-tagging-mode-parse-tag-file librarian-tagging-mode-main-loc))
        (t (message "ERROR: GLOBAL-TAGS-LOCATION IS EMPTY"))
        )
  )

(provide 'librarian-tagging)
;;; librarian-tagging-mode.el ends here
