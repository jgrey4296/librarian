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
  (require 'librarian--tag)
  )

(defvar litm-substitution-sources  nil)

(defvar litm-main-loc         nil)

(defvar litm-all-tags         nil)

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

(defun litm-cache-misses (maybes)
  "Get tags being added which aren't in the global cache"
  (-reject (-rpartial #'gethash librarian--tag-global-tags nil) maybes)
  )

(defun litm-cache-update-global-tags (update)
  "Called with new tags to update the global tags hashtable "
  (let ((delta (lit-get-delta update))
        )
    (cl-loop for tag in (car delta) ;; increment these
             do
             (puthash tag (1+ (gethash tag lit-global-tags 0)) lit-global-tags)
             )
    (cl-loop for tag in (cadr delta) ;; subtract these
             do
             (puthash tag (1- (gethash tag lit-global-tags 1)) lit-global-tags)
             )
    )
  )

(defun litm-set-tags (new)
  "Utility action to set tags.
Implement a cl-defmethod `librarian-set-tags' ((mode (eql '{}))) to use,
and `librarian-set-new-tags'.

Can set multiple sections of entries, moving by `evil-backward-section-begin'

Returns a list of normalized, new tags that were not in the global cache
 "
  (save-excursion
    (let* ((new (librarian-normalize-tags major-mode new))
           (cache-misses (litm-cache-misses new))
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
                        (lit-get-delta new)
                        ))
              )
        (litm-cache-update-global-tags new)
        (librarian-cache-tags major-mode new)
        (librarian-backward-entry major-mode)
        )
      cache-misses
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

(provide 'librarian--tag-mode)
;;; librarian-tag-mode.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("lit-" . "librarian--tag-")
;; ("litm-" . "librarian-tag-mode-")
;; )
;; End:
