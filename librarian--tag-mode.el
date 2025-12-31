 ;;; librarian-tag.el -*- lexical-binding: t; no-byte-compile: t;-*-

(eval-when-compile
  (require 'cl-lib)
  (require 'evil)
  (require 'f)
  (require 'librarian--tag)
  )

(defvar librarian-tag-mode-substitution-sources  nil)

(defvar librarian-tag-mode-main-loc         nil)

(defvar librarian-tag-mode-all-tags         nil)

;;-- mode def

(define-minor-mode librarian-tag-mode
  "  "
  :init-value nil
  :lighter "LibTag"
  ;; :global t
  ;; :keymap nil

  )

(defun librarian--tag-mode/turn-on ()
  (unless (minibufferp)
    (librarian-tag-mode 1)
    )
  )

(define-globalized-minor-mode global-librarian-tagging-mode librarian-tag-mode librarian--tag-mode/turn-on)

;;-- end mode def

(defun librarian-tag-mode-random-selection  (n)
  (interactive "nHow many tags? ")
  (let* ((tags (hash-table-keys librarian--tag-global-tags))
         (selection (mapcar (lambda (x) (seq-random-elt tags)) (make-list n ?a)))
         )
    (with-temp-buffer-window "*Rand Tags*"
                             'display-buffer-pop-up-frame
                             nil
                             (mapc (lambda (x) (princ x ) (princ "\n")) selection)
                             )
    )
  )

(defun librarian-tag-mode-cache-misses (maybes)
  "Get tags being added which aren't in the global cache"
  (-reject (-rpartial #'gethash librarian--tag-global-tags nil) maybes)
  )

(defun librarian-tag-mode-cache-update-global-tags (update)
  "Called with new tags to update the global tags hashtable "
  (let ((delta (librarian--tag-get-delta update))
        )
    (cl-loop for tag in (car delta) ;; increment these
             do
             (puthash tag (1+ (gethash tag librarian--tag-global-tags 0)) librarian--tag-global-tags)
             )
    (cl-loop for tag in (cadr delta) ;; subtract these
             do
             (puthash tag (1- (gethash tag librarian--tag-global-tags 1)) librarian--tag-global-tags)
             )
    )
  )

(defun librarian-tag-mode-set-tags (new)
  "Utility action to set tags.
Implement a cl-defmethod `librarian-set-tags' ((mode (eql '{}))) to use,
and `librarian-set-new-tags'.

Can set multiple sections of entries, moving by `evil-backward-section-begin'

Returns a list of normalized, new tags that were not in the global cache
 "
  (save-excursion
    (let* ((new (librarian-normalize-tags major-mode new))
           (cache-misses (librarian-tag-mode-cache-misses new))
           start-pos
          )
      (cond ((eq evil-state 'visual)
             (setq start-pos evil-visual-beginning)
             (move-marker librarian--tag-marker evil-visual-end))
            (t
             (setq start-pos (line-beginning-position))
             (move-marker librarian--tag-marker (line-end-position)))
            )
      (goto-char librarian--tag-marker)
      (while (< start-pos (point))
        (cond ((null (save-excursion (librarian-tag-mode-get-tags)))
               (librarian-set-new-tags major-mode new))
              (t
               (apply #'librarian-set-tags
                        major-mode
                        (librarian--tag-get-delta new)
                        ))
              )
        (librarian-tag-mode-cache-update-global-tags new)
        (librarian-cache-tags major-mode new)
        (librarian-backward-entry major-mode)
        )
      cache-misses
      )
    )
  )

(defun librarian-tag-mode-get-tags ()
  "Utility action to get tags for current entry.
updates `librarian--tag--current-entry-tags'
Implement a cl-defmethod `librarian-get-tags` ((mode (eql '{}))) to use
returns the list of tags extracted
"
  (save-excursion
    (setq librarian--tag--current-entry-tags
          (librarian-normalize-tags major-mode (librarian-get-tags major-mode)))
       )
  librarian--tag--current-entry-tags
  )

(defun librarian-tag-mode-get-buffer-tags (&optional buffer)
  " sets `librarian--tag--current-buffer-tags'
Implement a cl-defmethod `librarian-get-buffer-tags` ((mode (eql '{}))) to use
returns the list of tags extracted
"
  (setq librarian--tag--current-buffer-tags
        (with-current-buffer (or buffer (current-buffer))
          (save-excursion
            (librarian-normalize-tags major-mode (librarian-get-buffer-tags major-mode))))
        )
  librarian--tag--current-buffer-tags
  )

(defun librarian-tag-mode-parse-tag-file (path)
  " parse a file of tags and insert them into the global tag hash "
  (with-temp-buffer
    (insert-file path)
    (goto-char (point-min))
    (while (< (point) (point-max))
      (let ((tagline (split-string (buffer-substring (line-beginning-position) (line-end-position))
                                   ":" nil " +")))
        (unless (or (> (length tagline) 2) (string-empty-p (car tagline)))
          (puthash (car tagline) (string-to-number (or (cadr tagline) "1"))
                   librarian--tag-global-tags)))
      (forward-line)
      )
    )
  )

(defun librarian-tag-mode-rebuild-tag-database ()
  "Rebuild the tag database from librarian-tag-mode-main-loc"
  (interactive)
  (clrhash librarian--tag-global-tags)
  (cond ((not librarian-tag-mode-main-loc)
         (message "no tags location is specified"))
        ((not (f-exists? librarian-tag-mode-main-loc))
         (message "tags location does not exist : %s" librarian-tag-mode-main-loc))
        ((f-dir? librarian-tag-mode-main-loc)
         (let ((files (f-entries librarian-tag-mode-main-loc
                                 (-rpartial 'f-ext? "sub")
                                 t)))
           (message "Tags location is a directory, reading files")
           (cl-loop for file in files
                    do
                    (librarian-tag-mode-parse-tag-file file))
           ))
        ((f-file? librarian-tag-mode-main-loc)
         (librarian-tag-mode-parse-tag-file librarian-tag-mode-main-loc))
        (t (error "Unkown tag rebuild state"))
        )
  )

(provide 'librarian--tag-mode)
;;; librarian--tag-mode.el ends here
