;;; librarian-regular.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'dash)
  (require 's)
  (require 'cl-lib)
  (require 'f)
  (require 'parent-mode)
  )

(defconst lib-splitter "#")

(defvar lib-cache (make-hash-table))

(defvar librarian-regular-loc nil)

(defvar-local lib-targets nil)

(defun librarian--regular-offset (str) ;; -> str
  (let* ((max-line 50)
         (len (length str))
         (amnt (- max-line len))
         )
    (if (< 0 amnt)
        (make-string amnt ? )
      (make-string 10 ? )
      )
    )
  )

;;;###autoload
(define-minor-mode librarian-regular-minor-mode
  " for all modes in (parent-mode-list major-mode) load any
files of urls in librarian-regular-loc,
Use librarian-regular-go to choose one of those urls and jump to it
 "
  :init-value nil
  :lighter "lib-regular"
  ;; :global t
  :keymap nil
  (if (or (not librarian-regular-loc) (not (f-exists? librarian-regular-loc)))
      (message "Lib-Regular location doesn't exist: %s" librarian-regular-loc)
    (setq-local lib-targets
                ;; Loop for each active mode
                (cl-remove-duplicates
                 (cl-loop for mode in (append (parent-mode-list major-mode) '(fundamental-mode) local-minor-modes global-minor-modes)
                          for source-exists = (and librarian-regular-loc (f-exists? (f-join librarian-regular-loc (symbol-name mode))))
                          when (and source-exists (not (gethash mode lib-cache)))
                          do ;; load the source file
                          (puthash mode (lib--load-file (f-join librarian-regular-loc (symbol-name mode))) lib-cache)
                          ;; and construct the result list
                          when source-exists append (gethash mode lib-cache)
                          )
                 :test #'equal)
                )
    )
  )

(defun lib--load-file (file)
  "read a list of (name . url) from the given file"
  (cl-assert (f-exists? file) t "Lib-Regular Load File Check")
  (let ((max-line 40)
        targets)
    (with-temp-buffer
      (insert-file-contents file)
      (mapc #'(lambda (x)
                (-when-let* ((vals (split-string x lib-splitter t " +"))
                             (offset (librarian--regular-offset (car vals)))
                             )
                  (push (cons (format "%s%s(%s)"
                                      (car vals)
                                      offset
                                      (f-filename file)
                                      )
                              (cadr vals)) targets)
                  ))
            (s-lines (buffer-substring-no-properties (point-min) (point-max)))
            )
      )
    targets
    )
  )

(defun lib-minor-mode/turn-on ()
  (unless (minibufferp)
    (librarian-regular-minor-mode 1))
  )

;;;###autoload
(define-globalized-minor-mode global-librarian-regular-minor-mode librarian-regular-minor-mode lib-minor-mode/turn-on)

;;;###autoload (autoload 'librarian--regular-go "librarian--regular")
(defun lib-go ()
  " suggest a list of regular urls to browse to "
  (interactive)
  (unless librarian-regular-minor-mode
    (user-error "Lib-Regular Mode isn't Active")
    )
  (ivy-read "Lookup: "
            lib-targets
            :require-match t
            :sort t
            ;; TODO use browse handler
            :action #'(lambda (x) (browse-url (cdr x)))
            )
)

;;;###autoload (autoload 'librarian--regular-clear "librarian--regular")
(defun lib-clear ()
  " Clear the cache of librarian-regular "
  (interactive)
  (clrhash lib-cache)
  )

(provide 'librarian--regular)
;;; librarian--regular.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("lib-" . "librarian--regular-")
;; )
;; End:
