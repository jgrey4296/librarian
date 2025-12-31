;;; librarian-regular.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'dash)
  (require 's)
  (require 'cl-lib)
  (require 'f)
  (require 'parent-mode)
  )

(defconst librarian--regular-splitter "#")

(defvar librarian--regular-cache (make-hash-table))

(defvar librarian-regular-loc nil)

(defvar-local librarian--regular-targets nil)

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
  :lighter "librarian--regular-regular"
  ;; :global t
  :keymap nil
  (cond ((null librarian-regular-loc) t)
        ((and librarian-regular-loc (not (f-exists? librarian-regular-loc)))
         (message "Lib-Regular location doesn't exist: %s" librarian-regular-loc))
        (t
         (setq-local librarian--regular-targets
                ;; Loop for each active mode
                (cl-remove-duplicates
                 (cl-loop for mode in (append (parent-mode-list major-mode) '(fundamental-mode) local-minor-modes global-minor-modes)
                          for source-file = (f-join librarian-regular-loc (symbol-name mode))
                          for source-exists = (and librarian-regular-loc (f-exists? source-file))
                          when (and source-exists (not (gethash mode librarian--regular-cache)))
                          do ;; load the source file
                          (puthash mode (librarian--regular--load-file source-file) librarian--regular-cache)
                          ;; and construct the result list
                          when source-exists append (gethash mode librarian--regular-cache)
                          )
                 :test #'equal)
                )
         )
    )
  )

(defun librarian--regular--load-file (file)
  "read a list of (name . url) from the given file"
  (cl-assert (f-exists? file) t "Lib-Regular Load File Check")
  (let ((max-line 40)
        targets)
    (with-temp-buffer
      (insert-file-contents file)
      (mapc #'(lambda (x)
                (-when-let* ((vals (split-string x librarian--regular-splitter t " +"))
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

(defun librarian--regular-minor-mode/turn-on ()
  (unless (minibufferp)
    (librarian-regular-minor-mode 1))
  )

;;;###autoload
(define-globalized-minor-mode global-librarian-regular-minor-mode librarian-regular-minor-mode librarian--regular-minor-mode/turn-on)

;;;###autoload (autoload 'librarian--regular-go "librarian--regular")
(defun librarian--regular-go ()
  " suggest a list of regular urls to browse to "
  (interactive)
  (unless librarian-regular-minor-mode
    (user-error "Lib-Regular Mode isn't Active")
    )
  (ivy-read "Lookup: "
            librarian--regular-targets
            :require-match t
            :sort t
            ;; TODO use browse handler
            :action #'(lambda (x) (browse-url (cdr x)))
            )
)

;;;###autoload (autoload 'librarian--regular-clear "librarian--regular")
(defun librarian--regular-clear ()
  " Clear the cache of librarian-regular "
  (interactive)
  (clrhash librarian--regular-cache)
  )

(provide 'librarian--regular)
;;; librarian--regular.el ends here
