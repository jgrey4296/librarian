;;; librarian-regular.el -*- lexical-binding: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: April 20, 2023
;; Modified: April 20, 2023
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
;;
;;
;;; Code:

;;-- end header

(eval-when-compile
  (require 'dash)
  (require 's)
  (require 'cl-lib)
  (require 'f)
  (require 'parent-mode)
  )

(defconst lib-splitter "#")

(defvar lib-cache (make-hash-table))

(defvar lib-location nil)

(defvar-local lib-targets nil)

;;;###autoload
(define-minor-mode librarian-regular-minor-mode
  " for all modes in (parent-mode-list major-mode) load any
files of urls in lib-location,
Use librarian-regular-go to choose one of those urls and jump to it
 "
  :init-value nil
  :lighter "lib-regular"
  ;; :global t
  :keymap nil
  (if (or (not lib-location) (not (f-exists? lib-location)))
      (message "Lib-Regular location doesn't exist: %s" lib-location)
    (setq-local lib-targets
                ;; Loop for each active mode
                (cl-remove-duplicates
                 (cl-loop for mode in (append (parent-mode-list major-mode) '(fundamental-mode) local-minor-modes global-minor-modes)
                          for source-exists = (and lib-location (f-exists? (f-join lib-location (symbol-name mode))))
                          when (and source-exists (not (gethash mode lib-cache)))
                          do ;; load the source file
                          (puthash mode (lib--load-file (f-join lib-location (symbol-name mode))) lib-cache)
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
  (let (targets)
    (with-temp-buffer
      (insert-file-contents file)
      (mapc #'(lambda (x)
                (-when-let (vals (split-string x lib-splitter t " +"))
                  (push (cons (car vals) (cadr vals)) targets)
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
;;; librarian-regular.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("lib-" . "librarian--regular-")
;; )
;; End:
