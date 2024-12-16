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
  )

(defconst lir--splitter "#")

(defvar lir-cache (make-hash-table))

(defvar lir-location nil)

(defvar-local lir-targets nil)

(define-minor-mode librarian-regular-minor-mode
  " for all modes in (parent-mode-list major-mode) load any
files of urls in lir-location "
  :init-value nil
  :lighter "librarian-regular"
  ;; :global t
  :keymap nil
  (setq-local lir-targets
              ;; Loop for each active mode
              (cl-remove-duplicates
               (cl-loop for mode in (append (parent-mode-list major-mode) '(fundamental-mode) local-minor-modes global-minor-modes)
                        for source-exists = (f-exists? (f-join lir-location (symbol-name mode)))
                        when (and source-exists (not (gethash mode lir-cache)))
                        do ;; load the source file
                        (puthash mode (lir--load-file (f-join lir-location (symbol-name mode))) lir-cache)
                        ;; and construct the result list
                        when source-exists append (gethash mode lir-cache)
                        )
               :test #'equal)
              )
  )

(defun lir--load-file (file)
  "read a list of (name . url) from the given file"
  (let (targets)
    (with-temp-buffer
      (insert-file-contents file)
      (mapc #'(lambda (x)
                (-when-let (vals (split-string x lir--splitter t " +"))
                  (push (cons (car vals) (cadr vals)) targets)
                  ))
            (s-lines (buffer-substring-no-properties (point-min) (point-max)))
            )
      )
    targets
    )
  )

(defun lir-minor-mode/turn-on ()
  (unless (minibufferp)
    (librarian-regular-minor-mode 1))
  )

(define-globalized-minor-mode global-librarian-regular-minor-mode librarian-regular-minor-mode lir-minor-mode/turn-on)

(defun librarian-regular-go ()
  " suggest a list of regular urls to browse to "
  (interactive)
  (ivy-read "Lookup: "
            lir-targets
            :require-match t
            :sort t
            ;; TODO use browse handler
            :action #'(lambda (x) (browse-url (cdr x)))
            )
)

(defun librarian-regular-clear ()
  " Clear the cache of librarian-regular "
  (interactive)
  (clrhash lir-cache)
  )

(provide 'librarian--regular)
;;; librarian-regular.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("lir-" . "librarian--regular-")
;; )
;; End:
