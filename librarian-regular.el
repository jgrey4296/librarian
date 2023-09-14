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

(defconst librarian-regular--splitter "#")

(defvar librarian-regular--cache (make-hash-table))

(defvar librarian-regular--location nil)

(defvar-local librarian-regular--targets nil)

(define-minor-mode librarian-regular-minor-mode
  " for all modes in (parent-mode-list major-mode) load any
files of urls in librarian-regular--location "
  :init-value nil
  :lighter "librarian-regular"
  ;; :global t
  :keymap nil
  (setq-local librarian-regular--targets
              (cl-loop for mode in (append (parent-mode-list major-mode) '(fundamental-mode) local-minor-modes global-minor-modes)
                       when (f-exists? (f-join librarian-regular--location (symbol-name mode)))
                       do
                       (unless (gethash mode librarian-regular--cache)
                         (puthash mode (librarian-regular--load-file (f-join librarian-regular--location (symbol-name mode)))
                                  librarian-regular--cache)
                         )
                       append
                       (gethash mode librarian-regular--cache)
                       )
              )
  )

(defun librarian-regular--load-file (file)
  "read a list of (name . url) from the given file"
  (let (targets)
    (with-temp-buffer
      (insert-file-contents file)
      (mapc #'(lambda (x)
                (-when-let (vals (split-string x librarian-regular--splitter t " +"))
                  (push (cons (car vals) (cadr vals)) targets)
                  ))
            (s-lines (buffer-substring-no-properties (point-min) (point-max)))
            )
      )
    targets
    )
  )

(defun librarian-regular-minor-mode/turn-on ()
  (unless (minibufferp)
    (librarian-regular-minor-mode 1))
  )

(define-globalized-minor-mode global-librarian-regular-minor-mode librarian-regular-minor-mode librarian-regular-minor-mode/turn-on)

(defun librarian-regular-go ()
  (interactive)
  (ivy-read "Lookup: "
            (buffer-local-value 'librarian-regular--targets (current-buffer))
            :require-match t :sort t
            :action #'(lambda (x) (browse-url (cdr x)))
            )
)

(provide 'librarian-regular)
;;; librarian-regular.el ends here
