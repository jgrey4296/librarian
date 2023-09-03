;;; librarian.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey@Johns-Mac-mini.local>
;; Created: September 03, 2023
;; Modified: September 03, 2023
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
;;  A General controller for lookup, spelling, definition and online search
;;
;;
;;; Code:
;;-- end header
(require 'cl-lib)
(require 'ivy)
(require 'counsel)
(require 'browse-url)
(require 'dash)
(require 'dash-docs)
(require 'counsel-dash)
(require 'xref)
(require 'better-jumper)
(require 'thingatpt)
(require 'evil)
(require 'eldoc)
(require 'f)
(require 's)

(define-minor-mode librarian-mode
  "An interface for controlling lookups, spelling, documentation, online search"
  :global t
  :lighter (:eval (format "Browser: %s" librarian-default-browser))
)

(defun librarian-debug ()
  "
Check librarian settings:
documentation function assignments,
assigned browser,
installed docsets
registered url handlers

"
  (interactive)
  (let ((handlers (list
                   (cons :assignments     librarian-assignments-functions)
                   (cons :definition      librarian-definition-functions)
                   (cons :declaration     librarian-declaration-functions)
                   (cons :documentation   librarian-documentation-functions)
                   (cons :file            librarian-file-functions)
                   (cons :implementations librarian-implementations-functions)
                   (cons :references      librarian-references-functions)
                   (cons :type-definition librarian-type-definition-functions)
                   ))
        )
    (message "Lookup Handlers Are:\n%s"
             (string-join (mapcar #'(lambda (x)
                                      (format "%-25s : %s" (car x) (cdr x)))
                                  handlers) "\n")
             )
    )
  )

(defun librarian-url (&optional url)
  " use librarian to open a url "
  (interactive)
  (let ((url (cond (url url)
                   ((and (boundp 'evil-state) (eq evil-state 'visual))
                    (buffer-substring-no-properties evil-visual-beginning evil-visual-end))
                   (t nil)))
        )
    (cond ((not url)
           (librarian-online-select))
          ((f-exists? url)
           (shell-command (format "open %s" url)))
          (t
           (call-interactively #'librarian-online url) ;;TODO
           )
          )
    )
  )


(defalias 'librarian-docset-install #'counsel-dash-install-docset)

(provide 'librarian)
;;; librarian.el ends here
