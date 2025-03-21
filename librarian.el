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
;; Package-Requires: ((emacs "24.3") (dash) (s) (cl-lib) (f) (evil) (better-jumper) (browse-url) (counsel) (counsel-dash) (free-keys) (xref) (browse-url) (helpful) (thingatpt) (eldoc) (ivy) (counsel) (synosaurus) (wordnut) (helm-wordnet))
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

(require 'lib--util)
(require 'lib--backend)
(require 'lib--structs)

(require 'lib--biblio)
(require 'lib--browse)
(require 'lib--config)
(require 'lib--docsets)
(require 'lib--doc)
(require 'lib--envs)
(require 'lib--file)
(require 'lib--man)
(require 'lib--online)
(require 'lib--regular)
(require 'lib--tag)
(require 'lib--tag-chart)
(require 'lib--tag-mode)
(require 'lib--words)
(require 'lib--insert)

(librarian--doc-init-defaults)

(defconst lib-active-on-modes (list 'text-mode 'prog-mode 'conf-mode))

(defconst lib-forbid-modes (list 'magit-mode))

(defvar lib-mode-map (make-sparse-keymap))

(evil-make-intercept-map lib-mode-map 'normal)

;;;###autoload (autoload 'librarian-minor-mode "librarian")
(define-minor-mode lib-mode
  "An interface for controlling lookups, spelling, documentation, online search"
  :lighter "Librarian"
  :keymap librarian-mode-map
  )

(defun librarian-mode/turn-on ()
  (when (and (not (or (minibufferp)
                      (apply #'derived-mode-p librarian-forbid-modes)))
             (apply #'derived-mode-p librarian-active-on-modes))
    (librarian-mode 1)
    )
  )

;;;###autoload
(define-globalized-minor-mode global-librarian-mode librarian-mode librarian-mode/turn-on
  (message "Librarian: %s" global-librarian-mode)
  (if global-librarian-mode
      (progn ;; activating
        (global-librarian-regular-minor-mode 1)
        (global-librarian-tagging-mode 1)
        (librarian--browse-load-variants)
        (unless (not (and (boundp 'librarian-configs--modules-cache) librarian--config-modules-cache))
          (librarian--config--build-modules-cache))

        (setq xref-show-definitions-function #'ivy-xref-show-defs
              xref-show-xrefs-function       #'ivy-xref-show-xrefs
              browse-url-browser-function    #'librarian--browse-open-url
              browse-url-handlers nil
              browse-url-default-handlers '(
                                            ("." . librarian--browse-open-url)
                                            )
              )
        )
    (progn ;; deactivating
      (global-librarian-regular-minor-mode -1)
      (global-librarian-tagging-mode -1)
      (setq browse-url-browser-function #'browse-url-default-browser)
      )
    )
  )

;;;###autoload
(defun librarian-debug ()
  " Check librarian settings:
   documentation function assignments,
   assigned browser,
   installed docsets
   registered url handlers
   "
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((handlers (cl-loop for prop in librarian--doc-valid-keywords
                             for fns = (plist-get
                                        librarian--doc-handlers-plist
                                        prop)
                             collect
                             (format "%-25s: %s" prop fns)
                             ))
          )
      (message (format "Lookup Handlers for \"%s\" are: \n%s"
                       (buffer-name)
                       (string-join handlers "\n")))
      )
    )
)
(advice-add #'ivy-xref-show-xrefs :around #'librarian--util-fix-ivy-xrefs)


(provide 'librarian)
;;; librarian.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("lib-" . "librarian-")
;; )
;; End:
