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

(eval-when-compile
  (require 'f)
  (require 'cl-lib)
  (require 's)
  (require 'dash)

  (require 'evil-core)
  (require 'evil-common)
  (require 'thingatpt)
  (require 'eldoc)
  (require 'ivy)
  (require 'counsel)
  (require 'eww)
  (require 'bibtex)
  (require 'bibtex-completion)
  (require 'org-ref-bibtex)

  (require 'synosaurus)
  (require 'powerthesaurus nil t)
  (require 'wordnut)
  (when (eq system-type 'darwin) (require 'osx-dictionary))
  (require 'define-word nil t)
  (require 'helm-wordnet)

  (require 'better-jumper)
  (require 'browse-url)
  (require 'counsel-dash)

  (require 'free-keys)
  (require 'helpful)
  (require 'xref)
  (require 'browse-url)
  (require 'dash-docs)
  (require 'counsel-dash)
  )

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
(require 'lib--words)
(require 'lib--insert)

(librarian--doc-init-defaults)
(defconst lib-active-on-modes (list 'text-mode 'prog-mode 'conf-mode))

(defconst lib-forbid-modes (list 'magit-mode))

(defvar lib-mode-map (make-sparse-keymap))

(evil-make-intercept-map lib-mode-map 'normal)

(define-minor-mode lib-mode
  "An interface for controlling lookups, spelling, documentation, online search"
  :lighter (:eval (format "Browser: %s" librarian-default-browser))
  :keymap librarian-mode-map
  )

(defun librarian-mode/turn-on ()
  (when (and (not (or (minibufferp)
                      (apply #'derived-mode-p librarian-forbid-modes)))
             (apply #'derived-mode-p librarian-active-on-modes))
    (librarian-mode 1)
    )
  )

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
  (let ((handlers (list
                   (cons :assignments     librarian--doc-assignments-functions)
                   (cons :definition      librarian--doc-definition-functions)
                   (cons :declaration     librarian--doc-declaration-functions)
                   (cons :documentation   librarian--doc-documentation-functions)
                   (cons :file            librarian--doc-file-functions)
                   (cons :implementations librarian--doc-implementations-functions)
                   (cons :references      librarian--doc-references-functions)
                   (cons :type-definition librarian--doc-type-definition-functions)
                   ))
        )
    (message (format "Lookup Handlers Are:\n%s"
                     (string-join (mapcar #'(lambda (x)
                                              (format "%-25s : %s" (car x) (cdr x)))
                                          handlers) "\n")
                     )
             )
    )
  )

(provide 'librarian)
;;; librarian.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("lib-" . "librarian-")
;; )
;; End:
