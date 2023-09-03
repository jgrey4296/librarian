;;; librarian-defs.el -*- lexical-binding: t; no-byte-compile: t; -*-
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
;;
;;
;;; Code:
;;-- end header

(defvar counsel-search-engine)

(defvar dash-docs-docsets nil)

(defvar ivy-initial-inputs-alist)

(defvar librarian-valid-keywords '(
                                    :definition
                                    :declaration
                                    :implementations
                                    :type-definition
                                    :references
                                    :documentation
                                    :assignments
                                    )
  "Valid Types of Lookup commands that can be registered")

(setq-default librarian-definition-functions      nil
              librarian-declaration-functions     nil
              librarian-implementations-functions nil
              librarian-type-definition-functions nil
              librarian-references-functions      nil
              librarian-documentation-functions   nil
              librarian-file-functions            nil
              librarian-assignments-functions     nil
              )

(defvar librarian--provider-url-alist nil
  "An alist that maps online resources to either:

  1. A search url (needs on '%s' to substitute with an url encoded query),
  2. A non-interactive function that returns the search url in #1,
  3. An interactive command that does its own search for that provider.

Used by `+lookup/online'.")

(defvar librarian-definition-defaults
  '(librarian-dictionary-definition-backend-fn
    librarian-xref-definitions-backend-fn
    librarian-dumb-jump-backend-fn
    librarian-project-search-backend-fn
    librarian-evil-goto-definition-backend-fn)
  )

(defvar librarian-references-defaults '(librarian-thesaurus-definition-backend-fn
                                        librarian-xref-references-backend-fn
                                        librarian-project-search-backend-fn
                                        )
  )

(defconst librarian--regular-splitter "#")

(defvar librarian--amazon-url "")

(defvar librarian--last-provider nil)

(defvar librarian--xwidget-webkit-last-session-buffer nil)

(defvar librarian-args  '("-sLI"))

(defvar librarian-browser-use-preview t)

(defvar librarian-browser-variants ())

(defvar librarian-browser-variants-file "~/.browsers")

(defvar librarian-curl-cmd  "curl")

(defvar librarian-declaration-defaults nil)

(defvar librarian-default-browser "firefox")

(defvar librarian-dictionary-prefer-offline nil)

(defvar librarian-documentation-defaults '(librarian-dash-docsets-backend-fn librarian-online-backend-fn))

(defvar librarian-epub-args '("-a" "ebook-viewer"))

(defvar librarian-file-defaults '(librarian-bug-reference-backend-fn librarian-ffap-backend-fn))

(defvar librarian-implementations-defaults ())

(defvar librarian-open-url-fn #'browse-url "Function to use to open search urls.")

(defvar librarian-pdf-args  '("-a" "Preview" "-nF"))

(defvar librarian-refocus-target  "iTerm")

(defvar librarian-regular--cache (make-hash-table))

(defvar librarian-regular-location nil)

(defvar librarian-regular-minor-mode-map (make-sparse-keymap))

(defvar librarian-type-definition-defaults ())

(defvar-local librarian-regular-targets nil)
(provide 'librarian-defs)
;;; librarian-defs.el ends here
