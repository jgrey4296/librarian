;;; lookup-search.el -*- lexical-binding: t; no-byte-compile: t; -*-
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

(require 'evil-common)

(defvar librarian-online--provider-url-alist nil
  "An alist that maps online resources to either:

  1. A search url (needs on '%s' to substitute with an url encoded query),
  2. A non-interactive function that returns the search url in #1,
  3. An interactive command that does its own search for that provider.

Used by `librarian-online'.")

(defvar librarian-online--open-url #'browse-url "Function to use to open search urls.")

(defvar librarian--last-provider nil)

(defun librarian--online-provider (&optional force-p namespace)
  (let ((key (or namespace major-mode)))
    (or (and (not force-p)
             (cdr (assq key librarian--last-provider)))
        (when-let (provider
                   (completing-read
                    "Search on: "
                    (mapcar #'car librarian-online--provider-url-alist)
                    nil t))
          (setf (alist-get key librarian--last-provider) provider)
          provider))))

;;;###autoload
(defun librarian-online (query provider)
  "Look up QUERY in the browser using PROVIDER.
When called interactively, prompt for a query and, when called for the first
time, the provider from `librarian-online--provider-url-alist'. In subsequent calls, reuse
the previous provider. With a non-nil prefix argument, always prompt for the
provider.

QUERY must be a string, and PROVIDER must be a key of
`librarian-online--provider-url-alist'."
  (interactive
   (list (if (use-region-p) (librarian-get))
         (librarian--online-provider current-prefix-arg)))

  (let ((backends (cdr (assoc provider librarian-online--provider-url-alist))))
    (unless backends
      (user-error "No available online lookup backend for %S provider"
                  provider))
    (catch 'done
      (dolist (backend backends)
        (cl-check-type backend (or string function))
        (cond ((stringp backend)
               (funcall librarian-online--open-url
                        (format backend
                                (url-encode-url
                                 (or query
                                     (read-string (format "Search for (on %s): " provider)
                                                  (thing-at-point 'symbol t)))))))
              ((condition-case-unless-debug e
                   (and (fboundp backend)
                        (funcall backend query))
                 (error
                  (delq! major-mode librarian--last-provider 'assq)
                  (signal (car e) (cdr e))))
               (throw 'done t)))))))

(defun librarian-online-select ()
  "Run `librarian-online', but always prompt for the provider to use."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'librarian-online)))

(evil-define-command evil-librarian-online (query &optional bang)
  "Look up QUERY online. Will prompt for search engine the first time, then
reuse it on consecutive uses of this command. If BANG, always prompt for search
engine."
  (interactive "<a><!>")
  (librarian-online query (librarian--online-provider bang 'evil-ex)))

(evil-define-command evil-librarian-dash (query &optional bang)
  "Look up QUERY in your dash docsets. If BANG, prompt to select a docset (and
install it if necessary)."
  (interactive "<a><!>")
  (let (selected)
    (when bang
      (setq selected (helm-dash-read-docset "Select docset" (helm-dash-official-docsets)))
      (unless (dash-docs-docset-path selected)
        (librarian-install-docset selected)))
    (librarian-in-docsets query selected)))

(provide 'librarian-online)
;;; lookup-search.el ends here
