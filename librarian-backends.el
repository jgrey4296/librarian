;;; librarian-handlers.el -*- lexical-binding: t; no-byte-compile: t; -*-
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

(defun librarian--browser-backend-default (url &rest args)
  " Find and call the appropriate browser program,
after `browse-url-handlers` have processed the url
"
  (cond ((-contains? args 'quicklook)
         (start-process "open-ql" "*browse-select*" "qlmanage" "-p" (shell-quote-argument url)))
        ((and (-contains? args 'local) (f-ext? url "epub"))
         (apply 'start-process "open-epub" "*browse-select*" "open" url librarian-epub-args)
         )
        ((and (-contains? args 'local) (f-ext? url "pdf") librarian-browser-use-preview)
         (apply 'start-process "open-pdf" "*browse-select*" "open" url librarian-pdf-args)
         )
        ((not (s-equals? librarian-default-browser "eww"))
         (message "Using %s" librarian-default-browser)
         (start-process "open-url" "*browse-select*" librarian-default-browser url)
         )
        (t
         (eww-browse-url url args))
        )

  (sleep-for 2)
  ;; (librarian-browser-regain-focus)
  )

(defun librarian--browser-backend-amazon (url &rest args)
  ;; TODO Handle US and UK
  (signal 'browse-todo url)
  )

(defun librarian--online-backend-google (query)
  "Search Google, starting with QUERY, with live autocompletion."
  (cond ((and (bound-and-true-p ivy-mode) (fboundp 'counsel-search))
         (let ((ivy-initial-inputs-alist `((t . ,query)))
               (counsel-search-engine 'google))
           (call-interactively #'counsel-search)
           t))
        ((and (bound-and-true-p helm-mode) (boundp 'helm-source-google-suggest))
         (helm :sources 'helm-source-google-suggest
               :buffer "*helm google*"
               :input query)
         t)))

(defun librarian--online-backend-duckduckgo (query)
  "Search DuckDuckGo, starting with QUERY, with live autocompletion."
  (cond ((and (bound-and-true-p ivy-mode) (fboundp 'counsel-search))
         (let ((ivy-initial-inputs-alist `((t . ,query)))
               (counsel-search-engine 'ddg))
           (call-interactively #'counsel-search)
           t))))

(provide 'librarian-backends)
;;; librarian-handlers.el ends here
