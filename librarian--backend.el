;;; librarian-handlers.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;;
;;
;;
;;; Code:
;;-- end header

(eval-when-compile
  (require 'thingatpt)
  (require 'eldoc)
  (require 'ivy)
  (require 'xref)
  (require 'evil)
  (require 'counsel)
  )

(unless (boundp 'counsel-search-engine)    (defvar counsel-search-engine nil))
(unless (boundp 'ivy-initial-inputs-alist) (defvar ivy-initial-inputs-alist nil))

(defvar lib--bibtex-scholar-search-fields       '("author" "editor" "ALTauthor" "Alteditor" "year" "doi" "isbn"))

(defvar lib--bibtex-scholar-search-fields-exact '("title"))

(defun lib---browser-amazon (url &rest args)
  ;; TODO Handle US and UK
  (signal 'browse-todo url)
  )

(defun lib---online-google (query)
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

(defun lib---online-duckduckgo (query)
  "Search DuckDuckGo, starting with QUERY, with live autocompletion."
  (cond ((and (bound-and-true-p ivy-mode) (fboundp 'counsel-search))
         (let ((ivy-initial-inputs-alist `((t . ,query)))
               (counsel-search-engine 'ddg))
           (call-interactively #'counsel-search)
           t))))

(defun lib---words-dictionary (identifier)
  "Look up dictionary definition for IDENTIFIER."
  (when (derived-mode-p 'text-mode)
    (librarian-words-definition identifier)
    'deferred))

(defun lib---words-thesaurus (identifier)
  "Look up synonyms for IDENTIFIER."
  (when (derived-mode-p 'text-mode)
    (librarian-synonyms identifier)
    'deferred))

(defun lib---dumb-jump (_identifier)
  "Look up the symbol at point (or selection) with `dumb-jump', which conducts a
project search with ag, rg, pt, or git-grep, combined with extra heuristics to
reduce false positives.

This backend prefers \"just working\" over accuracy."
  (and (require 'dumb-jump nil t)
       (dumb-jump-go)))

(defun lib---project-search (identifier)
  "Conducts a simple project text search for IDENTIFIER.

Uses and requires `+ivy-file-search', `+helm-file-search', or `+vertico-file-search'.
Will return nil if neither is available. These require ripgrep to be installed."
  (unless identifier
    (let ((query (rxt-quote-pcre identifier)))
      (ignore-errors
        (+ivy-file-search :query query)
        t)
      )
    )
  )

(defun lib---evil-goto-def (_identifier)
  "Uses `evil-goto-definition' to conduct a text search for IDENTIFIER in the
current buffer."
  (when (fboundp 'evil-goto-definition)
    (ignore-errors
      (cl-destructuring-bind (beg . end)
          (bounds-of-thing-at-point 'symbol)
        (evil-goto-definition)
        (let ((pt (point)))
          (not (and (>= pt beg)
                    (<  pt end))))))))

(defun lib---ffap (identifier)
  "Tries to locate the file at point (or in active selection).
Uses find-in-project functionality (provided by ivy, helm, or project),
otherwise falling back to ffap.el (find-file-at-point)."
  (let ((guess
         (cond (identifier)
               ((region-active-p)
                (buffer-substring-no-properties
                 (region-beginning)
                 (region-end)))
               ((if (require 'ffap) (ffap-guesser)))
               ((thing-at-point 'filename t)))))
    (cond ((and (stringp guess)
                (or (file-exists-p guess)
                    (ffap-url-p guess)))
           (find-file-at-point guess))
          ((projectile-project-p)
           (counsel-file-jump guess (projectile-project-root)))
          ((find-file-at-point (ffap-prompter guess))))
    t))

(defun lib---bug-reference (_identifier)
  "Searches for a bug reference in user/repo#123 or #123 format and opens it in
the browser."
  (require 'bug-reference)
  (when (fboundp 'bug-reference-try-setup-from-vc)
    (let ((old-bug-reference-mode bug-reference-mode)
          (old-bug-reference-prog-mode bug-reference-prog-mode)
          (bug-reference-url-format bug-reference-url-format)
          (bug-reference-bug-regexp bug-reference-bug-regexp))
      (bug-reference-try-setup-from-vc)
      (unwind-protect
          (let ((bug-reference-mode t)
                (bug-reference-prog-mode nil))
            (catch 'found
              (bug-reference-fontify (line-beginning-position) (line-end-position))
              (dolist (o (overlays-at (point)))
                ;; It should only be possible to have one URL overlay.
                (when-let (url (overlay-get o 'bug-reference-url))
                  (browse-url url)

                  (throw 'found t)))))
        ;; Restore any messed up fontification as a result of this.
        (bug-reference-unfontify (line-beginning-position) (line-end-position))
        (if (or old-bug-reference-mode
                old-bug-reference-prog-mode)
            (bug-reference-fontify (line-beginning-position) (line-end-position)))))))

(defun lib---docsets-dash (identifier)
  " This backend is meant for `librarian-documentation-functions'.
Docsets can be searched directly via `librarian-docset-consult'."
  (when (fboundp 'dash-docs-docset-path)
    (when-let (docsets (cl-remove-if-not #'dash-docs-docset-path (dash-docs-buffer-local-docsets)))
      (librarian-docset-consult nil identifier docsets)
      'deferred))
  )

(defun lib---docsets-online (identifier)
  "Open the browser and search for IDENTIFIER online.
When called for the first time, or with a non-nil prefix argument, prompt for
the search engine to use."
  (librarian-online identifier
   (librarian--online-provider (not current-prefix-arg))))

(defun lib---xref-show (fn identifier &optional show-fn)
  (let ((xrefs (funcall fn
                        (xref-find-backend)
                        identifier)))
    (when xrefs
      (let* ((jumped nil)
             (xref-after-jump-hook
              (cons (lambda () (setq jumped t))
                    xref-after-jump-hook)))
        (funcall (or show-fn #'xref--show-defs)
                 (lambda () xrefs)
                 nil)
        (if (cdr xrefs)
            'deferred
          jumped)))))

(defun lib---xref-definitions (identifier)
  "Non-interactive wrapper for `xref-find-definitions'"
  (condition-case _
      (lib---xref-show 'xref-backend-definitions identifier #'xref--show-defs)
    (cl-no-applicable-method nil)))

(defun lib---xref-references (identifier)
  "Non-interactive wrapper for `xref-find-references'"
  (condition-case _
      (lib---xref-show 'xref-backend-references identifier #'xref--show-xrefs)
    (cl-no-applicable-method nil)))

(defun lib---bibliography-scholar (arg)
  "Open the bibtex entry at point in google-scholar by its doi.
With arg, searchs the dplp instead.
"
  (let* ((search-texts (mapcar #'bibtex-autokey-get-field librarian--bibtex-scholar-search-fields))
         (exact-texts  (mapcar #'bibtex-autokey-get-field librarian--bibtex-scholar-search-fields-exact))
         (exact-string (s-join " " (mapcar #'(lambda (x) (format "\"%s\"" x))
                                           (-filter #'(lambda (x) (not (string-empty-p x))) exact-texts))))
         (all-terms (s-concat exact-string " " (s-join " " search-texts)))
         (cleaned (s-replace-regexp "{.+?\\(\\w\\)}" "\\1" all-terms))
         )
    (+lookup/online cleaned "Scholar")
    )
  )

(provide 'librarian--backend)
;;; librarian-handlers.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("lib-" . "librarian--backend-")
;; )
;; End:
