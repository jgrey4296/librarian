;;; librarian--biblio-edit.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(eval-when-compile
  (require 'bookmark)
  (require 'bibtex)
  )

;;-- vars

(defvar lib-fill-column 50000)

(defvar lib-helm-candidates nil)

(defvar lib-candidates-names '())

(defvar lib-rand-log ".emacs_rand_bib_log")

(defvar lib-open-doi-with-pdf        nil)

(defvar lib-open-url-with-pdf        nil)

(defvar lib-todo-loc                 nil)

(defvar lib-todo-files-loc           nil)

(defvar lib-completions-loc          nil)

(defvar lib-export-bib-loc           nil)

(defvar lib-temp-tex-loc             nil)

(defvar lib-search-fields               '("tags" "year" "publisher"))

(defvar lib-scholar-search-fields       '("author" "editor" "ALTauthor" "Alteditor" "year" "doi" "isbn"))

(defvar lib-scholar-search-fields-exact '("title"))

(defvar lib-doi-url               "https://doi.org/%s")

(defvar lib-completion-display-formats
      '(
        (judicial . "${=has-pdf=:1} ${=type=:10} || ${year:4} || ${author:20} || ${short_parties:80} || ${tags:*}")
        (review   . "${=has-pdf=:1} REVIEW     || ${year:4} || ${author:20} || REVIEW ${title:73} || ${tags:*}")
        (online   . "${=has-pdf=:1} ${=type=:10} || ${year:4} || ${author:20} || ${title:80} || ${tags:*}")
        (t        . "${=has-pdf=:1} ${=type=:10} || ${year:4} || ${author:20} || ${title:80} || ${tags:*}")
        ;; (t     . ("${author:20} || ${title:*} || ${year:4}" 40))
        ;; (t     . "${author:35} ${title:*} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:7}")
        )
      "Display formats for the ivy bibtex. see bibtex-completion-display-formats"
      )

(setq bibtex-completion-display-formats lib-completion-display-formats)

(defvar lib-pdf-loc-regexp               "file[[:digit:]]*\s*=\s*{\\(.+\\)/\\(.+pdfs\\)?")

(defvar lib-pdf-replace-match-string     "~/")

(defvar lib-pdf-replace-library-string   "pdfs")

(defvar bibtex-completion-pdf-open-function 'browse-url)

;;-- end vars

;;-- commands

(defun lib-entry-type ()
  " Edit the @type of a bibtex entry, using
bibtex-BibTeX-entry-alist for completion options "
  (interactive)
  (let* ((type-options (mapcar 'car bibtex-entry-alist))
         (selection (completing-read "New Bibtex Type: " type-options))
         )
    (save-excursion
      (bibtex-beginning-of-entry)
      (when (search-forward-regexp "@\\(.+?\\){" (line-end-position))
        (replace-match (string-trim selection) t nil nil 1)
        )
      )
    )
  )

(defun lib-copy-entry ()
  " Copy the entire entry under point "
  (interactive)
  (save-excursion
    (let ((start (bibtex-beginning-of-entry))
          (end (bibtex-end-of-entry)))
      (copy-region-as-kill start end)
      )
    )
  )

(defun lib-copy-key ()
  " Copy the cite key of the entry under point "
  (interactive)
  (kill-new (bibtex-completion-get-key-bibtex))
  (message "Copied Key: %s" (current-kill 0 t))
  )

(defun lib-copy-title ()
  (interactive)
  (kill-new (bibtex-autokey-get-field "title"))
  (message "Copied Title: %s" (current-kill 0 t))
  )

(defun lib-copy-field ()
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((entry (bibtex-parse-entry))
           (fields (-reject #'(lambda (x) (s-contains? "=" x)) (mapcar 'car entry)))
           (selected (ivy-read "Field to Copy: " fields)))
      (kill-new (bibtex-autokey-get-field selected)))
    )
  )

(defun lib-quickswap ()
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((journal (bibtex-autokey-get-field "journal"))
           (booktitle (bibtex-autokey-get-field "booktitle")))
      (bibtex-set-field "booktitle" journal)
      (bibtex-set-field "journal" booktitle)
      )
    )
  )

(defun lib-rename-file ()
  " Rename the file associated with the record "
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((fields      (bibtex-parse-entry))
         (file-fields (-filter (lambda (x) (string-match "file" (car x))) fields))
         (field-selection (if (eq 1 (length file-fields))
                              (caar file-fields)
                            (read-string "File Select: " "file")))
         (filename (bibtex-autokey-get-field field-selection))
         (ext (f-ext filename))
         (base (f-parent filename))
         (new-name (string-trim (read-string "New Filename: " (f-base filename))))
         (new-path (f-join base (format "%s.%s" new-name ext))))
    (message "Moving: %s\nto: %s" filename new-path)
    (f-move filename new-path)
    (bibtex-set-field field-selection new-path)
    )
  )

(defun lib-dired-unify-pdf-locations ()
  "Unify bibtex pdf paths of marked files"
  (interactive)
  (seq-each 'lib-unify-pdf-locations-in-file (dired-get-marked-files))
  )

(defun lib-unify-pdf-locations-in-file (name)
  "Change all pdf locations in bibtex file to relative,
ensuring they work across machines "
  (message "Unifying Locations in %s" name)
  (with-temp-buffer
    (insert-file-contents name t)
    (goto-char (point-min))
    (while (re-search-forward lib-pdf-loc-regexp nil t)
      (replace-match lib-pdf-replace-match-string nil nil nil 1)
      (when (eq 6 (length (match-data)))
        (replace-match lib-pdf-replace-library-string t nil nil 2))
      )
    (write-file name)
    )
  )

(defun lib-swap-editor-author ()
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((entry (save-excursion (bibtex-parse-entry)))
           (editor (alist-get "editor" entry nil nil 'equal))
           (author (alist-get "author" entry nil nil 'equal))
           )
      (cond ((and editor author) nil)
            (editor
             (bibtex-set-field "author" (substring editor 1 -1))
             (bibtex-beginning-of-entry)
             (if (re-search-forward "editor" (save-excursion (bibtex-end-of-entry) (point)) t)
                 (delete-line))
             )
            (author
             (bibtex-set-field "editor" (substring author 1 -1))
             (bibtex-beginning-of-entry)
             (when (re-search-forward "author" (save-excursion (bibtex-end-of-entry) (point)) t)
                 (delete-line))
             )
            )
      )
    )
  )

(defun lib-swap-booktitle-journal ()
  (interactive)
    (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((entry (save-excursion (bibtex-parse-entry)))
           (book (alist-get "booktitle" entry nil nil 'equal))
           (journal (alist-get "journal" entry nil nil 'equal))
           )
      (cond ((and book journal) nil)
            (book
             (bibtex-set-field "journal" (substring book 1 -1))
             (bibtex-beginning-of-entry)
             (if (re-search-forward "booktitle" (save-excursion (bibtex-end-of-entry) (point)) t)
                 (delete-line))
             )
            (journal
             (bibtex-set-field "booktitle" (substring journal 1 -1))
             (bibtex-beginning-of-entry)
             (when (re-search-forward "journal" (save-excursion (bibtex-end-of-entry) (point)) t)
                 (delete-line))
             )
            )
      )
    )
  )

(defun lib-open-pdf (&optional path)
  "Open pdf for a bibtex entry, if it exists.
assumes point is in
the entry of interest in the bibfile.  but does not check that."
  (interactive)
  (basic-save-buffer)
  (save-excursion
    (let* ((target (if path path (bibtex-autokey-get-field '("file" "OPTfile"))))
           )
      (message "Bibtex Open Target: %s" target)
      (cond ((string-empty-p target)
             (message "No File to open"))
            ((not (file-exists-p target))
             (message "File does not exist"))
            (t (call-process "open" nil nil nil target))
            )

      (when lib-open-doi-with-pdf
          (lib-open-doi))
      (when lib-open-url-with-pdf
          (lib-open-url))
      )))

(defun lib-open-url ()
  " Open the current entry's url in browser "
  (interactive)
  (when (bibtex-text-in-field "url")
    (browse-url (bibtex-text-in-field "url")))
  )

(defun lib-open-doi ()
  " Follow the doi link of the current entry in a browser "
  (interactive)
  (when (bibtex-text-in-field "doi")
    (browse-url (format lib-doi-url (bibtex-text-in-field "doi")))
    )
  )

(defun lib-open-folder ()
  " Open the associated file's folder in finder "
  (interactive)
  (let* ((target (bibtex-autokey-get-field '("file" "OPTfile"))))
    (when (and (not (string-empty-p target)) (f-exists? target))
      (message "Opening %s" target)
      (shell-command (concat "open " (shell-quote-argument (f-parent target))))
      )
    )
  )

(defun lib-load-random ()
  " Run in a bibtex file, opens a random entry externally,
      and logs it has been opened in a separate file.

Log into lib-rand-log.
 "
  (interactive)
  (widen)
  (let* ((location (f-dirname (buffer-file-name)))
         (log_file (f-join location lib-rand-log))
         (log_hash (if (f-exists? log_file)
                       (with-temp-buffer
                         (insert-file-contents log_file)
                         (let ((uf (make-hash-table :test 'equal)))
                           (seq-each (lambda (x) (puthash x 't uf)) (split-string (buffer-string) "\n"))
                           uf))
                     (make-hash-table :test 'equal)))
         )
    ;; go to random line
    (goto-char (random (point-max)))
    (org-ref-bibtex-next-entry)
    (let ((entry (bibtex-parse-entry)))
      (while entry
        (if (gethash (alist-get "=key=" entry nil nil 'equal) log_hash)
            (progn (goto-char (random (point-max)))
                   (org-reg-bibtex-next-entry)
                   (setq entry (bibtex-parse-entry)))
          (progn
            (write-region (alist-get "=key=" entry nil nil 'equal)
                          nil log_file 'append)
            (write-region "\n" nil log_file 'append)
            (bibtex-narrow-to-entry)
            (goto-char (point-min))
            (lib-open-pdf)
            (setq entry nil)
            )
          )
        )
      )
    )
  )

(defun lib-quicklook-pdf ()
  "Open pdf for a bibtex entry, if it exists.
assumes point is in
the entry of interest in the bibfile.  but does not check that."
  (interactive)
  (save-excursion
    (let* ((target (bibtex-autokey-get-field '("file" "OPTfile"))))
      (message "%s : %s" target (file-exists-p target))
      (browse-url target 'quicklook)
      )))

(defun lib-use-newest-file ()
  "Get the newest pdf or epub from the downloads dir,
copy it to the bib todo directory,
and insert it into the current entry "
  (interactive)
  (unless (string-empty-p (bibtex-autokey-get-field "file"))
    (user-error "Entry already has a file"))
  (let* ((files (f-files (expand-file-name "~/Downloads")
                                          #'(lambda (x) (or (f-ext? x "pdf")
                                                            (f-ext? x "epub")
                                                            ))))
         (newest (car-safe (sort files #'(lambda (x y)
                                 (not (time-less-p (f-modification-time x)
                                                   (f-modification-time y)))))))
         )
    (unless newest
      (user-error "No Applicable File Found")
      )
    (when (f-exists? (f-join lib-todo-files-loc (f-filename newest)))
      (use-error "File already exists in todo directory: %s" newest)
      )
    (message "Newest: %s" newest)
    (f-move newest (f-join lib-todo-files-loc (f-filename newest)))
    (bibtex-beginning-of-entry)
    (save-excursion
      (bibtex-set-field "file" (f-join lib-todo-files-loc (f-filename newest)))
      )
    )
)

(defun lib-kill-entry-key ()
  (interactive)
  (bibtex-beginning-of-entry)
  (let ((key (bibtex-completion-get-key-bibtex)))
    (unless (s-matches? "_$" key)
      (re-search-forward "{" (line-end-position))
      (kill-line)
      (insert ",")
      )
    )
  )

(defun lib-lock-key ()
  (interactive)
  (bibtex-beginning-of-entry)
  (let ((key (bibtex-completion-get-key-bibtex)))
    (unless (s-matches? "_$" key)
      (re-search-forward "{" (line-end-position))
      (kill-line)
      (insert (concat key "_,"))
      )
    )
  )

(defun lib-subcite ()
  (interactive)
  (lib-lock-key)
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
         (key (alist-get "=key=" entry nil nil #'s-equals?))
         (type (alist-get "=type=" entry nil nil #'s-equals?))
         (year (alist-get "year" entry nil nil #'s-equals?))
         (cite-type (concat "@In" type))
         )
    ;; make key permanent if necessary

    ;; go to end of entry
    ;; insert stub
    (bibtex-end-of-entry)
    (insert "\n\n")
    (insert cite-type)
    (insert (format "{%s%s,\n" lib-default-stubkey-base (random 5000)))
    (insert "year = " year ",\n")
    (insert "crossref = {" key "},\n")
    (insert "title = {},\n")
    (insert "author = {},\n")
    (insert "}")
    (bibtex-beginning-of-entry)
    (lib-edit-field "title")
    (lib-edit-field "author")
    (lib-edit-field "pages")
    )
  )

(defun lib-title-case (x)
  "Split a string into separated words, and capitalise the first letter of each
before rejoining "
  (let* ((case-fold-search nil)
         (orig-syntax-table (syntax-table))
         (mod-syn (copy-syntax-table orig-syntax-table))
        )

    (condition-case err
        (progn
          ;; https://stackoverflow.com/questions/1314670
          (modify-syntax-entry ?’ "w" mod-syn)
          (modify-syntax-entry ?' "w" mod-syn)
          (set-syntax-table mod-syn)
          (string-join (cl-loop for x in (split-string x " +" t " +")
                                collect
                                (cond (t (capitalize x))
                                      ((s-matches-p (rx word-start (or "and" (+ upper-case)) word-end ) x)
                                       x)
                                      (t (capitalize x))
                                      )
                                )
                       " ")
          )
      (t (set-syntax-table orig-syntax-table)
         (signal (car err) (cdr err)))
      (t (set-syntax-table orig-syntax-table)
         err
         )
      )
    )
  )

(defun lib-toggle-doi-load ()
  (interactive)
  (setq lib-open-doi-with-pdf (not lib-open-doi-with-pdf))
  (message "Open DOI on pdf? %s" lib-open-doi-with-pdf)
  )

(defun lib-toggle-url-load ()
  (interactive)
  (setq lib-open-url-with-pdf (not lib-open-url-with-pdf))
  (message "Open URL on pdf? %s" lib-open-url-with-pdf)
  )

(defun lib-visual-select-entry ()
  " Evil visual select the current entry "
  (interactive)
  (evil-visual-make-region (bibtex-beginning-of-entry)
                           (bibtex-end-of-entry))
)

(defun lib-goto-crossref-entry ()
  " Follow the crossref field in the entry "
  (interactive)
  (when (bibtex-text-in-field "crossref")
    (bibtex-find-crossref (bibtex-text-in-field "crossref"))
    )
  )

(defun lib-google-scholar (arg)
  "Open the bibtex entry at point in google-scholar by its doi.
With arg, searchs the dplp instead.
"
  (interactive "P")
  (let* ((search-texts (mapcar #'bibtex-autokey-get-field lib-scholar-search-fields))
         (exact-texts  (mapcar #'bibtex-autokey-get-field lib-scholar-search-fields-exact))
         (exact-string (s-join " " (mapcar #'(lambda (x) (format "\"%s\"" x))
                                           (-filter #'(lambda (x) (not (string-empty-p x))) exact-texts))))
         (all-terms (s-concat exact-string " " (s-join " " search-texts)))
         (cleaned (s-replace-regexp "{.+?\\(\\w\\)}" "\\1" all-terms))
         )
    (librarian-online cleaned "Scholar")
    )
  )

(defun lib-lookup-orcid (arg)
  (interactive "P")
  (let* ((fields (split-string (bibtex-autokey-get-field '("author" "editor")) " and " t " +"))
         (chosen (ivy-read "Search For: " fields))
         (cleaned (s-replace-regexp "[^[:word:]]+" "+" chosen))
         )
    (librarian-online cleaned "ORCID")
    )
  )

(defun lib-window-set-downloads ()
  (interactive)
  (let* ((top-wind (split-window-right))
         (bot-wind (with-selected-window top-wind
                     (split-window-below)))
         )
    (with-selected-window top-wind
      (bookmark-jump "downloads")
      )
    (with-selected-window bot-wind
      (bookmark-jump "todo_pdfs")
      )
    )
  )

(defun lib-window-set-dropbox()
  (interactive)
  (let* ((top-wind (split-window-right))
         (bot-wind (with-selected-window top-wind
                     (split-window-below)))
         )
    (with-selected-window top-wind
      (bookmark-jump "dropbox")
      )
    (with-selected-window bot-wind
      (bookmark-jump "todo_pdfs")
      )
    )
  )

(defun lib-window-file-folder ()
  " Find the folder in which the entry's associated file exists "
  (interactive)
  (let* ((target (bibtex-autokey-get-field '("file" "OPTfile"))))
    (when (and (not (string-empty-p target)) (f-exists? (f-parent target)))
      (message "Opening %s" (f-parent target))
      (find-file-other-window (f-parent target))
      (goto-char (point-min))
      (search-forward (f-filename target))
      t
      )
    )
  )

(defun lib-window-dwim ()
  (interactive)
  (let ((entry-start (save-excursion (bibtex-beginning-of-entry) (point)))
        (entry-end (save-excursion (bibtex-end-of-entry) (point)))
        )
    (if (and (<= entry-start (point))
             (<= (point) entry-end))
        (unless (lib-window-file-folder)
          (lib-window-set-downloads))
      (lib-window-set-downloads))
    )
  )

(defun lib-sort-buffer-by-year ()
  (interactive)
  (let ((bibtex-autokey-year-length 4)
        (bibtex-maintain-sorted-entries (list
                                         #'(lambda () (string-to-number (lib-autokey-get-year)))
                                         #'<)))
    (bibtex-sort-buffer)
    )
  )

(defun lib-sort-buffer-by-type ()
  (interactive)
  (let ((bibtex-autokey-year-length 4)
        (bibtex-maintain-sorted-entries 'entry-class)
        )
    (bibtex-sort-buffer)
    )
  )

(defun lib-autokey-get-year ()
  "Return year field contents as a string obeying `bibtex-autokey-year-length'."
  (let* ((str (bibtex-autokey-get-field '("date" "year"))) ; possibly ""
         (year (or (and (iso8601-valid-p str)
                        (let ((year (decoded-time-year (iso8601-parse str))))
                          (and year (number-to-string year))))
                   ;; BibTeX permits a year field "(about 1984)", where only
                   ;; the last four nonpunctuation characters must be numerals.
                   (and (string-match "\\([0-9][0-9][0-9][0-9]\\)[^[:alnum:]]*\\'" str)
                        (match-string 1 str))
                   (user-error "%s : Year or date field `%s' invalid" (bibtex-autokey-get-title) str))))
    (substring year (max 0 (- (length year) bibtex-autokey-year-length)))))

;;-- end commands

(provide 'librarian--biblio-edit)

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    December 17, 2024
;; Modified:   December 17, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; librarian--biblio-edit.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("lib-" . "librarian--biblio-edit-")
;; )
;; End:
