;;; librarian--biblio-edit.el -*- lexical-binding: t; no-byte-compile: t; -*-

(eval-when-compile
  (require 'bookmark)
  (require 'bibtex)

  (declare-function 'librarian-browse-open 'librarian--browse)
  )

;;-- vars

(defvar librarian--biblio-edit-fill-column 50000)

(defvar librarian--biblio-edit-helm-candidates nil)

(defvar librarian--biblio-edit-candidates-names '())

(defvar librarian--biblio-edit-rand-log ".emacs_rand_bib_log")

(defvar librarian--biblio-edit-open-doi-with-pdf        nil)

(defvar librarian--biblio-edit-open-url-with-pdf        nil)

(defvar librarian--biblio-edit-todo-loc                 nil)

(defvar librarian--biblio-edit-todo-files-loc           nil)

(defvar librarian--biblio-edit-completions-loc          nil)

(defvar librarian--biblio-edit-export-bib-loc           nil)

(defvar librarian--biblio-edit-temp-tex-loc             nil)

(defvar librarian--biblio-edit-search-fields               '("tags" "year" "publisher"))

(defvar librarian--biblio-edit-scholar-search-fields       '("author" "editor" "ALTauthor" "Alteditor" "year" "doi" "isbn"))

(defvar librarian--biblio-edit-scholar-search-fields-exact '("title"))

(defvar librarian--biblio-edit-doi-url               "https://doi.org/%s")

(defvar librarian--biblio-edit-completion-display-formats
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

(setq bibtex-completion-display-formats librarian--biblio-edit-completion-display-formats)

(defvar librarian--biblio-edit-pdf-loc-regexp               "file[[:digit:]]*\s*=\s*{\\(.+\\)/\\(.+pdfs\\)?")

(defvar librarian--biblio-edit-pdf-replace-match-string     "~/")

(defvar librarian--biblio-edit-pdf-replace-library-string   "pdfs")

(defvar bibtex-completion-pdf-open-function 'browse-url)

(defvar librarian--biblio-edit-downloads-bookmark "downloads")

(defvar librarian--biblio-edit-dropbox-bookmark "dropbox")

(defvar librarian--biblio-edit-todos-bookmark "todo.pdfs")
;;-- end vars

;;-- commands

(defun librarian--biblio-edit-entry-type ()
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

(defun librarian--biblio-edit-copy-entry ()
  " Copy the entire entry under point "
  (interactive)
  (save-excursion
    (let ((start (bibtex-beginning-of-entry))
          (end (bibtex-end-of-entry)))
      (copy-region-as-kill start end)
      )
    )
  )

(defun librarian--biblio-edit-copy-key ()
  " Copy the cite key of the entry under point "
  (interactive)
  (kill-new (bibtex-completion-get-key-bibtex))
  (message "Copied Key: %s" (current-kill 0 t))
  )

(defun librarian--biblio-edit-copy-title ()
  (interactive)
  (kill-new (bibtex-autokey-get-field "title"))
  (message "Copied Title: %s" (current-kill 0 t))
  )

(defun librarian--biblio-edit-copy-field ()
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((entry (bibtex-parse-entry))
           (fields (-reject #'(lambda (x) (s-contains? "=" x)) (mapcar 'car entry)))
           (selected (ivy-read "Field to Copy: " fields)))
      (kill-new (bibtex-autokey-get-field selected)))
    )
  )

(defun librarian--biblio-edit-quickswap ()
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

(defun librarian--biblio-edit-rename-file ()
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

(defun librarian--biblio-edit-dired-unify-pdf-locations ()
  "Unify bibtex pdf paths of marked files"
  (interactive)
  (seq-each 'librarian--biblio-edit-unify-pdf-locations-in-file (dired-get-marked-files))
  )

(defun librarian--biblio-edit-unify-pdf-locations-in-file (name)
  "Change all pdf locations in bibtex file to relative,
ensuring they work across machines "
  (message "Unifying Locations in %s" name)
  (with-temp-buffer
    (insert-file-contents name t)
    (goto-char (point-min))
    (while (re-search-forward librarian--biblio-edit-pdf-loc-regexp nil t)
      (replace-match librarian--biblio-edit-pdf-replace-match-string nil nil nil 1)
      (when (eq 6 (length (match-data)))
        (replace-match librarian--biblio-edit-pdf-replace-library-string t nil nil 2))
      )
    (write-file name)
    )
  )

(defun librarian--biblio-edit-swap-editor-author ()
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

(defun librarian--biblio-edit-swap-booktitle-journal ()
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

(defun librarian--biblio-edit-open-pdf (&optional path)
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
             (message "File does not exist: %s" target))
            (t (call-process "open" nil nil nil target))
            )

      (when librarian--biblio-edit-open-doi-with-pdf
          (librarian--biblio-edit-open-doi))
      (when librarian--biblio-edit-open-url-with-pdf
          (librarian--biblio-edit-open-url))
      )))

(defun librarian--biblio-edit-open-url ()
  " Open the current entry's url in browser "
  (interactive)
  (when (bibtex-text-in-field "url")
    (librarian-browse-open (bibtex-text-in-field "url")))
  )

(defun librarian--biblio-edit-open-doi ()
  " Follow the doi link of the current entry in a browser "
  (interactive)
  (when (bibtex-text-in-field "doi")
    (browse-url (format librarian--biblio-edit-doi-url (bibtex-text-in-field "doi")))
    )
  )

(defun librarian--biblio-edit-open-folder ()
  " Open the associated file's folder in finder "
  (interactive)
  (let* ((target (bibtex-autokey-get-field '("file" "OPTfile"))))
    (when (and (not (string-empty-p target)) (f-exists? target))
      (message "Opening %s" target)
      (shell-command (concat "open " (shell-quote-argument (f-parent target))))
      )
    )
  )

(defun librarian--biblio-edit-load-random ()
  " Run in a bibtex file, opens a random entry externally,
      and logs it has been opened in a separate file.

Log into librarian--biblio-edit-rand-log.
 "
  (interactive)
  (widen)
  (let* ((location (f-dirname (buffer-file-name)))
         (log_file (f-join location librarian--biblio-edit-rand-log))
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
            (librarian--biblio-edit-open-pdf)
            (setq entry nil)
            )
          )
        )
      )
    )
  )

(defun librarian--biblio-edit-quicklook-pdf ()
  "Open pdf for a bibtex entry, if it exists.
assumes point is in
the entry of interest in the bibfile.  but does not check that."
  (interactive)
  (save-excursion
    (let* ((target (bibtex-autokey-get-field '("file" "OPTfile"))))
      (message "%s : %s" target (file-exists-p target))
      (browse-url target 'quicklook)
      )))

(defun librarian--biblio-edit-use-newest-file ()
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
    (when (f-exists? (f-join librarian--biblio-edit-todo-files-loc (f-filename newest)))
      (user-error "File already exists in todo directory: %s" newest)
      )
    (message "Newest: %s" newest)
    (f-move newest (f-join librarian--biblio-edit-todo-files-loc (f-filename newest)))
    (bibtex-beginning-of-entry)
    (save-excursion
      (bibtex-set-field "file" (f-join librarian--biblio-edit-todo-files-loc (f-filename newest)))
      )
    )
)

(defun librarian--biblio-edit-kill-entry-key ()
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

(defun librarian--biblio-edit-lock-key ()
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

(defun librarian--biblio-edit-subcite ()
  (interactive)
  (librarian--biblio-edit-lock-key)
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
    (insert (format "{%s%s,\n" librarian--biblio-clean-default-stubkey-base (random 5000)))
    (insert "year = " year ",\n")
    (insert "crossref = {" key "},\n")
    (insert "title = {},\n")
    (insert "author = {},\n")
    (insert "}")
    (bibtex-beginning-of-entry)
    (librarian--biblio-edit-edit-field "title")
    (librarian--biblio-edit-edit-field "author")
    (librarian--biblio-edit-edit-field "pages")
    )
  )

(defun librarian--biblio-edit-title-case (x)
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

(defun librarian--biblio-edit-toggle-doi-load ()
  (interactive)
  (setq librarian--biblio-edit-open-doi-with-pdf (not librarian--biblio-edit-open-doi-with-pdf))
  (message "Open DOI on pdf? %s" librarian--biblio-edit-open-doi-with-pdf)
  )

(defun librarian--biblio-edit-toggle-url-load ()
  (interactive)
  (setq librarian--biblio-edit-open-url-with-pdf (not librarian--biblio-edit-open-url-with-pdf))
  (message "Open URL on pdf? %s" librarian--biblio-edit-open-url-with-pdf)
  )

(defun librarian--biblio-edit-visual-select-entry ()
  " Evil visual select the current entry "
  (interactive)
  (evil-visual-make-region (bibtex-beginning-of-entry)
                           (bibtex-end-of-entry))
)

(defun librarian--biblio-edit-goto-crossref-entry ()
  " Follow the crossref field in the entry "
  (interactive)
  (when (bibtex-text-in-field "crossref")
    (bibtex-find-crossref (bibtex-text-in-field "crossref"))
    )
  )

(defun librarian--biblio-edit-google-scholar (arg)
  "Open the bibtex entry at point in google-scholar by its doi.
With arg, searchs the dplp instead.
"
  (interactive "P")
  (let* ((search-texts (mapcar #'bibtex-autokey-get-field librarian--biblio-edit-scholar-search-fields))
         (exact-texts  (mapcar #'bibtex-autokey-get-field librarian--biblio-edit-scholar-search-fields-exact))
         (exact-string (s-join " " (mapcar #'(lambda (x) (format "\"%s\"" x))
                                           (-filter #'(lambda (x) (not (string-empty-p x))) exact-texts))))
         (all-terms (s-concat exact-string " " (s-join " " search-texts)))
         (cleaned (s-trim (s-replace-regexp "{.+?\\(\\w\\)}" "\\1" all-terms)))
         )
    (librarian-online cleaned "Scholar" t)
    )
  )

(defun librarian--biblio-edit-lookup-orcid (arg)
  (interactive "P")
  (let* ((fields (split-string (bibtex-autokey-get-field '("author" "editor")) " and " t " +"))
         (chosen (ivy-read "Search For: " fields))
         (cleaned (s-replace-regexp "[^[:word:]]+" "+" chosen))
         )
    (librarian-online cleaned "ORCID")
    )
  )

(defun librarian--biblio-edit-window-set-downloads ()
  (interactive)
  (let* ((top-wind (split-window-right))
         (bot-wind (with-selected-window top-wind
                     (split-window-below)))
         )
    (with-selected-window top-wind (bookmark-jump librarian--biblio-edit-downloads-bookmark))
    (with-selected-window bot-wind (bookmark-jump librarian--biblio-edit-todos-bookmark))
    )
  )

(defun librarian--biblio-edit-window-set-dropbox()
  (interactive)
  (let* ((top-wind (split-window-right))
         (bot-wind (with-selected-window top-wind
                     (split-window-below)))
         )
    (with-selected-window top-wind (bookmark-jump librarian--biblio-edit-dropbox-bookmark))
    (with-selected-window bot-wind (bookmark-jump librarian--biblio-edit-todos-bookmark))
    )
  )

(defun librarian--biblio-edit-window-file-folder ()
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

(defun librarian--biblio-edit-window-dwim ()
  (interactive)
  (let ((entry-start (save-excursion (bibtex-beginning-of-entry) (point)))
        (entry-end (save-excursion (bibtex-end-of-entry) (point)))
        )
    (if (and (<= entry-start (point))
             (<= (point) entry-end))
        (unless (librarian--biblio-edit-window-file-folder)
          (librarian--biblio-edit-window-set-downloads))
      (librarian--biblio-edit-window-set-downloads))
    )
  )

(defun librarian--biblio-edit-sort-buffer-by-year ()
  (interactive)
  (let ((bibtex-autokey-year-length 4)
        (bibtex-maintain-sorted-entries (list
                                         #'(lambda () (string-to-number (librarian--biblio-edit-autokey-get-year)))
                                         #'<)))
    (bibtex-sort-buffer)
    )
  )

(defun librarian--biblio-edit-sort-buffer-by-type ()
  (interactive)
  (let ((bibtex-autokey-year-length 4)
        (bibtex-maintain-sorted-entries 'entry-class)
        )
    (bibtex-sort-buffer)
    )
  )

(defun librarian--biblio-edit-autokey-get-year ()
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

;;; librarian--biblio-edit.el ends here
