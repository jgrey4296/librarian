;;; librarian--biblio-clean.el -*- lexical-binding: t; no-byte-compile: t; -*-
(eval-when-compile
  (require 'bibtex)
  )

(defvar librarian--biblio-clean-indent-equals-column 14)
(defvar librarian--biblio-clean-remove-field-newlines-regexp "^=")

(defvar librarian--biblio-clean-default-stubkey-base "stub_key_")

(defvar librarian--biblio-clean-hooks '(librarian--biblio-clean-insert-stub-key-h ;; Initial key
                          ;; Initial formatting
                          librarian--biblio-clean-remove-empty-fields-h
                          librarian--biblio-clean-dont-break-lines-h
                          librarian--biblio-clean-normalise-symbols-h
                          ;; Specific fields
                          librarian--biblio-clean-format-doi-h
                          librarian--biblio-clean-check-file-h
                          librarian--biblio-clean-expand-shortened-url-h
                          ;; generate key
                          librarian--biblio-clean-orcb-key-h
                          librarian--biblio-clean-insert-volume-to-key-h
                          ;; Final alignment and indent
                          librarian--biblio-clean-remove-whitespace-h
                          librarian--biblio-clean-align-h
                          librarian--biblio-clean-indent-h
                          )
  )

(defvar librarian--biblio-clean-clean-move-entry-on-fail nil)

(defvar librarian--biblio-clean-curl-cmd      "curl")

(defvar librarian--biblio-clean-curl-args     '("-sLI" "--connect-timeout" "3"))


;;-- hooks
(defun librarian--biblio-clean-insert-stub-key-h ()
  "Insert a stub key if there isnt an actual one"
  (bibtex-beginning-of-entry)
  (search-forward "{" (line-end-position) t)
  (if (looking-at ",")
      (insert (format "%s%s" librarian--biblio-clean-default-stubkey-base (random 5000)))
    )
  )

(defun librarian--biblio-clean-remove-empty-fields-h ()
  (bibtex-beginning-of-entry)
  (while (re-search-forward "\\(ALT\\|OPT\\).+= {},?$" nil t)
    (kill-region (line-beginning-position) (line-end-position))
    (join-line)
    )
  )

(defun librarian--biblio-clean-dont-break-lines-h ()
  " Remove newlines from entries "
  (bibtex-beginning-of-entry)
  (beginning-of-line)
  (let* ((entry (bibtex-parse-entry))
         (keys (mapcar #'car entry))
         (paths (-reject #'(lambda (x) (string-match librarian--biblio-clean-remove-field-newlines-regexp x)) keys))
         (path-texts (mapcar #'bibtex-autokey-get-field paths))
         (path-cleaned (mapcar #'(lambda (x) (replace-regexp-in-string "\n+ *" " " x)) path-texts))
         )
    ;; Then update:
    (mapc #'(lambda (x) (bibtex-set-field (car x) (cdr x))) (-zip-pair paths path-cleaned))
    )
  )

(defun librarian--biblio-clean-normalise-symbols-h ()
  " Replace &amp; with &, and @ with \@
But not in urls
"
  (bibtex-beginning-of-entry)
  (let* ((keys (mapcar #'car (bibtex-parse-entry)))
         (focus (-filter #'(lambda (x) (not (string-match "=\\|url\\|doi\\|file" x))) keys))
         (texts (mapcar #'bibtex-autokey-get-field focus))
         (amps (mapcar #'(lambda (x) (replace-regexp-in-string " &amp; " " & " x)) texts))
         (ats (mapcar #'(lambda (x) (replace-regexp-in-string "@" "\\\\@" x)) amps))
         )
    ;; Then update:
    (mapc #'(lambda (x) (bibtex-set-field (car x) (cdr x))) (-zip-pair focus ats))
    )
  )

(defun librarian--biblio-clean-format-doi-h ()
  "Remove http://dx.doi.org/ in the doi field.
Used instead of org-ref-bibtex-format-url-if-doi
and orcb-clean-doi
"
  (let ((doi (bibtex-autokey-get-field "doi")))
    (when (ffap-url-p  doi)
      (bibtex-beginning-of-entry)
      (goto-char (car (cdr (bibtex-search-forward-field "doi" t))))
      (bibtex-kill-field)
      (bibtex-make-field "doi")
      (backward-char)
      (insert (replace-regexp-in-string "^http.*?\.org/" "" doi)))))

(defun librarian--biblio-clean-check-file-h ()
  " check any files mentioned actually exist "
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
         (file-likes (-filter 'identity (mapcar #'librarian--biblio-clean--get-file-entries entry)))
         )
    (mapc #'librarian--biblio-clean--check-file-exists file-likes)
    )
  )

(defun librarian--biblio-clean--get-file-entries (pair)
  (if (string-match "file" (car pair))
      pair
    nil)
  )

(defun librarian--biblio-clean--check-file-exists (pair)
  (let* ((orig (cdr pair))
         (sub (substring
               (cdr pair)
               1 -1))
         (full-target (if (f-relative? sub)
                          (f-join librarian--biblio-clean-pdf-loc sub)
                        (expand-file-name sub))))
    (cl-assert (eq (string-to-char orig) ?{))
    (if (not (f-exists? full-target))
        (signal 'error `("File Not Found: " ,full-target))))
  )

(defun librarian--biblio-clean-expand-shortened-url-h ()
  "Expand a shortened url, using CuRL
https://tecnoysoft.com/en/how-to-obtain-the-real-url-behind-a-shortened-url-using-curl/
 "
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
         (urls (-reject #'(lambda (x) (or (null x) (string-equal (cdr x) ""))) (mapcar #'librarian--biblio-clean--url-matcher entry)))
         (result-buffer (get-buffer-create "*CurlResponse*"))
         expanded
         )
    (when urls (message "Expanding %s urls" (length urls)))
    (cl-loop for urlpair in urls
             do
             (with-current-buffer result-buffer
               (erase-buffer))
             (apply #'call-process librarian--biblio-clean-curl-cmd nil result-buffer nil (append librarian--biblio-clean-curl-args (ensure-list (cdr urlpair))))
             (with-current-buffer result-buffer
               (goto-char (point-min))
               (when (re-search-forward "^location: " nil t)
                 (push (cons (car urlpair)
                             (s-replace "\r" "" (buffer-substring (point) (line-end-position))))
                       expanded))
               )
             )
    (cl-loop for urlpair in expanded
             do
             (bibtex-set-field (car urlpair) (cdr urlpair))
             )
    )
  )

(defun librarian--biblio-clean--url-matcher (x)
  (when (and (string-match "url" (car x))
             (<= (length (cdr x)) 30))
    (cons (car x) (substring (cdr x) 1 -1)))
  )

(defun librarian--biblio-clean-orcb-key-h ()
  "Replace the key in the entry.
Prompts for replacement if the new key duplicates one already in
the file.
Does not modify keys ending in an underscore
 "
  (bibtex-beginning-of-entry)
  ;; (end-of-line)
  ;; (forward-char -2)
  ;; (unless (looking-at "_")
  (search-forward "{" (line-end-position) t)
  (when (looking-at librarian--biblio-clean-default-stubkey-base)
    (let ((key (bibtex-generate-autokey))
          handle-duplicate
          )
      ;; remove any \\ in the key
      (setq key (replace-regexp-in-string "[ \\\\'{}]" "" key))
      ;; first we delete the existing key
      (bibtex-beginning-of-entry)
      (re-search-forward bibtex-entry-maybe-empty-head)
      (if (match-beginning bibtex-key-in-head)
	  (delete-region (match-beginning bibtex-key-in-head)
		         (match-end bibtex-key-in-head)))
      ;; check if the key is in the buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (when (bibtex-search-entry key)
	    (bibtex-search-entry key)
	    (bibtex-copy-entry-as-kill)
            (setq handle-duplicate t)
            )
          )
        )

      (when handle-duplicate
        (save-window-excursion
          (switch-to-buffer-other-window "*duplicate entry*")
          (bibtex-yank)
          (setq key (bibtex-read-key "Duplicate Key found, edit: " key))
          (kill-buffer "*duplicate entry*")
          )
        )
      (insert key)
      (kill-new key))
    )
  )

(defun librarian--biblio-clean-insert-volume-to-key-h ()
  (bibtex-beginning-of-entry)
  (search-forward "{" (line-end-position) t)
  (let ((vol (s-replace " " "_" (bibtex-autokey-get-field "volume"))))
    (unless (or (s-equals? vol "") (looking-at ".+?_,") (looking-at (format ".+?_%s" vol)))
      (goto-char (- (line-end-position) 1))
      (insert (format "_%s" vol))
      )
    )
  )

(defun librarian--biblio-clean-remove-whitespace-h ()
  " Remove newlines from entries "
  (bibtex-beginning-of-entry)
  (beginning-of-line)
  (let* ((entry (bibtex-parse-entry))
         (main (-reject #'(lambda (x) (s-contains? "=" (car x))) entry))
         (cleaned (mapcar #'(lambda (x) (cons (car x) (s-trim (substring (cdr x) 1 -1)))) main))
         )
    ;; Then update:
    (mapc #'(lambda (x) (bibtex-set-field (car x) (cdr x))) cleaned)
    )
  )

(defun librarian--biblio-clean-align-h ()
  " Aligns a bibtex entry's '{field} =' "
  (let (start end)
    (bibtex-beginning-of-entry)
    (setq start (line-beginning-position 2))
    (bibtex-end-of-entry)
    (setq end (line-end-position 0))
    (align-regexp start end "\\(\s+?\\)[a-z]" 1 1 nil)
    (align-regexp start end "\\(\s+?\\)=" 1 1 nil)
    (bibtex-end-of-entry)
    (setq end (line-end-position 0))
    (align-regexp start end "\\(.+?=\\)\\(\s+?\\)[{0-9\"]" 2 1 nil)
    )
  )

(defun librarian--biblio-clean-indent-h ()
  " Indent all fields to librarian--biblio-clean-indent-equals-column "
  (bibtex-beginning-of-entry)
  (while (re-search-forward "^.+?= {" nil t)
    (backward-char 3)
    (indent-to-column librarian--biblio-clean-indent-equals-column)

    )
  )

;;-- end hooks

(defun librarian--biblio-clean-bibtex-entry-commas ()
  " Ensure all fields have a comma at the end of the line"
  (bibtex-beginning-of-entry)
  (end-of-line)
  (if (not (looking-at-p ","))
      (insert ","))
  (while (re-search-forward "= {.+}$" nil t)
    (insert ",")
    )
  )

(defun librarian--biblio-clean-reformat-buffer (&optional read-options)
  "Reformat all BibTeX entries in buffer or region.
Without prefix argument, reformatting is based on `bibtex-entry-format'.
With prefix argument, read options for reformatting from minibuffer.
With \\[universal-argument] \\[universal-argument] prefix argument, reuse previous answers (if any) again.
If mark is active reformat entries in region, if not in whole buffer."
  (interactive "*P")
  (let* ((pnt (point))
         )
    (save-restriction
      (if mark-active (narrow-to-region (region-beginning) (region-end)))
      (bibtex-progress-message "Formatting" 1)
      (bibtex-map-entries (lambda (_key _beg _end)
                            (bibtex-progress-message)
                            (org-ref-clean-bibtex-entry)))
      (bibtex-progress-message 'done))
    (goto-char pnt)))

(defun librarian--biblio-clean-error-move-toggle ()
  (interactive)
  (setq librarian--biblio-clean-clean-move-entry-on-fail (not librarian--biblio-clean-clean-move-entry-on-fail))
  (message "Error on clean entry %s move to end of file" (if librarian--biblio-clean-clean-move-entry-on-fail
                                                             "will"
                                                           "will not"))
  )

(defun librarian--biblio-clean-ensure-newline-before-def ()
  (while (re-search-forward "\\(\n\\)\\(@.+?{.+?,\\)$" nil t)
    (goto-char (match-end 1))
    (insert "\n")
    (goto-char (match-end 0))
    )
  )

(defun librarian--biblio-clean-sort-entry ()
  "Use org-ref-sort-bibtex-entry, but narrowed to the entry"
  (interactive)
  (save-restriction
    (narrow-to-defun)
    (condition-case err
        (org-ref-sort-bibtex-entry)
      (error (message "Error: %s" err))
      )
    (bibtex-beginning-of-entry)
    (org-ref-clean-bibtex-entry)
    )
  )

(provide 'librarian--biblio-clean)

;;; librarian--biblio-clean.el ends here
