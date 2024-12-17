;;; librarian--biblio-clean.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(eval-when-compile
  (require 'bibtex)
  )

(defvar jg-bibtex-default-stubkey-base "stub_key_")

;;-- hooks

(defun +jg-bibtex-latex-normalise ()
  "Replace non-ascii characters in a bibtex entry.
but not in file or url entries
"
  (interactive)
    (goto-char (point-min))
    (dolist (char (mapcar (lambda (x)
			    (car x))
			  org-ref-nonascii-latex-replacements))
      (while (re-search-forward char nil t)
        (if (not (string-match "\\(file\\|url\\|doi\\).*?=" (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (replace-match (cdr (assoc char org-ref-nonascii-latex-replacements)))
          (goto-char (line-end-position))
          )
        )
      (goto-char (point-min))))

(defun +jg-bibtex-align-hook ()
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

(defun +jg-bibtex-indent-hook ()
  " Indent all fields to jg-bibtex-indent-equals-column "
  (bibtex-beginning-of-entry)
  (while (re-search-forward "^.+?= {" nil t)
    (backward-char 3)
    (indent-to-column jg-bibtex-indent-equals-column)

    )
  )

(defun +jg-bibtex-bibtex-entry-commas ()
  " Ensure all fields have a comma at the end of the line"
  (bibtex-beginning-of-entry)
  (end-of-line)
  (if (not (looking-at-p ","))
      (insert ","))
  (while (re-search-forward "= {.+}$" nil t)
    (insert ",")
    )
  )

(defun +jg-bibtex-normalise-symbols()
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

(defun +jg-bibtex-dont-break-lines-hook()
  " Remove newlines from entries "
  (bibtex-beginning-of-entry)
  (beginning-of-line)
  (let* ((entry (bibtex-parse-entry))
         (keys (mapcar #'car entry))
         (paths (-reject #'(lambda (x) (string-match jg-bibtex-remove-field-newlines-regexp x)) keys))
         (path-texts (mapcar #'bibtex-autokey-get-field paths))
         (path-cleaned (mapcar #'(lambda (x) (replace-regexp-in-string "\n+ *" " " x)) path-texts))
         )
    ;; Then update:
    (mapc #'(lambda (x) (bibtex-set-field (car x) (cdr x))) (-zip-pair paths path-cleaned))
    )
  )

(defun +jg-bibtex-clean-whitespace-hook()
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

(defun +jg-bibtex-remove-empty-fields ()
  (bibtex-beginning-of-entry)
  (while (re-search-forward "\\(ALT\\|OPT\\).+= {},?$" nil t)
    (kill-region (line-beginning-position) (line-end-position))
    (join-line)
    )
  )

(defun +jg-bibtex-reformat-buffer (&optional read-options)
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

(defun +jg-bibtex-check-file-hook ()
  " check any files mentioned actually exist "
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
         (file-likes (-filter 'identity (mapcar #'+jg-bibtex--get-file-entries entry)))
        )
    (mapc #'+jg-bibtex--check-file-exists file-likes)
    )
  )

(defun +jg-bibtex--check-file-exists (pair)
  (let* ((orig (cdr pair))
         (sub (substring
               (cdr pair)
               1 -1))
         (full-target (if (f-relative? sub)
                          (f-join jg-bibtex-pdf-loc sub)
                        (expand-file-name sub))))
    (cl-assert (eq (string-to-char orig) ?{))
    (if (not (f-exists? full-target))
        (signal 'error `("File Not Found: " ,full-target))))
  )

(defun +jg-bibtex--get-file-entries (pair)
  (if (string-match "file" (car pair))
      pair
    nil)
  )

(defun +jg-bibtex-insert-stub-key ()
  "Insert a stub key if there isnt an actual one"
  (bibtex-beginning-of-entry)
  (search-forward "{" (line-end-position) t)
  (if (looking-at ",")
      (insert (format "%s%s" jg-bibtex-default-stubkey-base (random 5000)))
    )
  )

(defun +jg-bibtex-orcb-key-hook ()
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
  (when (looking-at jg-bibtex-default-stubkey-base)
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

(defun +jg-bibtex-insert-volume-to-key ()
  (bibtex-beginning-of-entry)
  (search-forward "{" (line-end-position) t)
  (let ((vol (s-replace " " "_" (bibtex-autokey-get-field "volume"))))
    (unless (or (s-equals? vol "") (looking-at ".+?_,") (looking-at (format ".+?_%s" vol)))
      (goto-char (- (line-end-position) 1))
      (insert (format "_%s" vol))
      )
    )
  )

(defun +jg-bibtex-clean-doi-hook ()
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

(defun +jg-bibtex-isbn-clean ()
  (let ((isbn (bibtex-autokey-get-field "isbn")))
    (unless (string-empty-p isbn)
      (bibtex-set-field "isbn" (s-replace-regexp "[[:blank:]]+" "-" (s-trim isbn)))
      ))
  )

(defun +jg-bibtex--expand-shortened-url ()
  "Expand a shortened url, using CuRL
https://tecnoysoft.com/en/how-to-obtain-the-real-url-behind-a-shortened-url-using-curl/
 "
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
         (urls (-reject #'(lambda (x) (or (null x) (string-equal (cdr x) ""))) (mapcar #'+jg-bibtex--url-matcher entry)))
         (result-buffer (get-buffer-create "*CurlResponse*"))
         expanded
         )
    (when urls (message "Expanding %s urls" (length urls)))
    (cl-loop for urlpair in urls
             do
             (with-current-buffer result-buffer
               (erase-buffer))
             (apply #'call-process jg-bibtex-curl-cmd nil result-buffer nil (append jg-bibtex-curl-args (ensure-list (cdr urlpair))))
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

(defun +jg-bibtex--url-matcher (x)
  (when (and (string-match "url" (car x))
             (<= (length (cdr x)) 30))
    (cons (car x) (substring (cdr x) 1 -1)))
  )
;;-- end hooks

(provide 'librarian--biblio-clean)

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
;;; librarian--biblio-clean.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("lib-" . "librarian--biblio-clean-")
;; )
;; End:
