;;; librarian-bibliography.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey@Johns-Mac-mini.local>
;; Created: September 04, 2023
;; Modified: September 04, 2023
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
;;  TODO for getting entries from bibtex
;;
;;; Code:
;;-- end header

(require 'bibtex)
(require 'bibtex)
(require 'org-ref-bibtex)

(defvar librarian-curl-cmd  "curl")

(defvar librarian-curl-args  '("-sLI"))

(defconst jg-bibtex-meta-buffer "*Metadata*")

(defconst jg-bibtex-meta-program "ebook-meta")

(defconst jg-bibtex-meta-opts '(("title     " . "-t")
                                ("author    " . "-a")
                                ("comments  " . "-c")
                                ("publisher " . "-p")
                                ("series    " . "-s")
                                ("number    " . "-i")
                                ("rating    " . "-r")
                                ("date      " . "-d")
                                ("isbn      " . "--isbn")
                                ("ident     " . "--identifier=")
                                ("tags      " . "--tags=")
                                ("category  " . "--category=")
                                ("*Apply*")))

(defun +jg-bibtex-build-list ()
  "Build a list of all bibtex files to use for bibtex-helm "
  (interactive)
  (setq bibtex-completion-bibliography (directory-files jg-bibtex-loc-bibtex 't "\.bib$")
        jg-bibtex-helm-candidates nil
        )
  (bibtex-completion-clear-cache)
  (bibtex-completion-init)
  (mapcar #'+jg-bibtex-process-candidates (bibtex-completion-candidates))
  )

(defun +jg-bibtex-meta-retrieval ()
  " Use 'jg-bibtex-meta-program to retrieve metadata about files in current bibtex entry "
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((entry (bibtex-parse-entry))
           (files (-filter #'identity (mapcar #'+jg-bibtex-get-files-fn entry)))
           (procs (cl-loop for file in files
                            collect
                            (start-process
                             (format "bib:meta:%s" (f-base file))
                             (format "*bib:meta:%s*" (f-base file))
                             "ebook-meta"
                             (expand-file-name
                              (if (f-relative? file) (f-join jg-bibtex-pdf-loc file) file))
                             )
                            )
                   )
           )
      (with-process-wrap! jg-bibtex-meta-buffer procs)
      )
    )
  )

(defun +jg-bibtex-apply-meta ()
  " Use Calibre's ebook-meta program to take bibtex data and apply it to a pdf or epub "
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((arg-pairs nil)
           (entry (bibtex-parse-entry))
           (targets (-filter #'identity (mapcar #'+jg-bibtex-get-files-fn entry)))
           (keys (mapcar #'car entry))
           (meta-opts jg-bibtex-meta-opts)
          current
          options
          )
      (unless targets
        (error "No files to apply to"))
      ;; select fields and wrap
      (while (not (s-equals? "*Apply*" (setq current (ivy-read "Assign to option: " meta-opts))))
        (when (alist-get current meta-opts nil nil #'equal)
          (push (cons  (alist-get current meta-opts nil nil #'equal)
                       (let* ((field (ivy-read "Select Field: " keys))
                              (text (bibtex-autokey-get-field field)))
                         (if (string-empty-p text) field text)))
                arg-pairs)
          )
        )
      (unless arg-pairs
        (error "Nothing selected to apply"))

      ;; Convert ready for use as args
      (setq options (cl-loop for pair in arg-pairs
                             append (list (car pair) (cdr pair))))
                             ;; (format (if (s-matches? "=$" (car pair))
                             ;;             "%s\"%s\"" "%s \"%s\"")
                             ;;         (car pair) (cdr pair))
                             ;; )

      (ivy-read "Apply Metadata to: "
                (mapcar #'(lambda (x) (cons (f-base x) x)) targets)
                :multi-action (-partial #'+jg-bibtex-apply-meta-fn options)
                :action (-partial #'+jg-bibtex-apply-meta-solo-fn options)
                )

      )
    )
  )

(defun +jg-bibtex-apply-meta-solo-fn (args file)
  (+jg-bibtex-apply-meta-fn args (list file))
  )

(defun +jg-bibtex-apply-meta-fn (args files)
  (with-process-wrap! jg-bibtex-meta-buffer
                      (cl-loop for file in files
                               collect
                               (apply #'start-process
                                      (format "meta:apply:%s" (f-base (cdr file)))
                                      (format "*meta:apply:%s*" (f-base (cdr file)))
                                      jg-bibtex-meta-program
                                      (cdr file)
                                      args
                                      )
                               )
                      )
  (with-current-buffer jg-bibtex-meta-buffer
    (insert (format "Process Args: %s\n" args))
    )
  )

(defun +jg-bibtex-set-ebook-cover ()
  " Use Calibre's ebook-meta program to select an image and apply it as an epub's cover image "
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((arg-pairs nil)
           (entry (bibtex-parse-entry))
           (ebook nil)
           (cover nil)
           (target (start-process
                    ("meta:cover:%s"
                    jg-bibtex-meta-program
                    (bibtex-autokey-get-field "file"))))

           )
      )
    )
  )

(defun +jg-bibtex-update-entry ()
  (interactive)
  (when (org-ref-bibtex-entry-doi)
    (doi-utils-update-bibtex-entry-from-doi (org-ref-bibtex-entry-doi))
    )
  )

(defun +jg-bibtex-insert-entry-from-doi ()
  (interactive)
  (let* ((doi (read-string "Doi: "))
         (results (funcall doi-utils-metadata-function doi))
         (type (plist-get results :type))
         (bibtex (-some (lambda (g) (funcall g type results)) doi-utils-bibtex-type-generators))
         )
    (cond ((not bibtex)
           (insert "@misc{,\n")
           (while results
             (let ((key (string-replace ":" "" (symbol-name (pop results))))
                   (value (pop results)))
               (if (not (vectorp value))
                   (insert (format "%s = {%s},\n" key value))
                 (dolist (x (append value nil))
                   (insert (format "%s = {%s},\n" key (-remove #'keywordp x)))
                   )
                 )
               )
             )
           (insert "}")
           )
          (t
           (insert bibtex)
           (backward-char)
           ;; set date added for the record
           (let ((ts (funcall doi-utils-timestamp-format-function)))
             (when ts
               (bibtex-set-field doi-utils-timestamp-field
			         ts)))
           (org-ref-clean-bibtex-entry)
           (save-buffer))
          )
    )
  )

(defun +jg-bibtex-refile-pdf (&optional destructive)
  " Refile a pdf from its location to its pdflib/year/author loc
returns the new location
"
  (when destructive
      (message "Destructive Refile"))
  (save-excursion
    (let* ((entry  (bibtex-parse-entry))
           (author (s-capitalize (bibtex-autokey-get-names)))
           (year   (bibtex-text-in-field "year"))
           (files  (-filter #'identity (mapcar #'+jg-bibtex-get-files-fn entry)))
           (pdflib jg-bibtex-pdf-loc)
           (finalpath (f-join pdflib year author))
           newlocs)
      (make-directory finalpath 'parents)

      (cl-loop for file in files
            do
            (let* ((fname (f-filename file))
                   (target (f-join finalpath fname))
                   )
              (message "Relocating %s to %s" file target)
              (if (s-equals? "y" (read-string (format "%sRefile to %s? " (if destructive "Destructive " "") target)))
                  (progn (cl-assert (not (f-exists? target)))
                         (if destructive
                             (f-move file target)
                           (progn (f-copy file target)
                                  (f-move file (f-join (f-parent file) (format "_refiled_%s" fname)))))
                         (push target newlocs))
                (push file newlocs))
              )
            )

      ;; Update entry with new locations
      (cl-loop for file in newlocs
            with count = 1
            do
            (bibtex-set-field (format "file%s" (if (eq count 1) "" count)) file)
            (cl-incf count)
            )
      )
    )
  )

(defun +jg-bibtex-refile-by-year ()
  " Kill the current entry and insert it in the appropriate year's bibtex file "
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((year (bibtex-text-in-field "year"))
         (year-file (format "%s.bib" year))
         (response (when year (read-string (format "Refile to %s? " year-file))))
         (target (if (or (s-equals? "y" response) (string-empty-p response))
                     (f-join jg-bibtex-loc-bibtex year-file)
                   (completing-read "Bibtex file: "
                                    (f-entries bib-path
                                               (lambda (f) (f-ext? f "bib"))))))
         )
    (unless (f-exists? target)
      (f-touch target)
      )
    (+jg-bibtex-refile-pdf current-prefix-arg)
    (bibtex-kill-entry)
    (with-temp-buffer
      (insert-file-contents target)
      (goto-char (point-max))
      (insert "\n")
      (bibtex-yank)
      (write-file target nil)
      )
    )
  )

(defun +jg-bibtex-refile-to-unsourced ()
  " Kill the current entry and insert it in the appropriate year's bibtex file "
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((target jg-bibtex-unsourced-bib-file)
         (response (read-string "Refile to unsourced? "))
         )
    (unless (f-exists? target) (f-touch target))
    (bibtex-kill-entry)
    (with-temp-buffer
      (insert-file-contents target)
      (goto-char (point-max))
      (insert "\n")
      (bibtex-yank)
      (write-file target nil)
      )
    )
)
(provide 'librarian-bibliography)
;;; librarian-bibliography.el ends here
