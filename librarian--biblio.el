;;; librarian-bibliography.el -*- lexical-binding: t; no-byte-compile: t; -*-

(eval-when-compile
  (require 's)
  (require 'bibtex)
  (require 'bibtex-completion)
  (require 'org-ref)
  (require 'macro-tools--processes)
  )

(defvar librarian--biblio-library-loc "" "Where the main bibtex library is located")

(defvar librarian--biblio-pdf-loc     "" "Where relative file paths are located")

(defvar librarian--biblio-unsourced-bib-file "" "target unsourced bib file")

(defconst librarian--biblio-meta-buffer "* Metadata*")

(defconst librarian--biblio-meta-program "ebook-meta")

(defconst librarian--biblio-meta-opts '(("title     " . "-t")
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

(defconst librarian--biblio-full-meta-program "exiftool")

(defconst librarian--biblio-full-meta-args (list
                                    "-g"                    ;; print group headings
                                    "-a"                    ;; allow duplicates
                                    "-u"                    ;; extract unknown tags
                                    ;; "-s"       ;; short tag names
                                    ;; "-n"       ;; no print conversion
                                    ;; "-X"       ;; xml format
                                    ;; "-j" ;; json format
                                    "-pdf:all"              ;; all pdf tags
                                    "-XMP:all"              ;; all xmp tags
                                    "-XML:all"              ;; all xml (epub) tags
                                    "-zip:all"
                                    "-file:filepath"
                                    "-file:filemodifydate"
                                    "-file:filetype"
                                    "-file:filesize"

                                    "--xml:manifest*"       ;; exclude manifest tags
                                    "--xml:spine*"          ;; exclude spine tags
                                    "--xml:guidereference*" ;; exclude guideref tags
                                    ))

(defconst librarian--biblio-meta-opts '(("title     " . "-t")
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

;;;###autoload (autoload 'librarian--biblio-build-list "librarian--biblio" nil t)
(defun librarian--biblio-build-list ()
  "Build a list of all bibtex files to use for bibtex-helm "
  (interactive)
  (setq bibtex-completion-bibliography (directory-files librarian--biblio-library-loc 't "\.bib$"))
  (bibtex-completion-clear-cache)
  (bibtex-completion-init) ;; compile bibtex-completion-display-formats
  ;; TODO: replace this with a non-blocking parse
  (bibtex-completion-candidates)
  )

(defun librarian--biblio-get-files-fn (x)
  " Given a pair, return the cdr if car matches 'file' "
  (when-let ((isfile (string-match "^file[[:digit:]]*" (car x)))
             (text (string-trim (cdr x) "{" "}"))
             )
    (expand-file-name (if (f-relative? text)
                          (f-join librarian--biblio-pdf-loc text)
                        text))
        )
    )

;;;###autoload (autoload 'librarian--biblio-meta-retrieval "librarian--biblio" nil t)
(defun librarian--biblio-meta-retrieval ()
  " Use 'librarian--biblio-meta-program to retrieve metadata about files in current bibtex entry "
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((entry (bibtex-parse-entry))
           (files (-filter #'identity (mapcar #'librarian--biblio-get-files-fn entry)))
           (procs (cl-loop for file in files
                            collect
                           (apply #'start-process
                                  (format "bib:meta:%s" (f-base file))
                                  (format "*bib:meta:%s*" (f-base file))
                                  librarian--biblio-full-meta-program
                                  (-concat librarian--biblio-full-meta-args
                                           (list (expand-file-name
                                                  (if (f-relative? file)
                                                      (f-join librarian--biblio-pdf-loc file)
                                                    file))
                                                 )
                                           )
                                  )
                           )
                  )
           )
      (with-process-wrap! librarian--biblio-meta-buffer
                          #'macro-tools--process-sentinel
                          procs)
      )
    )
  )

(defun librarian--biblio-apply-meta ()
  " Use Calibre's ebook-meta program to take bibtex data and apply it to a pdf or epub "
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((arg-pairs nil)
           (entry (bibtex-parse-entry))
           (targets (-filter #'identity (mapcar #'librarian--biblio-get-files-fn entry)))
           (keys (mapcar #'car entry))
           (meta-opts librarian--biblio-meta-opts)
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
                             append
                             (list (car pair) (cdr pair))))

      (ivy-read "Apply Metadata to: "
                (mapcar #'(lambda (x) (cons (f-base x) x)) targets)
                :multi-action (-partial #'librarian--biblio-apply-meta-fn options)
                :action (-partial #'librarian--biblio-apply-meta-solo-fn options)
                )

      )
    )
  )

(defun librarian--biblio-apply-meta-solo-fn (args file)
  "Apply metadata to a single file"
  (librarian--biblio-apply-meta-fn args (list file))
  )

(defun librarian--biblio-apply-meta-fn (args files)
  "Apply some metadata to some files using librarian--biblio-meta-program"
  (with-process-wrap! librarian--biblio-meta-buffer
                      #'macro-tools--process-sentinel
                      (cl-loop for file in files
                               collect
                               (apply #'start-process
                                      (format "meta:apply:%s" (f-base (cdr file)))
                                      (format "*meta:apply:%s*" (f-base (cdr file)))
                                      librarian--biblio-meta-program
                                      (cdr file)
                                      args
                                      )
                               )
                      )
  (with-current-buffer librarian--biblio-meta-buffer
    (insert (format "Process Args: %s\n" args))
    )
  )

;;;###autoload (autoload 'librarian--biblio-set-ebook-cover "librarian--biblio" nil t)
(defun librarian--biblio-set-ebook-cover ()
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
                    librarian--biblio-meta-program
                    (bibtex-autokey-get-field "file"))))

           )
      )
    )
  )

;;;###autoload (autoload 'librarian--biblio-update-entry "librarian--biblio" nil t)
(defun librarian--biblio-update-entry ()
  " Update an entry using the doi's online data "
  (interactive)
  (when (org-ref-bibtex-entry-doi)
    (doi-utils-update-bibtex-entry-from-doi (org-ref-bibtex-entry-doi))
    )
  )

;;;###autoload (autoload 'librarian--biblio-insert-entry-from-doi "librarian--biblio" nil t)
(defun librarian--biblio-insert-entry-from-doi ()
  "Create an entry from a doi"
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

(defun librarian--biblio-refile-pdf (&optional destructive)
  " Refile a pdf from its location to its pdflib/year/author loc
returns the new location
"
  (when destructive
      (message "Destructive Refile"))
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((entry  (bibtex-parse-entry))
           (author (s-capitalize (bibtex-autokey-get-names)))
           (year   (bibtex-text-in-field "year"))
           (century (replace-regexp-in-string "\\(..\\).." "\\100" year))
           (decade (replace-regexp-in-string "\\(...\\)." "\\10" year))
           (files  (-filter #'identity (mapcar #'librarian--biblio-get-files-fn entry)))
           (pdflib librarian--biblio-pdf-loc)
           (finalpath (f-join pdflib century decade year author))
           newlocs)

      (make-directory finalpath 'parents)

      (setq newlocs (cl-loop for file in files
                             for fname = (f-filename file)
                             for target = (f-join finalpath fname)
                             if (f-exists? target)
                             do (error "File Already Exists at Target: %s" target)
                             else
                             collect (cons file target)
                             ))

      (cl-loop for (source . target) in newlocs
               for response = (read-string (format "%sRefile to %s? "
                                                   (if destructive "Destructive " "")
                                                   target))
               when (s-equals? "y" response)
               if destructive
               do (f-move source target)
               else
               do
               (f-copy source (f-join (f-parent source)
                                      (format "_refiled_%s"
                                              (f-filename source))))
               (f-move source target)
            )

      ;; Update entry with new locations
      (cl-loop for (source . target) in newlocs
               with count = 1
               do
               (bibtex-set-field (format "file%s" (if (eq count 1) "" count)) target)
               (cl-incf count)
            )
      )
    )
  )

;;;###autoload (autoload 'librarian-biblio-refile-by-year "librarian--biblio" nil t)
(defun librarian--biblio-refile-by-year ()
  " Kill the current entry and insert it in the appropriate year's bibtex file
Then move the pdfs of the entry to the canonical location
"
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((year (bibtex-text-in-field "year"))
         (year-file (format "%s.bib" year))
         (response (when year (read-string (format "Refile to %s? " year-file))))
         (target (if (or (s-equals? "y" response) (string-empty-p response))
                     (f-join librarian--biblio-library-loc year-file)
                   (completing-read "Bibtex file: "
                                    (f-entries bib-path
                                               (lambda (f) (f-ext? f "bib"))))))
         )
    (unless (f-exists? target) (f-touch target))
    (librarian--biblio-refile-pdf current-prefix-arg)
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

;;;###autoload (autoload 'librarian--biblio-refile-to-unsourced "librarian--biblio" nil t)
(defun librarian--biblio-refile-to-unsourced ()
  " Kill the current entry and insert it in the appropriate year's bibtex file "
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((target librarian--biblio-unsourced-bib-file)
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

;;;###autoload (autoload 'librarian--biblio-refile-to-other-window "librarian--biblio" nil t)
(defun librarian--biblio-refile-to-other-window ()
  "Refile the entry under point to the other window"
  (interactive)
  (unless (save-selected-window (other-window 1) (eq major-mode 'bibtex-mode))
    (user-error "Other Window Is Not a Bibtex Buffer"))
  (save-excursion (librarian--biblio-edit-copy-entry))
  (save-selected-window (other-window 1)
                        (end-of-buffer)
                        (newline-and-indent)
                        (yank)
                        (recenter)
  )
  (evil-visual-state)
  (let ((sel (+evil:defun-txtobj)))
    (evil-visual-select (car sel) (cadr sel))
    )
  )

(defun librarian--biblio-font-lock-mod-hook ()
  (pushnew!
   bibtex-font-lock-keywords
   '(" title.+$" (0 '(:background "mediumpurple4")))
   '("\\(file\\).+?=" (1 '(:background "darkgoldenrod")))
   '("\\(tags\\).+?=.+$" (0 '(:background "darkseagreen")))
   )
  )

(provide 'librarian--biblio)
;;; librarian-bibliography.el ends here
