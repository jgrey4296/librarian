;; -*- mode:emacs-lisp; -*- lexical-bindings: t; -*-


(eval-when-compile
  (require 'dash)
  (require 'better-jumper)
  (require 'librarian--util)
  )

(defvar librarian--doc-valid-keywords '(
                             :definition
                             :declaration
                             :implementations
                             :type-definition
                             :references
                             :documentation
                             :assignments
                             )
  "Valid Types of Lookup commands that can be registered")

(defvar librarian--doc-default-handlers
  (list
    :definition
    (list
     #'librarian-backend--xref-definitions
     )
    :references
    (list
     #'librarian-backend--xref-references
     #'librarian-backend--project-search
     )
    )
  "plist of default handlers for librarian--doc-handlers-plist"
  )

(defvar-local librarian--doc-handlers-plist nil
  "Local plist of lookup handlers for documentation
Valid keys are librarian--doc-valid-keywords
"
  )

(defun librarian--doc-declare-type (type)
  "Add a new type of lookup handler"
  (cl-assert (keywordp type))
  (push type librarian--doc-valid-keywords)
  )

(defun librarian--doc-valid-type-p (type)
  "Check the given kwd is a valid handler type"
  (and (keywordp type) (-contains-p librarian--doc-valid-keywords type))
  )

(defun librarian--doc-update-handler (prop fns)
  " Add handlers to a specific handler type, buffer locally "
  (cl-assert (listp fns))
  (cl-assert (librarian--doc-valid-type-p prop))
  (cl-assert (plistp librarian--doc-handlers-plist))
  (let* ((local (buffer-local-value 'librarian--doc-handlers-plist (current-buffer)))
         (orig (plist-get local prop))
         (merged (cl-remove-duplicates (append fns orig) :from-end t))
         )
      (setq-local librarian--doc-handlers-plist
                  (plist-put local
                             prop
                             merged
                             ))
      )
  )

(defun librarian--doc-init-defaults ()
  (cl-assert (plistp librarian--doc-default-handlers))
  (setq-default librarian--doc-handlers-plist (seq-copy librarian--doc-default-handlers))
  )

(defun librarian--doc--run-handler (prop identifier)
  " for a prop kwd in librarian--doc-handlers-plist, get the handlers
and call the first one with identifier

TODO: keep trying handlers till one succeeds
"
  (cl-assert (librarian--doc-valid-type-p prop))
  (let* ((handlers (plist-get librarian--doc-handlers-plist prop))
         (selected-handler (pcase handlers
                             ('nil (user-error "No Handler Found for: %s" prop))
                             ((and x (pred listp)) (car-safe handlers))
                             ((and x (pred functionp)) x)
                             (x (user-error "Handler is not a function: %s" x))
                             ))
         )
    ;; Run the Handler:
    (pcase selected-handler
      ((and x (pred commandp))
       (call-interactively selected-handler))
      ((and x (pred functionp))
       (funcall x identifier))
      (x (user-error "Handler isn't a function: %s" x))
      )
    )
  )

(defun librarian--doc--go (type id &optional overridefn)
  " uses a handler type to jump to information "
  (pcase (list type id)
    ((or `(,_ "") `(,_ nil))
     (user-error "Nothing to pass to handler"))
    ((and `(,x ,_) (guard (not (librarian--doc-valid-type-p x))))
     (user-error "Not a valid lookup type: %s, available: %s : %s" x librarian--doc-valid-keywords))
    (x
     (librarian--doc--jump-to type id overridefn))
    )
  )

(defun librarian--doc--jump-to (prop identifier &optional display-fn)
  " access point to jump to markers, files, etc "
  (let ((origin (point-marker))
        (result (librarian--doc--run-handler prop identifier))
        )
    ;; Deal with result
    (unwind-protect
        ;; When a jump is real,
        (when (cond ((null result) nil)
                    ((markerp result)
                     (funcall (or display-fn #'switch-to-buffer) (marker-buffer result))
                     (goto-char result)
                     result)
                    (result))
          ;; record it with better-jumper
          (with-current-buffer (marker-buffer origin)
            (better-jumper-set-jump (marker-position origin)))
          result)
      ;; unwind if something fails
      (set-marker origin nil))
    )
  )

;;;###autoload
(defun librarian-definition (identifier &optional arg)
  "Jump to the definition of IDENTIFIER (defaults to the symbol at point).

Each function in `librarian--doc-definition-functions' is tried until one changes the
point or current buffer. Falls back to dumb-jump, naive
ripgrep/the_silver_searcher text search, then `evil-goto-definition' if
evil-mode is active."
  (interactive (list (librarian--util-get) current-prefix-arg))
  (librarian--doc--go :definition identifier)
  )

;;;###autoload
(defun librarian-declaration (identifier &optional arg)
  (interactive (list (librarian--util-get) current-prefix-arg))
  (librarian--doc--go :declaration identifier)
  )

;;;###autoload
(defun librarian-implementations (identifier &optional arg)
  "Jump to the implementations of IDENTIFIER (defaults to the symbol at point).

Each function in `librarian--doc-implementations-functions' is tried until one changes
the point or current buffer."
  (interactive (list (librarian--util-get) current-prefix-arg))
  (librarian--doc--go :implementations identifier)
  )

;;;###autoload
(defun librarian-type-definition (identifier &optional arg)
  "Jump to the type definition of IDENTIFIER (defaults to the symbol at point).

Each function in `librarian--doc-type-definition-functions' is tried until one changes
the point or current buffer."
  (interactive (list (librarian--util-get) current-prefix-arg))
  (librarian--doc--go :type-definition identifier)
  )

;;;###autoload
(defun librarian-references (identifier &optional arg)
  "Show a list of usages of IDENTIFIER (defaults to the symbol at point)

Tries each function in `librarian--doc-references-functions' until one changes the
point and/or current buffer. Falls back to a naive ripgrep/the_silver_searcher
search otherwise."
  (interactive (list (librarian--util-get) current-prefix-arg))
  (librarian--doc--go :references identifier)
  )

;;;###autoload
(defun librarian-documentation (identifier &optional arg)
  "Show documentation for IDENTIFIER (defaults to symbol at point or selection.

First attempts the :documentation handler specified with `set-lookup-handlers!'
for the current mode/buffer (if any), then falls back to the backends in
`librarian--doc-functions'."
  (interactive (list (librarian--util-get) current-prefix-arg))
  (let ((curr (selected-window)))
    (librarian--doc--go :documentation identifier)
    (select-window curr)
    )
  )

;;;###autoload
(defun librarian-assignments (identifier &optional arg)
  (interactive (list (librarian--util-get) current-prefix-arg))
  (librarian--doc--go :assignments identifier #'pop-to-buffer)
  )

;;;###autoload
(defun librarian-choose (identifier &optional arg)
  (interactive (list (librarian--util-get) current-prefix-arg))
  (let ((handler-sym (intern (ivy-read "Handler Option: " librarian--doc-valid-keywords))))
    (librarian--doc--go handler-sym identifier #'pop-to-buffer)
    )
  )

;;;###autoload
(defun librarian-file (&optional path)
  "Figure out PATH from whatever is at point and open it.

Each function in `librarian-file-functions' is tried until one changes the point
or the current buffer.

Otherwise, falls back on `find-file-at-point'."
  (interactive)
  (cond ((and path
              buffer-file-name
              (file-equal-p path buffer-file-name)
              (user-error "Already here")))
        ((librarian--doc--jump-to :file path))
        ((user-error "Couldn't find any files here")))
  )

(librarian--doc-init-defaults)
(provide 'librarian--doc)
;;; librarian--doc.el ends here
