;; -*- mode:emacs-lisp; -*- lexical-bindings: t; -*-
;;

(eval-when-compile
  (require 'dash)
  (require 'better-jumper)
  (require 'librarian--util)
  )

(defvar lid-valid-keywords '(
                             :definition
                             :declaration
                             :implementations
                             :type-definition
                             :references
                             :documentation
                             :assignments
                             )
  "Valid Types of Lookup commands that can be registered")

(defvar lid-default-handlers
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
  "plist of default handlers for lid-handlers-plist"
  )

(defvar-local lid-handlers-plist nil
  "Local plist of lookup handlers for documentation
Valid keys are lid-valid-keywords
"
  )

(defun lid-declare-type (type)
  "Add a new type of lookup handler"
  (cl-assert (keywordp type))
  (push type lid-valid-keywords)
  )

(defun lid-valid-type-p (type)
  "Check the given kwd is a valid handler type"
  (and (keywordp type) (-contains-p lid-valid-keywords type))
  )

(defun lid-update-handler (prop fns)
  " Add handlers to a specific handler type, buffer locally "
  (cl-assert (listp fns))
  (cl-assert (lid-valid-type-p prop))
  (cl-assert (plistp lid-handlers-plist))
  ;; (message "Updating: %s : %s : %s" prop fns (type-of fns))
  (let* ((orig (plist-get lid-handlers-plist prop))
         (merged (cl-remove-duplicates (append fns orig)))
         )
    (setq-local lid-handlers-plist
                (plist-put lid-handlers-plist
                           prop
                           merged
                           ))
    )
  )

(defun lid-init-defaults ()
  (cl-assert (plistp lid-default-handlers))
  (setq-default lid-handlers-plist (seq-copy lid-default-handlers))
  )

(defun lid--run-handler (prop identifier)
  " for a prop kwd in lid-handlers-plist, get the handlers
and call the first one with identifier

TODO: keep trying handlers till one succeeds
"
  (cl-assert (lid-valid-type-p prop))
  (let* ((handlers (plist-get lid-handlers-plist prop))
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

(defun lid--go (type id &optional overridefn)
  " uses a handler type to jump to information "
  (pcase (list type id)
    ((or `(,_ "") `(,_ nil))
     (user-error "Nothing to pass to handler"))
    ((and `(,x ,_) (guard (not (lid-valid-type-p x))))
     (user-error "Not a valid lookup type: %s, available: %s : %s" x lid-valid-keywords))
    (x
     (lid--jump-to type id overridefn))
    )
  )

(defun lid--jump-to (prop identifier &optional display-fn)
  " access point to jump to markers, files, etc "
  (let ((origin (point-marker))
        (result (lid--run-handler prop identifier))
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

Each function in `lid-definition-functions' is tried until one changes the
point or current buffer. Falls back to dumb-jump, naive
ripgrep/the_silver_searcher text search, then `evil-goto-definition' if
evil-mode is active."
  (interactive (list (librarian--util-get) current-prefix-arg))
  (lid--go :definition identifier)
  )

;;;###autoload
(defun librarian-declaration (identifier &optional arg)
  (interactive (list (librarian--util-get) current-prefix-arg))
  (lid--go :declaration identifier)
  )

;;;###autoload
(defun librarian-implementations (identifier &optional arg)
  "Jump to the implementations of IDENTIFIER (defaults to the symbol at point).

Each function in `lid-implementations-functions' is tried until one changes
the point or current buffer."
  (interactive (list (librarian--util-get) current-prefix-arg))
  (lid--go :implementations identifier)
  )

;;;###autoload
(defun librarian-type-definition (identifier &optional arg)
  "Jump to the type definition of IDENTIFIER (defaults to the symbol at point).

Each function in `lid-type-definition-functions' is tried until one changes
the point or current buffer."
  (interactive (list (librarian--util-get) current-prefix-arg))
  (lid--go :type-definition identifier)
  )

;;;###autoload
(defun librarian-references (identifier &optional arg)
  "Show a list of usages of IDENTIFIER (defaults to the symbol at point)

Tries each function in `lid-references-functions' until one changes the
point and/or current buffer. Falls back to a naive ripgrep/the_silver_searcher
search otherwise."
  (interactive (list (librarian--util-get) current-prefix-arg))
  (lid--go :references identifier)
  )

;;;###autoload
(defun librarian-documentation (identifier &optional arg)
  "Show documentation for IDENTIFIER (defaults to symbol at point or selection.

First attempts the :documentation handler specified with `set-lookup-handlers!'
for the current mode/buffer (if any), then falls back to the backends in
`lid-functions'."
  (interactive (list (librarian--util-get) current-prefix-arg))
  (let ((curr (selected-window)))
    (lid--go :documentation identifier)
    (select-window curr)
    )
  )

;;;###autoload
(defun librarian-assignments (identifier &optional arg)
  (interactive (list (librarian--util-get) current-prefix-arg))
  (lid--go :assignments identifier #'pop-to-buffer)
  )

;;;###autoload
(defun librarian-choose (identifier &optional arg)
  (interactive (list (librarian--util-get) current-prefix-arg))
  (let ((handler-sym (intern (ivy-read "Handler Option: " lid-valid-keywords))))
    (lid--go handler-sym identifier #'pop-to-buffer)
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
        ((lid--jump-to :file path))
        ((user-error "Couldn't find any files here")))
  )

(provide 'librarian--doc)
;; Local Variables:
;; read-symbol-shorthands: (
;; ("lid-" . "librarian--doc-")
;; )
;; End:
