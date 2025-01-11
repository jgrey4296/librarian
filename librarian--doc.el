;; -*- mode:emacs-lisp; -*- lexical-bindings: t; -*-
;;

(eval-when-compile
  (require 'dash)
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

(defvar lid-definition-defaults '(
                                  librarian-backend--words-dictionary
                                  librarian-backend--xref-definitions
                                  librarian-backend--dumb-jump
                                  librarian-backend--project-search
                                  librarian-backend--evil-goto-def
                                  )
  "The default handler list for looking up definitions "
  )

(defvar lid-references-defaults '(
                                  librarian-backend--words-thesaurus
                                  librarian-backend--xref-references
                                  librarian-backend--project-search
                                  )
  "The default list of handlers for looking up references"
  )

(defvar lid-declaration-defaults nil
  "the default list of handlers for looking up declarations")

(defvar lid-implementations-defaults nil
  "the default list of handlers for looking up implementations")

(defvar lid-type-definition-defaults nil
  "the default list of handlers for looking up definitions")

(defun lid-declare-type (type)
  "TODO : declare a new keyword for lookup instructions"
  )

(defun lid-valid-type-p (type)
  (and (keywordp type) (-contains-p lid-valid-keywords type))
  )

(defun lid-init-defaults ()
  (setq-default lid-assignments-functions     nil
                lid-declaration-functions     nil
                lid-definition-functions      nil
                lid-documentation-functions   nil
                lid-file-functions            nil
                lid-implementations-functions nil
                lid-references-functions      nil
                lid-type-definition-functions nil
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

(defun lid--run-handler (prop identifier)
  (let* ((handlers (pcase prop
                     (:assignments     lid-assignments-functions)
                     (:declaration     lid-declaration-functions)
                     (:definition      lid-definition-functions)
                     (:documentation   lid-documentation-functions)
                     (:file            lid-file-functions)
                     (:implementations lid-implementations-functions)
                     (:references      lid-references-functions)
                     (:type-definition lid-type-definition-functions)
                     (_ (user-error "Unrecognized lookup prop" prop))
                     ))
         selected-handler
         )
    ;; Select just one handler:
    (pcase handlers
      ('nil (user-error "No Handler Found for: %s" prop))
      ((and (pred listp) (pred (lambda (x) (< 1 (length x)))))
       (setq selected-handler (car handlers))
       )
      ((pred listp)
       (setq selected-handler (car handlers)))
      (_ (setq selected-handler handlers))
      )
    ;; Run the Handler:
    (if (commandp selected-handler)
        (call-interactively selected-handler)
      (funcall selected-handler identifier))
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
  (lid--go :documentation identifier)
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
