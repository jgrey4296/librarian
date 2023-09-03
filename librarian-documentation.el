;; -*- mode:emacs-lisp; -*- lexical-bindings: t; -*-
;;

(defun librarian-definition (identifier &optional arg)
  "Jump to the definition of IDENTIFIER (defaults to the symbol at point).

Each function in `librarian-definition-functions' is tried until one changes the
point or current buffer. Falls back to dumb-jump, naive
ripgrep/the_silver_searcher text search, then `evil-goto-definition' if
evil-mode is active."
  (interactive (list (librarian-get) current-prefix-arg))
  (cond ((null identifier) (user-error "Nothing under point"))
        ((librarian--jump-to :definition identifier nil arg))
        ((user-error "Couldn't find the definition of %S" (substring-no-properties identifier)))))

(defun librarian-declaration (identifier &optional arg)
  (interactive (list (librarian-get) current-prefix-arg))
  (cond ((null identifier) (user-error "Nothing under point"))
        ((librarian--jump-to :declaration identifier nil arg))
        ((user-error "Couldn't find the declaration of %S" (substring-no-properties identifier))))
  )

(defun librarian-implementations (identifier &optional arg)
  "Jump to the implementations of IDENTIFIER (defaults to the symbol at point).

Each function in `librarian-implementations-functions' is tried until one changes
the point or current buffer."
  (interactive (list (librarian-get) current-prefix-arg))
  (cond ((null identifier) (user-error "Nothing under point"))
        ((librarian--jump-to :implementations identifier nil arg))
        ((user-error "Couldn't find the implementations of %S" (substring-no-properties identifier)))))

(defun librarian-type-definition (identifier &optional arg)
  "Jump to the type definition of IDENTIFIER (defaults to the symbol at point).

Each function in `librarian-type-definition-functions' is tried until one changes
the point or current buffer."
  (interactive (list (librarian-get) current-prefix-arg))
  (cond ((null identifier) (user-error "Nothing under point"))
        ((librarian--jump-to :type-definition identifier nil arg))
        ((user-error "Couldn't find the definition of %S" (substring-no-properties identifier)))))

(defun librarian-references (identifier &optional arg)
  "Show a list of usages of IDENTIFIER (defaults to the symbol at point)

Tries each function in `librarian-references-functions' until one changes the
point and/or current buffer. Falls back to a naive ripgrep/the_silver_searcher
search otherwise."
  (interactive (list (librarian-get) current-prefix-arg))
  (cond ((null identifier) (user-error "Nothing under point"))
        ((librarian--jump-to :references identifier nil arg))
        ((user-error "Couldn't find references of %S" (substring-no-properties identifier)))))

(defun librarian-documentation (identifier &optional arg)
  "Show documentation for IDENTIFIER (defaults to symbol at point or selection.

First attempts the :documentation handler specified with `set-lookup-handlers!'
for the current mode/buffer (if any), then falls back to the backends in
`librarian-documentation-functions'."
  (interactive (list (librarian-get) current-prefix-arg))
  (librarian--run-handler :documentation identifier)
  )

(defun librarian-assignments (identifier &optional arg)
  (interactive (list (librarian-get) current-prefix-arg))
  (cond ((librarian--jump-to :assignments identifier #'pop-to-buffer arg))
        ((user-error "Couldn't find assignments for %S" (substring-no-properties identifier)))))

(defun librarian-choose (identifier &optional arg)
  (interactive (list (librarian-get) current-prefix-arg))
  (let ((handler-sym (intern (ivy-read "Handler Option: " librarian-valid-keywords)))
        )
    (cond ((librarian--jump-to handler-sym identifier #'pop-to-buffer arg))
          ((user-error "Failed to use %s for %S" handler-sym (substring-no-properties identifier))))
    )
  )

(defun librarian--run-handler (prop identifier)
  (let* ((handlers (pcase prop
                     (:definition      librarian-definition-functions)
                     (:declaration     librarian-declaration-functions)
                     (:implementations librarian-implementations-functions)
                     (:type-definition librarian-type-definition-functions)
                     (:references      librarian-references-functions)
                     (:documentation   librarian-documentation-functions)
                     (:file            librarian-file-functions)
                     (:assignments     librarian-assignments-functions)
                     (_ (user-error "Unrecognized lookup prop" prop))
                     ))
         selected-handler
         )
    ;; Select just one handler:
    (pcase handlers
      ('nil (user-error "No Handler Found for: %s" prop))
      ((and (pred listp) (pred (lambda (x) (< 1 (length x)))))
       ;; (setq selected-handler (intern-soft (ivy-read "Select a handler: " handlers :require-match t)))
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

(provide 'librarian-documentation)
