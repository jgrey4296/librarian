;;; librarian-envs.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

;;-- vars

(defvar librarian-envs-enter-hook nil "A general hook for when entering an environment. called with the state")

(defvar librarian-envs-exit-hook nil "A general hook for exiting an environment, called with the state")

(defvar librarian-envs--registered (make-hash-table) "Mapping of env names to their structs")

;;;###autoload
(defvar librarian-envs-marker ".lenvs"
  "Marker files librarian-envs-handling will look for to determine the librarian-environment"
  )

(defconst librarian-envs-handling-process-name
  "envs-handling-proc"
  "The name of the process librarian-envs-handling uses to run external actions")

(defconst librarian-envs-handling-buffer-name
  "*envs-handling*"
  "The buffer librarian-envs-handling puts external output into"
  )

(defconst librarian-envs-active (make-hash-table) "maps id -> activated handlers")

;;-- end vars

;;-- structs
(cl-defstruct (librarian-envs-handler)
  " An environment handler.
describes the language it handles,
its hooks
how to describe itself in the modeline
"
  (id       nil     :type 'symbol           :read-only t)
  (lang     nil     :type 'str              :read-only t)
  ;; maybe: hooks list to add to {lang}-mode-hook?
  (setup    nil     :type 'lambda           :read-only t)
  (select   nil     :type 'lambda           :read-only t :documentation "fn to generate a list of available env names" )
  (start    nil     :type 'lambda           :read-only t)
  (stop     nil     :type 'lambda           :read-only t)
  (teardown nil     :type 'lambda           :read-only t)
  (modeline nil     :type (or 'lambda str)  :read-only t :documentation "added to global-mode-string")
  (cmds     nil     :type 'list             :read-only t :documentation "eg: install package, update...")
  (desc     nil     :type 'str              :read-only t :documentation "for reporting")
  )

(cl-defstruct (librarian-envs-state)
  "The current envs state for a handler"
  (id      nil   :type 'symbol)
  (type    nil   :type 'symbol)
  (status  nil   :type 'symbol :documentation "nil|setup|active")
  (handler nil   :type 'librarian-envs-handler)
  (loc     nil   :type 'librarian-envs-loc)
  (locked  nil   :type 'bool)
  (data    nil   :type 'list :documentation "a list for arbitrary data handlers can put")
  )

(cl-defstruct (librarian-envs-loc)
  "Description of where environment data was found"
  (root       nil :type 'path :documentation "root of the project")
  (marker     nil :type 'path :documentation "relpath to the marker from the root")
  )
;;-- end structs

;;;###autoload
(defun lenv-register (&rest args)
  " Register a new handler.
Either a librarian-envs-handler, or a plist to build one
"
  (let* ((new-handler (if (keywordp (car-safe args))
                          (apply #'make-lenv-handler args)
                        (car args)))
         (id (lenv-handler-id new-handler))
         )
    (if (gethash id lenv--registered)
        (user-error "A Handler has already been registered with the name: %s" id)
      (puthash id new-handler lenv--registered)
      )
    )
)

(defun lenv-clear-registry ()
  (clrhash lenv--registered)
  )

;;;###autoload
(defun lenv-start! (arg)
  " Main access point for setting up environment.
    Acts as a Dispatch to activate appropriate environment
    and call the currently selected lsp/conda client entrypoint"
  (interactive "P")
  (let* ((loc (lenv--init-loc))
         ;; Handlers: (list handler-id | (handler-id args)
         (handlers nil)
         )
    ;; parse-venv
    (unless arg (setq handlers (lenv--parse-marker loc)))
    ;; select handlers to activate if venv doesn't specify
    (unless handlers
      (ivy-read "Available Handlers: "
                (hash-table-keys lenv--registered)
                :require-match t
                :action #'(lambda (x) (add-to-list 'handlers x))
                )
      )
    (unless handlers
      (user-error "No Handlers scheduled to run"))

    ;; wrap handlers in state with loc
    ;; filter locked
    ;; run setup funcs
    ;; run enter funcs
    ;; add modeline funcs
    ;; run enter hooks
    )
  )

;;;###autoload
(defun lenv-stop! ()
  (interactive)
  (let (handlers)
    (ivy-read "Available Handlers: "
              (hash-table-keys lenv--aactive)
              :require-match t
              :action #'(lambda (x) (add-to-list 'handlers x))
              )
    ;; filter locked
    ;; call exit-funcs
    ;; call exit-hooks
    ;; call teardown funcs
    ;; remove modeline funcs
    )
  )

;;;###autoload
(defun lenv-toggle-lock! ()
  "Toggle whether the environment can be changed or not"
  (interactive)
  (let (handlers)
    (ivy-read "Available Handlers: "
              (mapcar #'lenv-state-id lenv-active)
              :require-match t
              :action #'(lambda (x) (add-to-list 'handlers x))
              )
    (cl-loop for name in handlers
             do
             (setf (lenv-state-locked (gethash name lenv-active))
                   (not (lenv-state-locked (gethash name lenv-active)))
                   )
             )
    )
  )

;;;###autoload
(defun lenv-report! ()
  (interactive)
  (with-temp-buffer-window "*Envs Report*" 'display-buffer nil
    ;; Temp window of:
    ;; id registered active locked
  )
)

(defun lenv--init-loc (&optional start)
  " return a envs-loc "
  (let* ((root (projectile-project-root)))
    (make-lenv-loc :root root
                             :marker (when (f-exists? (f-join root lenv-marker)) lenv-marker)
                             )
    )
  )

(defun lenv--parse-marker (loc)
  "Parse a marker file for handler name data

return: (list marker-id | (marker-id args))
"
  (unless (lenv-loc-p loc)
    (error "Not passed a librarian-envs-loc" loc))
  (let ((marker (f-join (lenv-loc-root loc)
                        (lenv-loc-marker loc)))
        data
        )
    (unless (f-exists? marker) (error "Marker Doesn't Exist" marker))
    (with-temp-buffer
      (insert-file-contents marker)
      (goto-char (point-min))
      ;; go through each line
      ;; get {handler} {args*}
      )
    )
  )

(provide 'librarian-envs)

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    December 14, 2024
;; Modified:   December 14, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; librarian-envs.el ends here

;; Local Variables:
;; read-symbol-shorthands: (
;; ("lenv-" . "librarian-envs-")
;; ("make-lenv-" . "make-librarian-envs-")
;; )
;; End:
