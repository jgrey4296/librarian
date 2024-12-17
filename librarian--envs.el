;;; librarian-envs.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;;
;;
;;
;;; Code:
;;-- end header

;;-- vars

(defvar librarian-envs-enter-hook nil "A general hook for when entering an environment")

(defvar librarian-envs-exit-hook nil "A general hook for exiting an environment")

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
(cl-defstruct (lenv-handler)
  " An environment handler.
describes the language it handles,
its hooks
how to describe itself in the modeline
"
  (id       nil     :type 'symbol           :read-only t)
  (lang     nil     :type 'str              :read-only t)
  ;; maybe: hooks list to add to {lang}-mode-hook?
  (setup    nil     :type 'lambda           :read-only t)
  (select   nil     :type 'lambda           :read-only t :documentation "fn to generate and select env name" )
  (start    nil     :type 'lambda           :read-only t)
  (stop     nil     :type 'lambda           :read-only t)
  (teardown nil     :type 'lambda           :read-only t)
  (modeline nil     :type (or 'lambda str)  :read-only t :documentation "added to global-mode-string")
  (cmds     nil     :type 'list             :read-only t :documentation "eg: install package, update...")
  (desc     nil     :type 'str              :read-only t :documentation "for reporting")
  )

(cl-defstruct (lenv-state)
  "The current envs state for a handler"
  (id      nil   :type 'symbol :documentation "the same as the handler")
  (status  nil   :type 'symbol :documentation "nil|setup|active")
  (loc     nil   :type 'librarian-envs-loc)
  (locked  nil   :type 'bool)
  (data    nil   :type 'list :documentation "a list for arbitrary data handlers can put")
  )

(cl-defstruct (lenv-loc)
  "Description of where environment data was found"
  (root       nil :type 'path :documentation "root of the project")
  (marker     nil :type 'path :documentation "relpath to the marker from the root")
  )
;;-- end structs

;;;###autoload
(defun librarian-envs-register (&rest args)
  " Register a new handler.
Either a librarian-envs-handler, or a plist to build one
"
  (let* ((new-handler (if (keywordp (car-safe args))
                          (apply #'make-lenv-handler args)
                        (car args)))
         (id (lenv-handler-id new-handler))
         )
    (if (gethash id librarian-envs--registered)
        (user-error "A Handler has already been registered with the name: %s" id)
      (puthash id new-handler librarian-envs--registered)
      )
    )
)

(defun lenv-clear-registry ()
  (clrhash lenv--registered)
  (clrhash lenv-active)
  )

;;;###autoload
(defun librarian-envs-start! (arg &rest ids)
  " Main access point for setting up environment.
Acts as a Dispatch to activate appropriate environment
and call the currently selected lsp/conda client entrypoint
handlers can be passed as 2nd+ args

pass a prefix arg to use ivy to manually select from registered handlers
"

  (interactive "P")
  (let* ((loc (lenv--init-loc))
         ;; Handlers: (list handler-id | (handler-id args)
         (specs (mapcar #'ensure-list (or ids (lenv--parse-marker loc))))
         states
         )
    (when arg
      (setq specs nil)
      (ivy-read "Available Handlers: "
                (hash-table-keys lenv--registered)
                :require-match t
                :action #'(lambda (x) (add-to-list 'specs x))
                )
      )
    (unless specs (user-error "No Handlers scheduled to run"))

    ;; wrap handlers in state with loc
    (setq states (cl-loop for vals in specs
                          collect
                          (funcall #'lenv--activate-handler
                                   (car vals)
                                   loc
                                   (cdr vals))
                          )
          )
    (prog1
        (cl-loop for state in states
                 for valid              = (and state (lenv-state-p state) (not (lenv-state-locked state)))
                 when valid for status  = (lenv-state-status state)
                 when valid for handler = (lenv--get-handler (lenv-state-id state))
                 when (and valid handler (eq status 'nil)) do
                 ;; run setup and set modeline
                 (--if-let (lenv-handler-setup handler)    (apply it (lenv-state-data state)))
                 (--if-let (lenv-handler-modeline handler) (add-to-list 'global-mode-string it))
                 (setf (lenv-state-status state) 'setup)
                 when (and valid handler) do
                 ;; run start
                 (--if-let (lenv-handler-start handler)    (apply it (lenv-state-data state)))
                 (setf (lenv-state-status state) 'active)
                 ;; collect them to return
                 when (and valid (eq (lenv-state-status state) 'active)) collect state
                 )
      ;; run enter hooks
      (run-hooks 'lenv-enter-hook)
      )
    )
  )

;;;###autoload
(defun librarian-envs-stop! (arg &rest ids)
  (interactive)
  (let ((loc (lenv--init-loc))
        (specs (or ids
                   (mapcar #'car (lenv--parse-marker loc))))
        states
        )
    (when arg
      (setq specs nil)
      (ivy-read "Available Handlers: "
                (hash-table-keys lenv-active)
                :require-match t
                :action #'(lambda (x) (add-to-list 'specs x))
                )
      )
    (setq states (cl-loop for id in specs
                          collect
                          (lenv--get-state id)
                          ))
    (cl-loop for state in states
             for valid = (and state (lenv-state-p state) (not (lenv-state-locked state)))
             when valid for status = (lenv-state-status state)
             ;; or Deactivate
             when (eq status 'active) do
             (let ((handler (lenv--get-handler (lenv-state-id state))))
               (--if-let (lenv-handler-stop handler)     (apply it (lenv-state-data state))))
             (setf (lenv-state-status state) 'setup)
             ;; Teardown
             when (eq status 'setup) do
             (let ((handler (lenv--get-handler (lenv-state-id state))))
               (--if-let (lenv-handler-teardown handler) (apply it (lenv-state-data state))))
             (setf (lenv-state-status state) nil)
             when valid collect state
             )
    )
  )

;;;###autoload
(defun librarian-envs-toggle-lock! (&rest rest)
  "Toggle whether the environment can be changed or not"
  (interactive)
  (let ((ids rest))
    (unless ids (ivy-read "Available Handlers: "
                          (mapcar #'lenv-state-id lenv-active)
                          :require-match t
                          :action #'(lambda (x) (add-to-list 'handlers x))
                          ))
    (cl-loop for name in ids
             do
             (setf (lenv-state-locked (lenv--get-state name))
                   (not (lenv-state-locked (lenv--get-state name)))
                   )
             )
    )
  )

;;;###autoload
(defun librarian-envs-report! ()
  (interactive)
  (with-temp-buffer-window "*Envs Report*" 'display-buffer nil
    ;; Temp window of:
    ;; id registered active locked
  )
)

(defun lenv--init-loc (&optional start)
  " return a envs-loc "
  (let* ((root (projectile-project-root start)))
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
  (let ((marker (lenv--expand-marker loc))
        data
        )
    (unless (f-exists? marker) (error "Marker Doesn't Exist" marker))
    (with-temp-buffer
      (insert-file-contents marker)
      (goto-char (point-min))
      (while (< (point) (point-max))
        ;; go through each line
        ;; ignore comments and blank lines
        (cond ((looking-at "^#") nil)
              ((looking-at "^$") nil)
              (t (push (s-split " +" (buffer-substring (line-beginning-position)
                                                       (line-end-position)) t)
                       data)
                 )
              )
        (forward-line)
        )
      )
    (reverse data)
    )
  )

(defun lenv--expand-marker (loc)
  (f-join (lenv-loc-root loc) (lenv-loc-marker loc))
  )

(defun lenv--get-handler (id)
  "Get a handler by its id.
handles both strings and symbols
"
  (pcase id
    ((pred lenv-handler-p) id)
    ((pred symbolp) (gethash id lenv--registered))
    ((pred stringp) (gethash (intern id) lenv--registered))
    (x (user-error "Tried to get a handler with a bad type: %s" id))
    )
 )

(defun lenv--get-state (id)
  (pcase id
    ((pred lenv-state-p) id)
    ((pred symbolp) (gethash id lenv-active))
    ((pred stringp) (gethash (intern id) lenv-active))
    (x (user-error "Tried to get a state with a bad type: %s" id))
    )
  )

(defun lenv--activate-handler (id loc &optional data)
  " wrap a handler and loc into a state and add it to lenv-active.
then return the state
"
  (unless (lenv--get-handler id) (error "Tried to activate a non-registered handler" id))
  (unless (lenv-loc-p loc) (error "tried to activate a handler with an invalid loc" loc))
  (if (lenv--get-state id)
      (lenv--get-state id)
    (let* ((handler (lenv--get-handler id))
          (state (make-lenv-state
                  :id (lenv-handler-id handler)
                  :status nil
                  :loc loc
                  :locked nil
                  :data data)))
      (puthash (lenv-state-id state) state lenv-active)
      state
      )
    )
  )

(defalias 'librarian-envs-handler-p #'librarian--envs-handler-p)
(provide 'librarian--envs)

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
;; ("lenv-" . "librarian--envs-")
;; ("make-lenv-" . "make-librarian--envs-")
;; )
;; End:
