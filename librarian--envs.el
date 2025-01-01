;;; librarian--envs.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;;
;;
;;
;;; Code:
;;-- end header

;;-- vars
(eval-when-compile
  (require 'cl-lib)
  (require 'dash)
  (require 'f)
  (require 's)
  (require 'macro-tools--util)
  )

(defvar lenv-enter-hook nil "A general hook for when entering an environment")

(defvar lenv-exit-hook nil "A general hook for exiting an environment")

(defvar lenv-registered (make-hash-table) "Mapping of env names to their structs")

(defvar lenv-marker ".lenvs"
  "Marker files librarian will look for to determine the librarian-environment"
  )

(defconst lenv-process-name
  "envs-handling-proc"
  "The name of the process librarian uses to run external actions")

(defconst lenv-buffer-name
  "*envs-handling*"
  "The buffer librarian puts report output into"
  )

(defvar lenv-active (make-hash-table) "maps id -> activated handlers")

;;-- end vars

;;-- structs
;;;###autoload (defalias 'librarian-envs-handler-p #'librarian--envs-handler-p)
;;;###autoload (autoload 'librarian-envs-handler-p "librarian--envs")

(cl-defstruct (lenv-handler)
  " An environment handler.
describes the language it handles,
its callbacks,
and how to describe itself in the modeline

Callbacks (setup, start, stop, teardown) are of the form:
(lambda (state &rest rest) ...),
where rest are the data values  read from the relevant line in a .lenv file
"
  (id       nil     :type 'symbol           :read-only t)
  (lang     nil     :type 'str              :read-only t)
  ;; maybe: hooks list to add to {lang}-mode-hook?
  (setup    nil     :type 'lambda           :read-only t)
  (select   nil     :type 'lambda           :read-only t :documentation "fn to generate and select env name" )
  (start    nil     :type 'lambda           :read-only t)
  (stop     nil     :type 'lambda           :read-only t)
  (teardown nil     :type 'lambda           :read-only t)
  (modeline nil     :type 'lambda           :read-only t :documentation "added to global-mode-string. eval'd with the state on entry")
  (cmds     nil     :type 'list             :read-only t :documentation "eg: install package, update...")
  (desc     nil     :type 'str              :read-only t :documentation "for reporting")
  )

(cl-defstruct (lenv-state)
  "The current envs state for a handler"
  (id      nil   :type 'symbol :documentation "the same as the handler")
  (status  nil   :type 'symbol :documentation "nil|setup|active")
  (loc     nil   :type 'lenv-loc)
  (locked  nil   :type 'bool)
  (data    nil   :type 'list :documentation "a list for arbitrary data handlers can put")
  (modeline nil  :type 'string)
  )

(cl-defstruct (lenv-loc)
  "Description of where environment data was found"
  (root       nil :type 'path :documentation "root of the project")
  (marker     nil :type 'path :documentation "relpath to the marker from the root")
  )
;;-- end structs

(defun lenv-report-message (direction loc)
  "A Single message to report on the status of the environment at the end of lenv-start/stop"
  (when (not (hash-table-empty-p lenv-active))
    (message "Env %s : %s\n%s"
             direction
             (lenv-loc-root loc)
             (string-join (cl-loop for state being the hash-values in lenv-active
                                   for id     = (lenv-state-id state)
                                   for locked = (lenv-state-locked state)
                                   for status = (lenv-state-status state)
                                   collect
                                   (format "- %s%s : %s"
                                           (if locked "*" "")
                                           id
                                           status))
                          "\n"))
    )
  )

;;;###autoload (defalias 'librarian-envs-register! #'librarian--envs-register)
;;;###autoload (autoload 'librarian-envs-register! "librarian--envs")
(defun lenv-register (id &rest args)
  " Register a new handler.
Either a librarian--envs-handler, or a plist to build one
"
  (let* ((new-handler (if (lenv-handler-p id)
                          id
                        (lenv-macro-aware-build-handler id args)))
         (id (lenv-handler-id new-handler))
         )
    (if (gethash id lenv-registered)
        (message "A Handler has already been registered with the name: %s" id)
      (puthash id new-handler lenv-registered)
      (message "Registered Env Handler: %s" id)
      )
    )
)

(defun lenv-macro-aware-build-handler (id args)
  "Build a handler, ensuring the callbacks are actually functions"
  (cl-assert (plistp args) t "Should be a plist")
  (setq args (plist-put args :setup    (upfun! (plist-get args :setup))))
  (setq args (plist-put args :start    (upfun! (plist-get args :start))))
  (setq args (plist-put args :stop     (upfun! (plist-get args :stop))))
  (setq args (plist-put args :teardown (upfun! (plist-get args :teardown))))
  (setq args (plist-put args :modeline (upfun! (plist-get args :modeline))))
  (apply #'make-lenv-handler :id id args)
  )


;;;###autoload (defalias 'librarian-envs-clear! #'librarian--envs-clear-registry)
;;;###autoload (autoload 'librarian-envs-clear! "librarian--envs")
(defun lenv-clear-registry (&optional force)
  (interactive)
  (if (not (or force (hash-table-empty-p lenv-active)))
      (message "There are active environments, deactivate them before clearing")
    (message "Clearing Registered Environment Handlers")
    (clrhash lenv-registered)
    (clrhash lenv-active)
    )
  )

(defun lenv-init-loc (&optional start)
  " return an envs-loc "
  (let* ((root (or (projectile-project-root start) default-directory)))
    (make-lenv-loc :root root
                   :marker (when (f-exists? (f-join root lenv-marker)) lenv-marker)
                   )
    )
  )

(defun lenv-parse-marker (loc) ;; -> list
  "Parse a marker file for handler name data

return: (list marker-id | (marker-id args))
"
  (unless (lenv-loc-p loc)
    (error "Not passed a lenv-loc" loc))
  (let ((marker (lenv-expand-marker loc))
        data
        )
    (if (not (and marker (f-exists? marker)))
        (progn (message "Marker Doesn't Exist" marker) nil)
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
  )

(defun lenv-expand-marker (loc) ;; -> maybe[path]
  (-when-let* ((root (lenv-loc-root loc))
               (marker (lenv-loc-marker loc))
               (joined (f-join (lenv-loc-root loc) (lenv-loc-marker loc)))
               (exists (f-exists? joined))
               )
    joined
    )
  )

(defun lenv-get-handler (id)
  "Get a handler by its id.
handles both strings and symbols
"
  (pcase id
    ((pred lenv-handler-p) id)
    ((pred symbolp) (gethash id lenv-registered))
    ((pred stringp) (gethash (intern id) lenv-registered))
    (x (user-error "Tried to get a handler with a bad type: %s" id))
    )
 )

(defun lenv-get-state (id)
  (pcase id
    ((pred lenv-state-p) id)
    ((pred symbolp) (gethash id lenv-active))
    ((pred stringp) (gethash (intern id) lenv-active))
    (x (user-error "Tried to get a state with a bad type: %s" id))
    )
  )

(defun lenv-activate-handler (id loc &optional data)
  " wrap a handler and loc into a state and add it to lenv-active.
then return the state
"
  (unless (lenv-get-handler id) (error "Tried to activate a non-registered handler" id))
  (unless (lenv-loc-p loc) (error "tried to activate a handler with an invalid loc" loc))
  (if (lenv-get-state id)
      (lenv-get-state id)
    (let* ((handler (lenv-get-handler id))
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

;;;###autoload (defalias 'librarian-envs-start! #'librarian--envs-start)

;;;###autoload (autoload 'librarian--envs-start "librarian--envs")
(defun lenv-start (arg &rest ids)
  " Main access point for setting up environment.
Acts as a Dispatch to activate appropriate environment
and call the currently selected lsp/conda client entrypoint
handlers can be passed as 2nd+ args

pass a prefix arg to use ivy to manually select from registered handlers
"

  (interactive "P")
  (let* ((loc (lenv-init-loc))
         ;; Handlers: (list handler-id | (handler-id args)
         (specs (mapcar #'ensure-list (or ids (lenv-parse-marker loc))))
         states
         )
    (when arg (setq specs nil)
          (ivy-read "Available Handlers: "
                    (hash-table-keys lenv-registered)
                    :require-match t
                    :action #'(lambda (x) (add-to-list 'specs (list x)))
                    ))
    (if (not specs)
        (user-error "No Handlers scheduled to run")
      (message "Activating Environment Handlers: ")
      (cl-loop for spec in specs do (message "- %s" spec)))

    ;; wrap handlers in a state with loc
    (setq states (cl-loop for vals in specs
                          collect
                          (funcall #'lenv-activate-handler (car vals) loc (cdr vals))))
    ;; Add modeline fn
    (add-to-list 'global-mode-string
                '(:eval (lenv-mode-line-fn)))

    (prog1
        ;; now activate
        (cl-loop for state in states
                 for valid              = (and state (lenv-state-p state) (not (lenv-state-locked state)))
                 when valid for status  = (lenv-state-status state)
                 when valid for handler = (lenv-get-handler (lenv-state-id state))
                 when (and valid handler (eq status 'nil)) do
                 ;; run setup and set modeline
                 (--if-let (lenv-handler-setup handler) (apply it state (lenv-state-data state)))
                 (setf (lenv-state-status state) 'setup)
                 when (and valid handler (eq status 'setup)) do
                 ;; run start
                 (--if-let (lenv-handler-start handler)    (apply it state (lenv-state-data state)))
                 (--if-let (lenv-handler-modeline handler) (setf (lenv-state-modeline state)
                                                                 (apply it state (lenv-state-data state))))
                 (setf (lenv-state-status state) 'active)
                 ;; collect them to return
                 when t collect state
                 )
      ;; run enter hooks
      (run-hooks 'lenv-enter-hook)
      ;; Report
      (lenv-report-message "Activation" loc)
      )
    )
  )

;;;###autoload (defalias 'librarian-envs-stop! #'librarian--envs-stop)
;;;###autoload (autoload 'librarian--envs-stop "librarian--envs")
(defun lenv-stop (arg &rest ids)
  (interactive "P")
  (let* ((loc (lenv-init-loc))
         (specs (or ids (--if-let (lenv-parse-marker loc) (mapcar #'car it))))
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
    (if (not specs)
        (user-error "No Handlers scheduled to run")
      (message "Deactivating Environment Handlers: ")
      (cl-loop for spec in specs
               do (message "- %s" spec)))

    (setq states (cl-loop for id in specs
                          collect
                          (lenv-get-state id)
                          ))
    (prog1
        (cl-loop for state in states
                 for valid = (and state (lenv-state-p state) (not (lenv-state-locked state)))
                 when valid for status = (lenv-state-status state)
                 when valid for handler = (lenv-get-handler (lenv-state-id state))
                 ;; Deactivate
                 when (and valid handler (eq status 'active)) do
                 (--if-let (lenv-handler-stop handler)     (apply it state (lenv-state-data state)))
                 (setf (lenv-state-status state) 'setup
                       (lenv-state-modeline state) nil)
                 ;; or Teardown
                 when (and valid handler (eq status 'setup)) do
                 (--if-let (lenv-handler-teardown handler) (apply it state (lenv-state-data state)))
                 (setf (lenv-state-status state) nil)
                 (remhash (lenv-state-id state) lenv-active)
                 when t collect state
                 )
      ;; Run Exit hooks
      (run-hooks 'lenv-exit-hook)
      ;; Report
      (lenv-report-message "Activation" loc)
      )
    )
  )

;;;###autoload (defalias 'librarian-envs-toggle-lock! #'librarian--envs-toggle-lock)
;;;###autoload (autoload 'librarian--envs-toggle-lock "librarian--envs")
(defun lenv-toggle-lock (&rest rest)
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
             (setf (lenv-state-locked (lenv-get-state name))
                   (not (lenv-state-locked (lenv-get-state name)))
                   )
             )
    )
  )

;;;###autoload (defalias 'librarian-envs-report! #'librarian--envs-report)
;;;###autoload (autoload 'librarian--envs-report "librarian--envs")
(defun lenv-report ()
  "Display a report of all registered environments, and which are activated "
  (interactive)
  (with-temp-buffer-window "*Envs Report*" 'display-buffer nil
    (princ "* Active Environments:\n")
    (cl-loop for id being the hash-keys of lenv-active
             using (hash-values state)
             for handler = (lenv-get-handler id)
             do
             (princ (format "** %s : (%s)\n" id (lenv-state-status state)))
             (princ (format "- Root: %s\n" (lenv-loc-root (lenv-state-loc state))))
             (princ (format "- Args: %s\n" (lenv-state-data state)))
             )
    (princ "\n* Registered Environment Handlers:\n")
    (cl-loop for id being the hash-keys of lenv-registered
             using (hash-values handler)
             do
             (princ (format "** %s: \n" id))
             )
    )
  (with-current-buffer "*Envs Report*"
    (org-mode)
    (goto-char (point-min))
    )
)

(defun lenv-mode-line-fn ()
  "A Function for formatting active environment strings for the modeline"
  (-if-let (modelines (cl-loop for state being the hash-values of lenv-active
                               when (lenv-state-modeline state)
                               collect (lenv-state-modeline state)
                               ))
      (format "(Envs: %s)" (string-join modelines " "))
    ""
    )
  )


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
