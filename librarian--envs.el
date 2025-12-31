;;; librarian--envs.el -*- lexical-binding: t; no-byte-compile: t; -*-

(eval-when-compile
  (require 'cl-lib)
  (require 'dash)
  (require 'f)
  (require 's)
  (require 'macro-tools--util)
  (require 'projectile)
  )

;;-- vars

(defvar librarian--envs-enter-hook nil "A general hook for when entering an environment")

(defvar librarian--envs-exit-hook nil "A general hook for exiting an environment")

(defvar librarian--envs-registered (make-hash-table) "Mapping of env names to their structs")

(defvar-local librarian--envs-modeline-list (list))

(defvar librarian--envs-envvar "LIBRARIAN_HANDLERS")

(defvar librarian--envs-envvar-sep ":")

(defvar librarian--envs-envvar-opts "+")

(defconst librarian--envs-process-name
  "envs-handling-proc"
  "The name of the process librarian uses to run external actions")

(defconst librarian--envs-buffer-name
  "*envs-handling*"
  "The buffer librarian puts report output into"
  )

(defconst librarian--envs-modeline-format '("(Env: " (:eval librarian--envs-modeline-list) " )"))

(defvar librarian--envs-active (make-hash-table) "maps id -> activated handlers")

;;-- end vars

;;-- structs
;;;###autoload (defalias 'librarian-envs-handler-p #'librarian--envs-handler-p)
;;;###autoload (autoload 'librarian-envs-handler-p "librarian--envs")

(cl-defstruct (librarian--envs-handler)
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
  (headline nil     :type 'lambda           :read-only t :documentation "added to global-mode-string. eval'd with the state on entry")
  (cmds     nil     :type 'list             :read-only t :documentation "eg: install package, update...")
  (desc     nil     :type 'str              :read-only t :documentation "for reporting")
  )

(cl-defstruct (librarian--envs-state)
  "The current envs state for a handler"
  (id       nil   :type 'symbol :documentation "the same as the handler")
  (status   nil   :type 'symbol :documentation "nil|setup|active")
  (loc      nil   :type 'librarian--envs-loc)
  (locked   nil   :type 'bool)
  (data     nil   :type 'list :documentation "a list for arbitrary data handlers can put")
  (modeline nil   :type 'string)
  (headline nil   :type 'string)
  )

(cl-defstruct (librarian--envs-loc)
  "Description of where environment data was found"
  (root       nil :type 'path :documentation "root of the project")
  )
;;-- end structs

(defun librarian--envs-report-message (direction loc)
  "A Single message to report on the status of the environment at the end of librarian--envs-start/stop"
  (when (not (hash-table-empty-p librarian--envs-active))
    (message "Env %s : %s\n%s"
             direction
             (librarian--envs-loc-root loc)
             (string-join (cl-loop for state being the hash-values in librarian--envs-active
                                   for id     = (librarian--envs-state-id state)
                                   for locked = (librarian--envs-state-locked state)
                                   for status = (librarian--envs-state-status state)
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
(defun librarian--envs-register (id &rest args)
  " Register a new handler.
Either a librarian--envs-handler, or a plist to build one
"
  (let* ((new-handler (if (librarian--envs-handler-p id)
                          id
                        (librarian--envs-macro-aware-build-handler id args)))
         (id (librarian--envs-handler-id new-handler))
         )
    (if (gethash id librarian--envs-registered)
        (message "A Handler has already been registered with the name: %s" id)
      (puthash id new-handler librarian--envs-registered)
      (message "Registered Env Handler: %s" id)
      )
    )
)

(defun librarian--envs-macro-aware-build-handler (id args)
  "Build a handler, ensuring the callbacks are actually functions"
  (cl-assert (plistp args) t "Should be a plist")
  (setq args (plist-put args :setup    (upfun! (plist-get args :setup))))
  (setq args (plist-put args :start    (upfun! (plist-get args :start))))
  (setq args (plist-put args :stop     (upfun! (plist-get args :stop))))
  (setq args (plist-put args :teardown (upfun! (plist-get args :teardown))))
  (setq args (plist-put args :modeline (upfun! (plist-get args :modeline))))
  (setq args (plist-put args :headline (upfun! (plist-get args :headline))))
  (apply #'make-librarian--envs-handler :id id args)
  )

;;;###autoload (defalias 'librarian-envs-clear! #'librarian--envs-clear-registry)

;;;###autoload (autoload 'librarian-envs-clear! "librarian--envs")
(defun librarian--envs-clear-registry (&optional force)
  (interactive)
  (if (not (or force (hash-table-empty-p librarian--envs-active)))
      (message "There are active environments, deactivate them before clearing")
    (message "Clearing Registered Environment Handlers")
    (clrhash librarian--envs-registered)
    (clrhash librarian--envs-active)
    )
  )

(defun librarian--envs-init-loc (&optional start)
  " return an envs-loc "
  (let* ((root (or (projectile-project-root start) default-directory)))
    (make-librarian--envs-loc :root root)
    )
  )

(defun librarian--envs-read-environment ()
  "Read the environment variable LIBRARIAN-HANDLERS and return as a list"
  (interactive)
  (unless (getenv librarian--envs-envvar) (user-error "There is no %s envvar" librarian--envs-envvar))
  (let* ((val (getenv librarian--envs-envvar))
         (split-handlers (s-split librarian--envs-envvar-sep val t))
         (split-opts (mapcar #'(lambda (x) (s-split librarian--envs-envvar-opts x))
                             split-handlers
                             ))
         )
    split-opts
    )
  )

(defun librarian--envs-get-handler (id)
  "Get a handler by its id.
handles both strings and symbols
"
  (pcase id
    ((pred librarian--envs-handler-p) id)
    ((pred symbolp) (gethash id librarian--envs-registered))
    ((pred stringp) (gethash (intern id) librarian--envs-registered))
    (x (user-error "Tried to get a handler with a bad type: %s" id))
    )
 )

(defun librarian--envs-get-state (id)
  (pcase id
    ((pred librarian--envs-state-p) id)
    ((pred symbolp) (gethash id librarian--envs-active))
    ((pred stringp) (gethash (intern id) librarian--envs-active))
    (x (user-error "Tried to get a state with a bad type: %s" id))
    )
  )

(defun librarian--envs-activate-handler (id loc &optional data)
  " wrap a handler and loc into a state and add it to librarian--envs-active.
then return the state
"
  (unless (librarian--envs-get-handler id) (error "Tried to activate a non-registered handler" id))
  (unless (librarian--envs-loc-p loc) (error "tried to activate a handler with an invalid loc" loc))
  (if (librarian--envs-get-state id)
      (librarian--envs-get-state id)
    (let* ((handler (librarian--envs-get-handler id))
          (state (make-librarian--envs-state
                  :id (librarian--envs-handler-id handler)
                  :status nil
                  :loc loc
                  :locked nil
                  :data data)))
      (puthash (librarian--envs-state-id state) state librarian--envs-active)
      state
      )
    )
  )

;;;###autoload (defalias 'librarian-envs-start! #'librarian--envs-start)

;;;###autoload (autoload 'librarian--envs-start "librarian--envs")
(defun librarian--envs-start (arg &rest ids)
  " Main access point for setting up environment.
Acts as a Dispatch to activate appropriate environment
and call the currently selected lsp/conda client entrypoint
handlers can be passed as 2nd+ args

pass a prefix arg to use ivy to manually select from registered handlers
"

  (interactive "P")
  (let* ((loc (librarian--envs-init-loc))
         ;; Handlers: (list handler-id | (handler-id args)
         (specs (mapcar #'ensure-list (or ids (librarian--envs-read-environment))))
         states
         )
    (when arg (setq specs nil)
          (ivy-read "Available Handlers: "
                    (hash-table-keys librarian--envs-registered)
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
                          (funcall #'librarian--envs-activate-handler (car vals) loc (cdr vals))))
    ;; Use the minor mode for modeline and headline modification:
    (add-to-list 'global-mode-string librarian--envs-modeline-format)
    (add-hook 'prog-mode-hook #'librarian-env-minor-mode)

    (prog1
        ;; now activate
        (cl-loop for state in states
                 for valid              = (and state (librarian--envs-state-p state) (not (librarian--envs-state-locked state)))
                 when valid for status  = (librarian--envs-state-status state)
                 when valid for handler = (librarian--envs-get-handler (librarian--envs-state-id state))
                 when (and valid handler (eq status 'nil)) do
                 ;; run setup
                 (--if-let (librarian--envs-handler-setup handler) (apply it state (librarian--envs-state-data state)))
                 (setf (librarian--envs-state-status state) 'setup)
                 when (and valid handler (eq status 'setup)) do
                 ;; run start, setting modeline
                 (--if-let (librarian--envs-handler-start handler)    (apply it state (librarian--envs-state-data state)))
                 (--if-let (librarian--envs-handler-modeline handler) (setf (librarian--envs-state-modeline state)
                                                                 (apply it state (librarian--envs-state-data state))))
                 (--if-let (librarian--envs-handler-headline handler) (setf (librarian--envs-state-headline state)
                                                                 (apply it state (librarian--envs-state-data state))))
                 (setf (librarian--envs-state-status state) 'active)
                 ;; collect them to return
                 when t collect state
                 )
      ;; run enter hooks
      (run-hooks 'librarian--envs-enter-hook)
      ;; Report
      (librarian--envs-report-message "Activation" loc)
      )
    )
  )

;;;###autoload (defalias 'librarian-envs-stop! #'librarian--envs-stop)

;;;###autoload (autoload 'librarian--envs-stop "librarian--envs")
(defun librarian--envs-stop (arg &rest ids)
  (interactive "P")
  (let* ((loc (librarian--envs-init-loc))
         (specs (or ids (--if-let (librarian--envs-read-environment) (mapcar #'car it))))
         states
         )
    (when arg
      (setq specs nil)
      (ivy-read "Available Handlers: "
                (hash-table-keys librarian--envs-active)
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
                          (librarian--envs-get-state id)
                          ))
    (prog1
        (cl-loop for state in states
                 for valid = (and state (librarian--envs-state-p state) (not (librarian--envs-state-locked state)))
                 when valid for status = (librarian--envs-state-status state)
                 when valid for handler = (librarian--envs-get-handler (librarian--envs-state-id state))
                 ;; Deactivate, removing from modeline
                 when (and valid handler (eq status 'active)) do
                 (--if-let (librarian--envs-handler-stop handler)     (apply it state (librarian--envs-state-data state)))
                 (setf (librarian--envs-state-status state) 'setup
                       (librarian--envs-state-modeline state) nil
                       (librarian--envs-state-headline state) nil
                       )
                 ;; or Teardown
                 when (and valid handler (eq status 'setup)) do
                 (--if-let (librarian--envs-handler-teardown handler) (apply it state (librarian--envs-state-data state)))
                 (setf (librarian--envs-state-status state) nil)
                 (remhash (librarian--envs-state-id state) librarian--envs-active)
                 when t collect state
                 )
      ;; If nothing active, clear the mode line
      (unless (hash-table-values librarian--envs-active)
        (setq global-mode-string (remove librarian--envs-modeline-format global-mode-string))
        )
      ;; Run Exit hooks
      (run-hooks 'librarian--envs-exit-hook)
      ;; Report
      (librarian--envs-report-message "Activation" loc)
      )
    )
  )

;;;###autoload (defalias 'librarian-envs-toggle-lock! #'librarian--envs-toggle-lock)

;;;###autoload (autoload 'librarian--envs-toggle-lock "librarian--envs")
(defun librarian--envs-toggle-lock (&rest rest)
  "Toggle whether the environment can be changed or not"
  (interactive)
  (let ((ids rest))
    (unless ids (ivy-read "Available Handlers: "
                          (mapcar #'librarian--envs-state-id librarian--envs-active)
                          :require-match t
                          :action #'(lambda (x) (add-to-list 'handlers x))
                          ))
    (cl-loop for name in ids
             do
             (setf (librarian--envs-state-locked (librarian--envs-get-state name))
                   (not (librarian--envs-state-locked (librarian--envs-get-state name)))
                   )
             )
    )
  )

;;;###autoload (defalias 'librarian-envs-report! #'librarian--envs-report)

;;;###autoload (autoload 'librarian--envs-report "librarian--envs")
(defun librarian--envs-report ()
  "Display a report of all registered environments, and which are activated "
  (interactive)
  (with-temp-buffer-window "*Envs Report*" 'display-buffer nil
    (princ "* Active Environments:\n")
    (cl-loop for id being the hash-keys of librarian--envs-active
             using (hash-values state)
             for handler = (librarian--envs-get-handler id)
             do
             (princ (format "** %s : (%s)\n" id (librarian--envs-state-status state)))
             (princ (format "- Root: %s\n" (librarian--envs-loc-root (librarian--envs-state-loc state))))
             (princ (format "- Args: %s\n" (librarian--envs-state-data state)))
             )
    (princ "\n* Registered Environment Handlers:\n")
    (cl-loop for id being the hash-keys of librarian--envs-registered
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

(define-minor-mode librarian-env-minor-mode
  "Minor mode to enable librarian-specific local hooks for buffers.

eg: Setting head-line-format
"
  :lighter "LEnv-m"
  )

(defun librarian--envs-mode-activator ()
  """ adds to the modeline var """
  (let* ((states (hash-table-values librarian--envs-active))
         )
    (cl-loop for state in states
             for headline = (librarian--envs-state-modeline state)
             if headline
             do (add-to-list 'librarian--envs-modeline-list " ")
             and do (add-to-list 'librarian--envs-modeline-list headline)
             )
    )
  )

(defun librarian--envs-head-activator ()
  "Adds to the buffer local header-line-format"
  (let* ((states (hash-table-values librarian--envs-active))
         )
    (cl-loop for state in states
             for headline = (librarian--envs-state-headline state)
             if headline
             do
             (add-to-list 'header-line-format headline)
             )
    )
  )

(add-hook 'librarian-env-minor-mode-hook #'librarian--envs-head-activator)
(add-hook 'librarian-env-minor-mode-hook #'librarian--envs-mode-activator)

(provide 'librarian--envs)
;;; librarian-envs.el ends here
