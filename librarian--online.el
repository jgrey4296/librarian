;;; lookup-search.el -*- lexical-binding: t; no-byte-compile: t; -*-

(eval-when-compile
  (require 'url-util)
  (require 'librarian--browse)
  )

(defvar librarian--online-providers (make-hash-table :test #'equal))

(defvar librarian--online--open-url-fn
  #'librarian-browse-open
  ;; #'browse-url
  "Function to use to open search urls.")

(defvar librarian--online--last-provider nil)

(defun librarian--online-register-provider (name target)
  "Register a new online search provider.
Target is either:
- a format url string
- a fn of one arg
"
  (cl-assert (stringp name))
  (cl-assert (or (stringp target) (functionp target)) t
             (format "Failed to Register: %s, %s" target (type-of target)))
  (puthash name target librarian--online-providers)
  )

(defun librarian--online-get-provider (&optional name force-p)
  "Get the provider to use,
reuses the last provider
Returns str or fn
 "
  (setq librarian--online--last-provider
        (cond (force-p
               ;; force read
               (let ((provname (completing-read "Search on: " librarian--online-providers nil t)))
                 (cons provname (gethash provname librarian--online-providers))))
              ((gethash name librarian--online-providers)
               ;; Provided name
               (cons name (gethash name librarian--online-providers)))
              (librarian--online--last-provider
               librarian--online--last-provider)
              (t
               (let ((provname (completing-read "Search on: " librarian--online-providers nil t)))
                 (cons provname (gethash provname librarian--online-providers))))
              ))
  librarian--online--last-provider
  )

;;;###autoload
(defun librarian-online (query &optional provider noconfirm)
  "Look up QUERY in the browser using PROVIDER.
When called interactively, prompt for a query and, when called for the first
time, the provider from `librarian--online-providers'. In subsequent calls, reuse
the previous provider. With a non-nil prefix argument, always prompt for the
provider.

QUERY must be a string, and PROVIDER must be a key of
`librarian--online-providers'."
  (interactive
   (list (when (use-region-p) (librarian--util-get))))
  ;;
  (let* ((thing (thing-at-point 'symbol t))
         (provider (librarian--online-get-provider provider current-prefix-arg))
         (provname (car-safe provider))
         (provtarg (cdr-safe provider))
         )
    (unless provider
      (user-error "No available online lookup backend for %S provider" provider))
    (cond ((functionp provtarg)
           (funcall provtarg (or query thing)))
          (noconfirm
           (funcall librarian--online--open-url-fn
                    (url-encode-url (format provtarg (or query thing)))))
          ((stringp provtarg)
           (let ((val (read-string (format "Search for (on %s): " provname) (or query thing)))
                 )
                 (funcall librarian--online--open-url-fn
                          (url-encode-url (format provtarg val)))))
          (_ (user-error "Unknown provider target type: %s -> %s :: %s"
                         provname provtarg (type-of provtarg)))
          )
    )
  )

(defun librarian-online-select ()
  "Run `librarian-online', but always prompt for the provider to use."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'librarian-online)))

;;;###autoload
(defun librarian-url (&optional url &rest args)
  "use librarian to open a url "
  (interactive)
  (let ((url (cond (url url)
                   ((and (boundp 'evil-state) (eq evil-state 'visual))
                    (buffer-substring-no-properties evil-visual-beginning evil-visual-end))
                   (t nil)))
        )
    (cond ((not url)
           (librarian-online-select))
          ((f-exists? url)
           (shell-command (format "open %s" url)))
          (t
           (call-interactively #'librarian-online) ;;TODO
           )
          )
    )
  )

(evil-define-command evil-librarian-online (query &optional bang)
  "Look up QUERY online. Will prompt for search engine the first time, then
reuse it on consecutive uses of this command. If BANG, always prompt for search
engine."
  (interactive "<a><!>")
  (librarian-online query (librarian--online-get-provider nil bang))
  )

(provide 'librarian--online)
;;; librarian--online.el ends here
