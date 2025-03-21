;;; lookup-search.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;;; Commentary:
;;
;;
;;
;;; Code:
;;-- end header

(eval-when-compile
  (require 'url-util)
  (require 'librarian--browse)
  )

(defvar lio-providers (make-hash-table :test #'equal))

(defvar lio--open-url-fn
  #'librarian-browse-open
  ;; #'browse-url
  "Function to use to open search urls.")

(defvar lio--last-provider nil)

(defun lio-register-provider (name target)
  "Register a new online search provider.
Target is either:
- a format url string
- a fn of one arg
"
  (cl-assert (stringp name))
  (cl-assert (or (stringp target) (functionp target)) t
             (format "Failed to Register: %s, %s" target (type-of target)))
  (puthash name target lio-providers)
  )

(defun lio-get-provider (&optional force-p)
  "Get the provider to use,
reuses the last provider
Returns str or fn
 "
  (when (or force-p (not lio--last-provider))
    (let ((provname (completing-read "Search on: " lio-providers nil t)))
      (setq lio--last-provider (cons provname (gethash provname lio-providers))))
    )
  lio--last-provider
  )

;;;###autoload
(defun librarian-online (query provider)
  "Look up QUERY in the browser using PROVIDER.
When called interactively, prompt for a query and, when called for the first
time, the provider from `lio-providers'. In subsequent calls, reuse
the previous provider. With a non-nil prefix argument, always prompt for the
provider.

QUERY must be a string, and PROVIDER must be a key of
`lio-providers'."
  (interactive
   (list (when (use-region-p) (librarian--util-get))
         (lio-get-provider current-prefix-arg)))
  ;;
  (unless provider
    (user-error "No available online lookup backend for %S provider" provider))
  (let* ((thing (thing-at-point 'symbol t))
         (provname (car provider))
         (provtarg (cdr provider))
         )
    (cond ((functionp provtarg)
           (funcall provtarg (or query thing)))
          ((stringp provtarg)
           (funcall lio--open-url-fn
                    (url-encode-url
                     (format provtarg
                             (read-string
                              (format "Search for (on %s): " provname)
                              (or query thing)))))
           )
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
  (librarian-online query (lio-get-provider bang))
  )

(provide 'librarian--online)
;;; lookup-search.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("lio-" . "librarian--online-")
;; )
;; End:
