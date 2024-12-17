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

(defvar lio--provider-url-alist nil
  "An alist that maps online resources to either:

  1. A search url (needs on '%s' to substitute with an url encoded query),
  2. A non-interactive function that returns the search url in #1,
  3. An interactive command that does its own search for that provider.

Used by `librarian-online'.")

(defvar lio--open-url-fn #'browse-url "Function to use to open search urls.")

(defvar lio--last-provider nil)

(defun lio--provider (&optional force-p namespace)
  (let ((key (or namespace major-mode)))
    (or (and (not force-p)
             (cdr (assq key lio--last-provider)))
        (when-let (provider
                   (completing-read
                    "Search on: "
                    (mapcar #'car lio--provider-url-alist)
                    nil t))
          (setf (alist-get key lio--last-provider) provider)
          provider))))

;;;###autoload
(defun librarian-online (query provider)
  "Look up QUERY in the browser using PROVIDER.
When called interactively, prompt for a query and, when called for the first
time, the provider from `lio--provider-url-alist'. In subsequent calls, reuse
the previous provider. With a non-nil prefix argument, always prompt for the
provider.

QUERY must be a string, and PROVIDER must be a key of
`lio--provider-url-alist'."
  (interactive
   (list (if (use-region-p) (librarian--util-get))
         (lio--provider current-prefix-arg)))

  (let ((backends (cdr (assoc provider lio--provider-url-alist)))
        (thing (thing-at-point 'symbol t))
        )
    (unless backends
      (user-error "No available online lookup backend for %S provider" provider))
    (catch 'done
      (dolist (backend backends)
        (cl-check-type backend (or string function))
        (cond ((stringp backend) ;; backend is a string, use default
               (let* ((prompt (format "Search for (on %s): " provider))
                      (the_query (or query (read-string prompt thing)))
                      (url (url-encode-url (format backend the_query)))
                      )
                 (funcall lio--open-url-fn url))
               (throw 'done t))
              ((condition-case-unless-debug e
                   (and (fboundp backend)
                        (funcall backend query))
                 (error
                  (delq! major-mode lio--last-provider 'assq)
                  (signal (car e) (cdr e))))
               (throw 'done t)))))))

(defun librarian-online-select ()
  "Run `librarian-online', but always prompt for the provider to use."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'librarian-online)))

;;;###autoload
(defun librarian-url (&optional url &rest args)
  " use librarian to open a url, in place of `browse-url`' "
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
  (librarian-online query (lio--provider bang 'evil-ex)))

(provide 'librarian--online)
;;; lookup-search.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("lio-" . "librarian--online-")
;; )
;; End:
