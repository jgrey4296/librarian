;;; lib-.el -*- lexical-binding: t; -*-
;; TODO handle name conflicts between modes

(eval-when-compile
  (require 'cl-lib)
  (require 'f)
  (require 'ivy)
  (require 'parent-mode)
  )

(defvar lib-location nil)

(defvar lib-cache     (make-hash-table :test 'equal))

(defvar lib-key-cache (make-hash-table :test 'equal))

(defvar lib-processors (make-hash-table :test 'equal))

(defvar-local lib-keys nil)

(defun lib--propertize (mode file)
  (let ((base (format "%-20s # %s" (f-filename file) mode)))
    (set-text-properties 0 (length base) `(path ,file) base)
    base
    )
  )

;;;###autoload (defalias 'librarian-insert-clear-caches #'librarian--insert-clear-caches)
;;;###autoload (autoload 'librarian--insert-clear-caches "librarian--insert")
(defun lib-clear-caches ()
  "Clear and Rebuild the cache"
  (interactive)
  (message "Clearing General Insert Cache")
  (setq lib-cache (make-hash-table :test 'equal)
        lib-key-cache (make-hash-table :test 'equal)
        )
  (setq-local lib-keys nil)
  (lib-build-cache)
  )

(defun lib-build-cache ()
  " Build the buffer local general insert cache "
  (interactive)
  (setq-local lib-keys
              (cl-loop for mode in (append (parent-mode-list major-mode) '(fundamental-mode) local-minor-modes global-minor-modes)
                       for exists = (f-exists? (f-join lib-location (symbol-name mode)))
                       when (and exists (not (gethash mode lib-key-cache)))
                       do
                       (puthash mode
                                (mapcar (-partial #'lib--propertize (symbol-name mode))
                                        (-reject (-partial #'f-ext? "DS_Store")
                                                 (f-files (f-join lib-location (symbol-name mode)))))
                                lib-key-cache)
                       when exists
                       append (gethash mode lib-key-cache)
                       )
              )
  )

(defun lib-default (x)
  "The Default insertion function.
Splits the result by '#'
"
  (insert (car (split-string x "#" t " +")))
  )

;;;###autoload (defalias 'librarian-insert-trigger #'librarian--insert-trigger)
;;;###autoload (autoload 'librarian--insert-trigger "librarian--insert")
(defun lib-trigger ()
  " Entry ivy for insertions "
  (interactive)
  (ivy-read "Insert: " lib-keys
            :require-match t
            :sort t
            :action #'lib-call-sub-ivy
            )
  )

(defun lib-call-sub-ivy (selected)
  "The ivy for actually inserting a result"
  (unless (gethash selected lib-cache)
    (puthash selected (lib-load-file
                       (get-text-property 0 'path selected))
             lib-cache))

  (let* ((vals (gethash selected lib-cache))
         (selected-core (car (split-string selected "#" t " +")))
         (processor (gethash `(,major-mode ,selected-core) lib-processors #'lib-default))
         )
    (message "Retrieved: %s : %s : %s" major-mode selected-core processor)
    (when (and vals processor)
      (ivy-read (format "%s " (car vals))
                (cdr vals)
                :action processor
                :require-match t
                )
      )
    )
  )

;;;###autoload (defalias 'librarian-insert-register-processor #'librarian--insert-register-processor)
;;;###autoload (autoload 'librarian--insert-register-processor  "librarian--insert")
(defun lib-register-processor (mode key fn)
    "For a {mode} and a {key} group of inserts in that mode,
use {fn} to transform the insert value before inserting
fn is (lambda (str) (insert str))
"
    (when (gethash `(,mode ,key) lib-processors)
      (display-warning 'librarian-insert (format "Overwriting processor for: %s, %s" mode key)))
    (puthash `(,mode ,key) fn lib-processors)
  )

(defun lib-load-file (file)
  "read a (prompt . (items:list)) from the given file"
  (unless (f-exists? file)
    (user-error "Tried To Load a non-existent file: %s" file))
  (let (targets)
    (with-temp-buffer
      (insert-file-contents file)
      (setq targets (s-lines (buffer-substring-no-properties (point-min) (point-max))))
      )
    targets
    )
  )

;;;###autoload (defalias 'librarian-insert-minor-mode #'librarian--insert-minor-mode)
;;;###autoload (autoload 'librarian--insert-minor-mode "librarian--insert")
(define-minor-mode lib-minor-mode
  " Generalized insert mode for simple strings"
  :init-value nil
  :lighter "lib-"
  (lib-build-cache)
)

;;; Public Aliases

(defvaralias 'librarian-insert-loc 'librarian--insert-location)

(provide 'librarian--insert)
;; Local Variables:
;; read-symbol-shorthands: (
;; ("lib-" . "librarian--insert-")
;; )
;; End:
