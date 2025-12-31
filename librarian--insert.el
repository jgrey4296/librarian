;;; librarian--insert-.el -*- lexical-binding: t; -*-
;; TODO handle name conflicts between modes

(eval-when-compile
  (require 'cl-lib)
  (require 'f)
  (require 's)
  (require 'ivy)
  (require 'parent-mode)

  (declare-function s-lines "s")
  (declare-function ivy-read "ivy")
  (declare-function f-files "f")
  (declare-function f-ext? "f")
  (declare-function f-join "f")
  (declare-function f-exists? "f")
  (declare-function f-filename "f")
  (declare-function -reject "dash")
  (declare-function -partial "dash")
  (declare-function parent-mode-list "parent-mode")
  )

(defvar librarian-insert-loc nil "Where Insert files are located")

(defvar librarian--insert-cache     (make-hash-table :test 'equal))

(defvar librarian--insert-key-cache (make-hash-table :test 'equal))

(defvar librarian--insert-processors (make-hash-table :test 'equal))

(defvar-local librarian--insert-keys nil)

(defun librarian--insert--propertize (mode file)
  (let ((base (format "%-20s # %s" (f-filename file) mode)))
    (set-text-properties 0 (length base) `(path ,file) base)
    base
    )
  )

;;;###autoload (autoload 'librarian--insert-clear-caches "librarian--insert")
(defun librarian--insert-clear-caches ()
  "Clear and Rebuild the cache"
  (interactive)
  (message "Clearing General Insert Cache")
  (setq librarian--insert-cache (make-hash-table :test 'equal)
        librarian--insert-key-cache (make-hash-table :test 'equal)
        )
  (setq-local librarian--insert-keys nil)
  (librarian--insert-build-cache)
  )

(defun librarian--insert-build-cache ()
  " Build the buffer local general insert cache "
  (interactive)
  (let ((modes (append (parent-mode-list major-mode)
                       local-minor-modes
                       global-minor-modes
                       '(fundamental-mode)
                       ))
        )
    (when librarian-insert-loc
      (setq-local librarian--insert-keys
                  (cl-loop for mode in modes
                           for exists = (f-exists? (f-join librarian-insert-loc (symbol-name mode)))
                           when (and exists (not (gethash mode librarian--insert-key-cache)))
                           do
                           (puthash mode
                                    (mapcar (-partial #'librarian--insert--propertize (symbol-name mode))
                                            (-reject (-partial #'f-ext? "DS_Store")
                                                     (f-files (f-join librarian-insert-loc (symbol-name mode)))))
                                    librarian--insert-key-cache)
                           when exists
                           append (gethash mode librarian--insert-key-cache)
                           )
                  )
      )
    )
  )

(defun librarian--insert-default (x)
  "The Default insertion function.
Splits the result by '#'
"
  (insert (car (split-string x "#" t " +")))
  )

;;;###autoload (autoload 'librarian--insert-trigger "librarian--insert")
(defun librarian--insert-trigger ()
  " Entry ivy for insertions "
  (interactive)
  (ivy-read "Insert: " librarian--insert-keys
            :require-match t
            :sort t
            :action #'librarian--insert-call-sub-ivy
            )
  )

(defun librarian--insert-call-sub-ivy (selected)
  "The ivy for actually inserting a result"
  (unless (gethash selected librarian--insert-cache)
    (puthash selected (librarian--insert-load-file
                       (get-text-property 0 'path selected))
             librarian--insert-cache))

  (let* ((vals (gethash selected librarian--insert-cache))
         (selected-core (car (split-string selected "#" t " +")))
         (processor (gethash `(,major-mode ,selected-core) librarian--insert-processors #'librarian--insert-default))
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

;;;###autoload (autoload 'librarian--insert-register-processor  "librarian--insert")
(defun librarian--insert-register-processor (mode key fn)
    "For a {mode} and a {key} group of inserts in that mode,
use {fn} to transform the insert value before inserting
fn is (lambda (str) (insert str))
"
    (when (gethash `(,mode ,key) librarian--insert-processors)
      (display-warning 'librarian-insert (format "Overwriting processor for: %s, %s" mode key)))
    (puthash `(,mode ,key) fn librarian--insert-processors)
  )

(defun librarian--insert-load-file (file)
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

;;;###autoload (autoload 'librarian--insert-minor-mode "librarian--insert")
(define-minor-mode librarian--insert-minor-mode
  " Generalized insert mode for simple strings"
  :init-value nil
  :lighter "librarian--insert-"
  (librarian--insert-build-cache)
)

(provide 'librarian--insert)
;;; librarian--insert.el ends here
