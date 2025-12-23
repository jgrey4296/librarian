;;; librarian.el --- Various lookup utilities -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey@Johns-Mac-mini.local>
;; Created: September 03, 2023
;; Version: 1.0.0
;; Keywords:
;; Homepage: https://github.com/jgrey4296/librarian
;; Package-Requires: ((emacs "30.2") (better-jumper) (bibtex) (bibtex-completion) (bookmark) (cl-lib) (company) (counsel) (counsel-dash) (dash) (dash-docs) (define-word) (eldoc) (evil) (eww) (f) (flyspell) (flyspell-correct) (flyspell-correct-ivy) (flyspell-lazy) (helm-wordnet) (ispell) (ivy) (macro-tools "1.0.0") (org-ref) (parent-mode) (powerthesaurus) (s) (spell-fu) (synosaurus) (thingatpt) (wordnut) (xref) (projectile))
;; Package written on: ((emacs 30.2))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  A General controller for lookup, spelling, definition and online search
;;
;;
;;; Code:


(require 'lib--util)
(require 'lib--backend)
(require 'lib--structs)

(require 'lib--biblio)
(require 'lib--browse)
(require 'lib--config)
(require 'lib--docsets)
(require 'lib--doc)
(require 'lib--envs)
(require 'lib--man)
(require 'lib--online)
(require 'lib--regular)
(require 'lib--tag)
(require 'lib--tag-chart)
(require 'lib--tag-mode)
(require 'lib--words)
(require 'lib--insert)

(librarian--doc-init-defaults)

(defconst lib-active-on-modes (list 'text-mode 'prog-mode 'conf-mode))

(defconst lib-forbid-modes (list 'magit-mode))

(defvar lib-mode-map (make-sparse-keymap))

(evil-make-intercept-map lib-mode-map 'normal)

;;;###autoload (autoload 'librarian-mode "librarian")
(define-minor-mode lib-mode
  "An interface for controlling lookups, spelling, documentation, online search"
  :lighter "Librarian"
  :keymap librarian-mode-map
  )

(defun librarian-mode-active-p ()
  (and (not (minibufferp))
       (not (apply #'derived-mode-p librarian-forbid-modes))
       (apply #'derived-mode-p librarian-active-on-modes)
       )
  )

(defun librarian-mode/turn-on ()
  (when (librarian-mode-active-p)
    (librarian-mode 1)
    )
  )

;;;###autoload
(define-globalized-minor-mode global-librarian-mode librarian-mode librarian-mode/turn-on
  (message "Librarian: %s" global-librarian-mode)
  (if global-librarian-mode
      (progn ;; activating
        (global-librarian-regular-minor-mode 1)
        (global-librarian-tagging-mode 1)
        (librarian--browse-load-variants)
        (unless (not (and (boundp 'librarian-configs--modules-cache) librarian--config-modules-cache))
          (librarian--config--build-modules-cache))

        (setq xref-show-definitions-function #'ivy-xref-show-defs
              xref-show-xrefs-function       #'ivy-xref-show-xrefs
              browse-url-browser-function    #'librarian--browse-open-url
              browse-url-handlers nil
              browse-url-default-handlers '(
                                            ("." . librarian--browse-open-url)
                                            )
              )
        (advice-add #'ivy-xref-show-xrefs :around #'librarian--util-fix-ivy-xrefs)
        )
    (progn ;; deactivating
      (global-librarian-regular-minor-mode -1)
      (global-librarian-tagging-mode -1)
      (setq browse-url-browser-function #'browse-url-default-browser)
      (advice-remove #'ivy-xref-show-xrefs #'librarian--util-fix-ivy-xrefs)
      )
    )
  )

;;;###autoload
(defun librarian-debug ()
  " Check librarian settings:
   documentation function assignments,
   assigned browser,
   installed docsets
   registered url handlers
   "
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((handlers (cl-loop for prop in librarian--doc-valid-keywords
                             for fns = (plist-get
                                        librarian--doc-handlers-plist
                                        prop)
                             collect
                             (format "%-25s: %s" prop fns)
                             ))
          )
      (message (format "Lookup Handlers for \"%s\" are: \n%s"
                       (buffer-name)
                       (string-join handlers "\n")))
      )
    )
)

;;-- public aliases

;;;###autoload
(defvaralias 'librarian-biblio-buffer      'librarian--biblio-meta-buffer)

;;;###autoload
(defvaralias 'librarian-biblio-program     'librarian--biblio-meta-program)

;;;###autoload
(defvaralias 'librarian-bibio-args         'librarian--biblio-meta-opts)

;;;###autoload
(defvaralias 'librarian-biblio-library-loc 'librarian--biblio-library-loc)

;;;###autoload
(defvaralias 'librarian-biblio-pdf-loc     'librarian--biblio-pdf-loc)

;;;###autoload
(defvaralias 'librarian-biblio-unsourced-loc 'librarian--biblio-unsourced-bib-file)

;;;###autoload
(defalias 'librarian-regular-clear!                  #'librarian--regular-clear)

;;;###autoload
(defalias 'librarian-regular-go!                     #'librarian--regular-go)

;;;###autoload
(defalias 'librarian-insert-minor-mode               #'librarian--insert-minor-mode)

;;;###autoload
(defalias 'librarian-insert-register-processor       #'librarian--insert-register-processor)

;;;###autoload
(defalias 'librarian-insert-trigger                  #'librarian--insert-trigger)

;;;###autoload
(defalias 'librarian-insert-clear-caches             #'librarian--insert-clear-caches)

;;;###autoload
(defalias 'librarian-biblio-build-file-list          #'librarian--bibio-build-list)

;;;###autoload
(defalias 'librarian-biblio-get-meta                 #'librarian--biblio-meta-retrieval)

;;;###autoload
(defalias 'librarian-biblio-set-cover                #'librarian--biblio-set-ebook-cover)

;;;###autoload
(defalias 'librarian-biblio-update-entry-from-doi    #'librarian--biblio-update-entry)

;;;###autoload
(defalias 'librarian-biblio-create-from-doi          #'librarian--biblio-insert-entry-from-doi)

;;;###autoload
(defalias 'librarian-biblio-refile-to-canonical      #'librarian--biblio-refile-by-year)

;;;###autoload
(defalias 'librarian-biblio-refile-to-unsourced      #'librarian--biblio-refile-to-unsourced)

;;;###autoload
(defalias 'librarian-biblio-refile-to-other-window   #'librarian--biblio-refile-to-other-window)

;;;###autoload
(defalias 'librarian-docsets-install                 #'counsel-dash-install-docset)

;;-- end public aliases

(provide 'librarian)
;;; librarian.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("lib-" . "librarian-")
;; )
;; End:
