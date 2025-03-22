;; -*- mode:emacs-lisp; -*- lexical-bindings: t; -*-

(eval-when-compile
  (require 'synosaurus)
  (require 'wordnut)
  (require 'flyspell)
  (require 'ispell)
  (require 'spell-fu)
  (require 'flyspell-correct)
  (require 'flyspell-correct-ivy)
  (require 'flyspell-lazy)
  (require 'helm-wordnet)

  ;; optional
  (require 'osx-dictionary nil t)
  (require 'define-word nil t)
  (require 'powerthesaurus nil t)
  (require 'company-ispell nil t)
  )

(defvar liw-dictionary-prefer-offline nil)

(defvar liw--lookup-define-fn
  (cond ((and (eq system-type 'darwin) (featurep 'osx-dictionary)) #'osx-dictionary--view-result)
        ((and (featurep 'wordnut) liw-dictionary-prefer-offline) #'wordnut-search)
        ((featurep 'define-word) #'define-word)
        ((featurep 'wordnut) #'wordnut-search)
        (t (lambda (x) (user-error "No dictionary backend is available"))):w
        )
  )
(defvar liw--lookup-define-alt-fn #'helm-wordnet-suggest)

(defvar liw--lookup-synonyms-fn
  (cond ((featurep 'synosaurus) #'synosaurus-choose-and-replace)
        ((featurep 'powerthesaurus) #'powerthesaurus-lookup-word-dwim)
        (t (lambda (x) (user-error "No thesaurus backend is available")))
        )
  )

(defun librarian-words-definition (identifier &optional arg)
  "Look up the definition of the word at point (or selection)."
  (interactive
   (list (or (librarian--util-get 'word)
             (read-string "Look up in dictionary: "))
         current-prefix-arg))
  (message "Looking up dictionary definition for %S" identifier)
  (if arg
      (funcall liw--lookup-define-alt-fn)
    (funcall liw--lookup-define-fn identifier)
    )
  )

(defun librarian-words-synonyms (identifier &optional _arg)
  "Look up and insert a synonym for the word at point (or selection)."
  (interactive
   (list (librarian--util-get 'word) ; TODO actually use this
         current-prefix-arg))
  (message "Looking up synonyms for %S" identifier)
  (funcall liw--lookup-synonyms-fn)
  )

(provide 'librarian--words)
;; Local Variables:
;; read-symbol-shorthands: (
;; ("liw-" . "librarian--words-")
;; )
;; End:
