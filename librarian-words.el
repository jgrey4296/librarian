;; -*- mode:emacs-lisp; -*- lexical-bindings: t; -*-

;;
;;; Dictionary

(defun librarian-dictionary-definition (identifier &optional arg)
  "Look up the definition of the word at point (or selection)."
  (interactive
   (list (or (librarian-get 'word)
             (read-string "Look up in dictionary: "))
         current-prefix-arg))
  (message "Looking up dictionary definition for %S" identifier)
  (cond ((and(eq system-type 'darwin) (fboundp 'osx-dictionary--view-result))
         (osx-dictionary--view-result identifier))
        ((and librarian-dictionary-prefer-offline (fboundp 'wordnut-search))
         (wordnut-search identifier))
        ((fboundp 'define-word)
         (define-word identifier nil arg))
        ((user-error "No dictionary backend is available"))))

(defun librarian-synonyms (identifier &optional _arg)
  "Look up and insert a synonym for the word at point (or selection)."
  (interactive
   (list (librarian-get 'word) ; TODO actually use this
         current-prefix-arg))

  (message "Looking up synonyms for %S" identifier)
  (cond ((and librarian-dictionary-prefer-offline (fboundp 'synosaurus-choose-and-replace))
         (synosaurus-choose-and-replace))
        ((fboundp 'powerthesaurus-lookup-word-dwim)
         (powerthesaurus-lookup-word-dwim))
        ((user-error "No thesaurus backend is available")))
  )
