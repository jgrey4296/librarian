;;; test-envs-bc.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

;; describe, it, expect, before-each
(describe "sanity:"
  (it "is the simplest test" (expect t :to-be (not nil)))
  )

(describe "handlers:"
  ;; Vars:
  :var ((hand (make-lenv-handler :id 'test)))
  ;; Specs:
  (it "is a sanity test"    (expect t :to-be (not nil)))
  (it "can check the type"  (should (lenv-handler-p hand)))
  (it "should have an id"   (expect (lenv-handler-id hand) :to-be 'test))
)

(describe "state:"
  ;; Vars:
  :var ((state (make-lenv-state :id 'test)))
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "can check the type" (should (lenv-state-p state)))
  (it "should have an id" (expect (lenv-state-id state) :to-be 'test))
)

(describe "loc:"
  ;; Vars:
  :var ((loc (make-lenv-loc :root "blah" :marker "bloo")))
  ;; Specs:
  (it "is a sanity test"     (expect t :to-be (not nil)))
  (it "can check the type"   (expect (lenv-loc-p loc)) :to-be t)
  (it "should have a root"   (expect (lenv-loc-root loc) :to-equal "blah"))
  (it "should have a marker" (expect (lenv-loc-marker loc) :to-equal "bloo"))
  (it "should be auto-created" (expect (lenv-loc-p (lenv--init-loc)) :to-be t))

)

(describe "handler registration:"
  ;; Vars:
  :var (
        (hand (make-lenv-handler :id 'from-struct :lang 'lisp))
        (plist '(:id from-plist :lang py))
        )
  ;; Setup
  (before-each (lenv-clear-registry))
  ;; Teardown
  (after-all (lenv-clear-registry))
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "can register from a plist"
    (expect (gethash 'from-plist lenv--registered) :to-be nil)
    (apply #'lenv-register plist)
    (expect (gethash 'from-plist lenv--registered) :not :to-be nil)
    )
  (it "can register from a handler"
    (expect (gethash 'from-struct lenv--registered) :to-be nil)
    (lenv-register hand)
    (expect (gethash 'from-struct lenv--registered) :not :to-be nil)
    )
  (it "can get a registered handler by string id"
    (lenv-register hand)
    (expect (lenv--get-handler "from-struct") :to-be hand))
  (it "can get a registered handler by symbol id"
    (lenv-register hand)
    (expect (lenv--get-handler 'from-struct) :to-be hand))
  (it "will return nill on getting a non-existing handler"
    (expect (lenv--get-handler 'blah) :to-be nil))
)

(describe "handler activation:"
  :var ((hand (make-lenv-handler :id 'test))
        (loc  (make-lenv-loc :root default-directory :marker lenv-marker))
        )
  (before-each (lenv-clear-registry)
               (lenv-register hand)
               )
  (it "should add a state obj on activation"
    (expect (hash-table-empty-p lenv-active) :to-be t)
    (lenv--activate-handler 'test loc)
    (expect (hash-table-empty-p lenv-active) :to-be nil)
    )
  (it "should return existing handler on re-activate"
    (expect (hash-table-empty-p lenv-active) :to-be t)
    (let ((state (lenv--activate-handler 'test loc)))
      (expect (hash-table-empty-p lenv-active) :to-be nil)
      (expect (lenv--activate-handler 'test loc) :to-be state)
      )
    )
  (it "can get an activated handler state"
    (let ((state (lenv--activate-handler 'test loc)))
      (expect (lenv--get-state 'test) :to-be state)
      (expect (lenv--get-state "test") :to-be state)
      )
    )
  )

(describe "marker parsing:"
  :var ((loc (make-lenv-loc :root default-directory :marker lenv-marker)))
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "can expand the marker path"
    (expect (lenv--expand-marker loc) :to-equal (f-join default-directory lenv-marker))
    (should (f-exists? (lenv--expand-marker loc)))
    )
  (it "can parse the marker"
    (expect (lenv--parse-marker loc) :to-equal '(("mamba" "doot-dev312") ("lsp") ("py-lsp" "t")))
    )
)

(describe "locking:"
  :var ((hand1 (make-lenv-handler :id 'blah))
        (hand2 (make-lenv-handler :id 'bloo))
        (loc  (make-lenv-loc :root default-directory :marker lenv-marker))
        )
  (before-all ;; add some handlers to the active list
    (lenv-clear-registry)
    (lenv-register hand1)
    (lenv-register hand2)
    (lenv--activate-handler 'blah loc)
    (lenv--activate-handler 'bloo loc)
    )
  (before-each
    (setf (lenv-state-locked (lenv--get-state 'blah)) nil
          (lenv-state-locked (lenv--get-state 'bloo)) nil
          )
    )
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "can toggle a handler"
    (expect (lenv-state-locked (lenv--get-state 'blah)) :to-be nil)
    (lenv-toggle-lock! 'blah)
    (expect (lenv-state-locked (lenv--get-state 'blah)) :to-be t)
    (lenv-toggle-lock! 'blah)
    (expect (lenv-state-locked (lenv--get-state 'blah)) :to-be nil)
    )
  (it "can toggle multiple handlers"
    (expect (lenv-state-locked (lenv--get-state 'blah)) :to-be nil)
    (expect (lenv-state-locked (lenv--get-state 'bloo)) :to-be nil)
    (lenv-toggle-lock! 'blah)
    (expect (lenv-state-locked (lenv--get-state 'blah)) :to-be t)
    (expect (lenv-state-locked (lenv--get-state 'bloo)) :to-be nil)
    (lenv-toggle-lock! 'blah 'bloo)
    (expect (lenv-state-locked (lenv--get-state 'blah)) :to-be nil)
    (expect (lenv-state-locked (lenv--get-state 'bloo)) :to-be t)
    (lenv-toggle-lock! 'blah 'bloo)
    (expect (lenv-state-locked (lenv--get-state 'blah)) :to-be t)
    (expect (lenv-state-locked (lenv--get-state 'bloo)) :to-be nil)

    )
)

(describe "setup/start:"
  ;; Vars:
  :var ((hand1 (make-lenv-handler :id 'blah))
        (hand2 (make-lenv-handler :id 'bloo))
        )
  ;; Setup
  (before-each
    (lenv-clear-registry)
    (lenv-register hand1)
    (lenv-register hand2)
    )
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "basic state starting"
    (let ((states (lenv-start! nil "blah")))
      (expect (length states) :to-equal 1)
      (expect (lenv-state-status (car states)) :to-be 'active)
      )
    )
  (it "multi state starting"
    (let ((states (lenv-start! nil "blah" "bloo")))
      (expect (length states) :to-equal 2)
      (cl-loop for state in states
               do
               (expect (lenv-state-status state) :to-be 'active)
               )
      )
    )
  (it "only starts unlocked states"
    (lenv-start! nil 'blah)
    (lenv-toggle-lock! 'blah)
    (expect (length (lenv-start! nil 'blah 'bloo)) :to-equal 1)
    )
  (it "only starts non-started states"
    (lenv-start! nil 'blah)
    (expect (length (lenv-start! nil 'blah 'bloo)) :to-equal 1)
    )
  (it "adds params to state data"
    (let* ((states (lenv-start! nil '(blah bloo aweg)))
           (data (lenv-state-data (car states)))
           )
      (expect (length states) :to-equal 1)
      (expect (length data) :to-equal 2)
      (expect data :to-equal '(bloo aweg))
      )
    )
  )

(describe "setup/start hooks:"
  ;; Vars:
  :var (hand setup start)
  ;; Setup
  (before-each
    (lenv-clear-registry)
    (setf (symbol-function 'setup) #'(lambda (&rest data) nil)
          (symbol-function 'start) #'(lambda (&rest data) nil)
          hand (make-lenv-handler :id 'blah :setup #'setup :start #'start)
          )
    (spy-on 'setup)
    (spy-on 'start)
    (lenv-register hand)
    )
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "callbacks setup/start"
    (expect 'setup :not :to-have-been-called)
    (expect 'start :not :to-have-been-called)
    (lenv-start! nil 'blah)
    (expect 'setup :to-have-been-called)
    (expect 'start :to-have-been-called)
    )

)

(describe "stop/teardown:"
  ;; vars
  :var ((loc  (make-lenv-loc :root default-directory :marker lenv-marker))
        (hand1 (make-lenv-handler :id 'blah))
        (hand2 (make-lenv-handler :id 'bloo))
        )
  ;; Setup
  (before-each
    (lenv-clear-registry)
    (lenv-register hand1)
    (lenv-register hand2)
    (lenv--activate-handler 'blah loc)
    (lenv--activate-handler 'bloo loc)
    )
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "can stop handlers"
    (expect (lenv--get-state 'blah) :not :to-be nil)
    (expect (lenv-stop! nil 'blah) :not :to-be nil)
    )
)

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    December 15, 2024
;; Modified:   December 15, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; test-envs-bc.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("lenv-" . "librarian-envs-")
;; ("make-lenv-" . "make-librarian-envs-")
;; )
;; End:
