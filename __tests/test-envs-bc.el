;;; test-envs-bc.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

;; describe, it, expect, before-each
(describe "sanity"
  (it "is the simplest test" (expect t :to-be (not nil)))
  )


(describe "handlers"
  ;; Vars:
  :var ((hand (make-lenv-handler :id 'test)))
  ;; Specs:
  (it "is a sanity test"    (expect t :to-be (not nil)))
  (it "can check the type"  (should (lenv-handler-p hand)))
  (it "should have an id"   (expect (lenv-handler-id hand) :to-be 'test))
)

(describe "state"
  ;; Vars:
  :var ((state (make-lenv-state :id 'test)))
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "can check the type" (should (lenv-state-p state)))
  (it "should have an id" (expect (lenv-state-id state) :to-be 'test))
)

(describe "loc"
  ;; Vars:
  :var ((loc (make-lenv-loc :root "blah" :marker "bloo")))
  ;; Specs:
  (it "is a sanity test"     (expect t :to-be (not nil)))
  (it "can check the type"   (should (lenv-loc-p loc)))
  (it "should have a root"   (expect (lenv-loc-root loc) :to-equal "blah"))
  (it "should have a marker" (expect (lenv-loc-marker loc) :to-equal "bloo"))

)

(describe "handler registration"
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
