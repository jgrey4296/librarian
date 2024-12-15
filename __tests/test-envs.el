;;; tests.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

;; should should-error should-not
(require 'ert)

(defun tests-librarian-envs-setup ()
  (add-to-list 'load-path (f-parent default-directory))
  (load "librarian-envs")
)

(ert-deftest tests-sanity-test ()
  "Initial Sanity Test"
  :tags '(sanity)
  ;; (tests-librarian-envs-setup)
  (should (equal (pp-to-string '(quote quote)) "'quote"))
  (should (equal (pp-to-string '((quote a) (quote b))) "('a 'b)\n"))
  (should (equal (pp-to-string '('a 'b)) "('a 'b)\n"))
  )

(ert-deftest test-envs-structs-test ()
  "Tests "
  :tags '(structs)
  (should (librarian-envs-handler-p (make-librarian-envs-handler)))
  (should (eq (librarian-envs-handler-id (make-librarian-envs-handler :id 'test)) 'test))

  (should (librarian-envs-state-p (make-librarian-envs-state)))

  (should (librarian-envs-loc-p (make-librarian-envs-loc)))
)

(ert-deftest test-envs-modify-locs-test ()
  "Tests "
  :tags '(structs locs modify)
  (let ((locs (make-librarian-envs-loc)))
    (should (librarian-envs-loc-p locs))
    (should (not (librarian-envs-loc-marker locs)))
    (setf (librarian-envs-loc-marker locs) "blah")
    (should (equal (librarian-envs-loc-marker locs) "blah"))

    )
)

(ert-deftest test-envs-register-handler-test ()
  "Tests handler registration  "
  :tags '(registration structs)
  (clrhash librarian-envs--registered)
  (should-not (gethash 'from-plist librarian-envs--registered))
  (should-not (gethash 'from-struct librarian-envs--registered))
  (librarian-envs-register :id 'from-plist :lang 'py)
  (librarian-envs-register (make-librarian-envs-handler :id 'from-struct :lang 'lisp))
  (should (gethash 'from-plist librarian-envs--registered))
  (should (gethash 'from-struct librarian-envs--registered))

)


;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    December 14, 2024
;; Modified:   December 14, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; tests.el ends here
