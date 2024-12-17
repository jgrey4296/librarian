;;; test-biblio-bc.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header
(rqure 'buttercup)

(describe "title case"
  (it "should behave by some rules"
    (expect (lib-title-case "this is a test") :to-be "This Is A Test" )
    (expect (lib-title-case "This Is A Test") :to-be "This Is A Test" )
    (expect (lib-title-case "Why Aren’t We Talking About Trump’s Fascism?") :to-be "Why Aren’t We Talking About Trump’s Fascism?" )
    (expect (lib-title-case "Why Aren't We Talking About Trump’s Fascism?") :to-be "Why Aren't We Talking About Trump’s Fascism?" )
    (expect (lib-title-case "Why Aren't 'We blah' Talking About Trump’s Fascism?") :to-be "Why Aren't 'we Blah' Talking About Trump’s Fascism?" )jk:W
    )
  )



;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    December 17, 2024
;; Modified:   December 17, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("lib-" . "librarian--biblio-util-")
;; )
;; End:
;;; test-biblio-bc.el ends here
