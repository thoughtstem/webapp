#lang info
(define collection "webapp")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/webapp.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(thoughtstem))
(define raco-commands
  '(("webapp" webapp/scripts/main "Invokes commands related to webapp" 100)))
