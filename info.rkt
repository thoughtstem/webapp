#lang info
(define collection "webapp")
(define deps '("base"
               "deta"
               "north"
               ;"raart"
               "ansi-color"
               "https://github.com/thoughtstem/website.git"
               "https://github.com/thoughtstem/website-js.git"
               "https://github.com/thoughtstem/english.git"
               ))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/webapp.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(thoughtstem))
(define compile-omit-paths
  '("scripts"))
(define raco-commands
  '(("webapp" webapp/scripts/main "Invokes commands related to webapp" 100)))
