#lang racket

(provide destroy)

(require webapp/scripts/generate/util)
(require webapp/scripts/migrate)

(define (destroy . args)
  (when (not (empty? (rest args)))
    (raise "There should be no field args.  Just supply a model name to destroy."))

  (define name   (first args))

  (maybe-delete-file (model-base.rkt name) )
  (maybe-delete-file (model-main.rkt name) )
  (maybe-delete-dir (model-dir name))

  (maybe-delete-file (test-main.rkt name) )
  (maybe-delete-dir (test-dir name))


  (delete-migration name))

