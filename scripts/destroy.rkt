#lang racket

(require webapp/scripts/generate/util)

(module+ main
  (define args
    (vector->list (current-command-line-arguments)))

  (when (not (empty? (rest args)))
    (raise "There should be no field args.  Just supply a model name to destroy."))

  (define name   (first args))

  (maybe-delete-file (model-base.rkt name) )
  (maybe-delete-file (model-main.rkt name) )
  (maybe-delete-dir (model-dir name))

  (maybe-delete-file (test-main.rkt name) )
  (maybe-delete-dir (test-dir name))

  (require mc-data/scripts/migrate)

  (delete-migration name)
  )

