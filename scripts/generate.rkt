#lang racket

(provide generate)

(require webapp/scripts/generate/util)
(require webapp/scripts/migrate)

(define (generate . args)
  (when (empty? args)
    (raise "You must supply a model name and list of fields"))

  (define name   (first args))
  (define fields (map (curryr string-split ":") (rest args)))

  ;Just to trigger an error if user passes in an unsupported type
  (map seed-value-of-type (map second fields))

  (maybe-create-dir (model-dir name))

  (maybe-create-file (model-base.rkt name) 
                     (model-base-file-template name fields))

  (maybe-create-file (model-main.rkt name) 
                     (model-file-template name fields))

  (maybe-create-dir (test-dir name))
  (maybe-create-file (test-main.rkt name) 
                     (test-file-template name fields))

  (create-migration name
                    (thunk
                      (fix-migration name fields)))) 

