#lang racket

(require webapp/scripts/generate/util)

(module+ main
  (define args
    (vector->list (current-command-line-arguments)))

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

  (require mc-data/scripts/migrate)

  (create-migration name
                    (thunk
                      (fix-migration name fields)))) 

