#lang at-exp racket

(provide create-db
         seed-db
         drop-db)

(require webapp/environment/util
         webapp/models/util
         db
         english)


(define (seed-db)
  (load-current-env!)
  (define insert-seeds! 
    (dynamic-require (string->symbol 
                       (~a (pkg-name) 
                           "/db/seeds")) 'insert-seeds!))
  (insert-seeds!)
  (void))

(define (create-db)
  (load-current-env!)
  (local-require webapp/scripts/migrate)

  (run-migrations))

(define (drop-db)
 (load-current-env!)
 (define (racket->sql s)
   (string-replace (~a s) "-" "_"))

 (for ([t (all-models-plural)])
  (define to-drop
   (racket->sql t))
  (displayln (~a "DROPPING " to-drop))
  (query-exec (conn) (~a "DROP TABLE IF EXISTS " to-drop)))

 (query-exec (conn) "DROP TABLE IF EXISTS north_schema_version"))

