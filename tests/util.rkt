#lang racket

(provide tests
         (all-from-out 
           rackunit
           webapp/db/util  
           webapp/models/util  
           ))

(require rackunit) 
(require webapp/logs/util
         webapp/environment/util 
         webapp/db/util
         webapp/models/util)

(load-current-env!)

(define-syntax-rule (tests exp ...)
  (module+ test
    (parameterize ([env "test"])
      ;Recreate the test db every time a suite of tests runs.  Makes sure that tests don't interfer with each other.
      (displayln-color 'blue "DROPPING DB")
      (drop-db)
      (displayln-color 'blue "CREATING DB")
      (create-db)
      (displayln-color 'blue "SEEDING DB")
      (seed-db)
      (displayln-color 'blue "RUNNING TESTS")
      exp ...      
      (void)
    )))
