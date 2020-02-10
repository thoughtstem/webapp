#lang at-exp racket

(provide 
  maybe-create-dir
  maybe-create-file
  maybe-delete-dir
  maybe-delete-file

  model-file-template
  model-base-file-template

  type->db-type

  migration-name
  test-main.rkt
  test-dir
  test-main.rkt
  test-dir
  model-main.rkt
  model-base.rkt
  model-dir

  fix-migration


  test-file-template)

(require english)
(require file/glob)
(require webapp/logs/util
         webapp/environment/util)

(define (doc-main.rkt model-name)
  (build-path "scribblings" "models" (plural model-name) "main.rkt"))

(define (doc-dir model-name)
  (build-path "scribblings" "models" (plural model-name)))

(define (test-main.rkt model-name)
  (build-path "tests" (plural model-name) "main.rkt"))

(define (test-dir model-name)
  (build-path "tests" (plural model-name)))

(define (model-main.rkt model-name)
  (build-path "models" (plural model-name) "main.rkt"))

(define (model-base.rkt model-name)
  (build-path "models" (plural model-name) "base.rkt"))

(define (model-dir model-name)
  (build-path "models" (plural model-name)))

(define (maybe-delete-dir path)
  (displayln-color 'yellow 
                   (~a "Delete dir? " path))
  (if (not (directory-exists? path))
    (displayln-color 'yellow   "  Already gone, skipping")  
    (begin
      (delete-directory/files path)
      (displayln-color 'red "  Deleted"))))

(define (maybe-create-dir path)
  (displayln-color 'blue 
                   (~a "Create dir? " path))
  (if (directory-exists? path)
    (displayln-color 'yellow   "  Already created, skipping")  
    (begin
      (make-directory* path)
      (displayln-color 'green "  Created"))))

(define (maybe-create-file path content)
  (displayln-color 'blue 
                   (~a "Create file? " path))
  (if (file-exists? path)
    (displayln-color 'yellow   "  Already created, skipping")  
    (begin
      (with-output-to-file path
                           (thunk 
                             (displayln content)))
      (displayln-color 'green "  Created") )))

(define (maybe-delete-file path)
  (displayln-color 'yellow 
                   (~a "Delete file? " path))
  (if (not (file-exists? path))
    (displayln-color 'yellow   "  Already gone, skipping")  
    (begin
      (delete-file path)
      (displayln-color 'red "  Deleted"))))

(define (model-file-template name fields)
  @~a{
    #lang racket
    (provide (all-from-out @|(pkg-name)|/models/@|(plural name)|/base))
    (require webapp/models/util
             @|(pkg-name)|/models/base
             @|(pkg-name)|/models/@|(plural name)|/base   )

    ;TODO: Put your higher-level multi-model logic here.  
  })

(define (model-base-file-template name fields)
  (define (field->deta f)
    (define name (first f)) 
    (define type (second f)) 

    @~a{
      [@name @|type|/f] 
    })

  @~a{
    #lang racket

    (provide (schema-out @name))

    (require webapp/models/util)

    (define-schema @name
                   ([id id/f #:primary-key #:auto-increment]
                    @(string-join 
                       (map field->deta fields)
                       "\n")))
  })

(define (test-file-template name fields)
  @~a{
    #lang racket

    (require webapp/tests/util)

    (tests 
      (check-pred
        (negate empty?)
        (all @name)
        "There are no @name records in the database.  Add some seeds or fix this test."))
  })


(define (doc-file-template name fields)
  @~a{
    #lang racket

    (require webapp/tests/util)

    (tests 
      (seed-db) 

      (check-pred
        (negate empty?)
        (all @name)))
  })


(define (type->db-type t)
  (match t
    ["integer" "integer"] 
    ["id" "integer"] 
    ["string" "text"] 
    ["datetime-tz" "timestamptz"] 
    [else (raise (~a "Unsupported type: " t))]))


(define (sqlify s)
  (string-replace
    s
    "-"
    "_"))

(define (up-migration name fields)
  (define (sql-line f)
    (define field-name (first f)) 
    (define type (second f)) 

    @~a{
    @(sqlify field-name) @(type->db-type type)
    })

  @~a{
  create table @(sqlify (plural name))(
         id serial primary key, 
         @(string-join (map sql-line fields) ",\n") 
     );
  })

(define (fix-migration name fields)
  (define m-name (migration-name name))

  (define f (first (glob (~a "migrations/*" m-name ".sql"))))
  (define ss (file->lines f))

  (define header-lines (take ss 5))

  (define lines (append header-lines
                        (list
                          "-- @up {" 
                          (up-migration name fields)
                          "-- }" 

                          "-- @down {" 
                          (~a "drop table " (sqlify (plural name)) ";")
                          "-- }" 
                          )))

  (define new-s
    (string-join lines "\n"))

  (displayln new-s)

  (with-output-to-file f #:exists 'replace
    (thunk
      (displayln new-s))))

(define (migration-name name)
  (~a "add-" (plural name) "-table"))

