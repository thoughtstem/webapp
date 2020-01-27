#lang racket
;Here we wrap up deta's functions,
;  Partially to avoid passing the db conneciton around.
;  Partially to future-proof things (who knows if we will keep using deta)
(provide (rename-out 
           [my-define-schema define-schema]
           [my-insert-one! insert-one!]
           [my-delete-one! delete-one!]
           [my-update-one! update-one!]
           [my-update! update!]
           [my-delete! delete!])

         (struct-out model-error)

         (except-out (all-from-out deta)
                     define-schema
                     insert-one!
                     delete-one!
                     update-one!
                     update!
                     delete!)

         has-many
         has-one
         belongs-to

         all
         find-by-id
         get
         get-fields
         get-values
         get-type
         all-models-plural
         
         define-seed)

(require webapp/environment/util
         (for-syntax english
                     webapp/environment/util)
         deta
         threading
         (for-syntax racket/syntax
                     racket/format)
         syntax/parse/define
         file/glob
         (only-in db query-exec))

(define (all-models-plural)
  ;If we want a singular version, we should probably track them as define-schema is used,
  ;  rather than assuming that the file structure is some kind of source of truth (even though is sort of is for now)

  (define model-files (glob (~a "/" (pkg-name) "/models/*/base.rkt")) )
  (map (compose 
         string->symbol
         (lambda (f)
           (list-ref (string-split (~a f) "/") 2))) 
       model-files))

(struct model-error (e) #:transparent)

(define-syntax-rule (catch-model-errors lines ...)
  (with-handlers ([exn:fail?
                    (lambda (e)
                      (model-error e))])
                 lines ...))

(define (my-insert-one! model)
  (catch-model-errors
    (insert-one! (conn) model)))

(define (my-update! . models)
  (apply update-one! (conn) models))

(define (my-update-one! model)
  (catch-model-errors
    (update-one! (conn) model))   )

(define (my-delete! . model)
  (catch-model-errors
    (apply delete! (conn) model)))

(define (my-delete-one! model)
  (catch-model-errors
    (delete-one! (conn) model)))

(define-syntax (has-one stx)
  (syntax-parse stx
    [(_ from to)
     (define from->to
       (format-id #'from
                  "~a->~a" 
                  (syntax->datum #'from)
                  (syntax->datum #'to)))

     (define to-name
       (~a (syntax->datum #'to)))

     (define x.from_id
       (format-id #'from
                  "x.~a_id" 
                  (syntax->datum #'from)))

     (define from-id
       (format-id #'from
                  "~a-id" 
                  (syntax->datum #'from)))

     (define to-schema
       (format-id #'to
                  "~a-schema" 
                  (syntax->datum #'to)))

     #`(begin
         (provide #,from->to)
         (define (#,from->to model)
           (define to-schema
             (dynamic-find-base-function '#,to-schema))

           (define s
             (in-entities
               (conn)
               (~>
                 (#,'from #,(sqlify (plural to-name)) #:as x)
                 (where (= #,x.from_id
                           ,(#,from-id model)))
                 (limit 1)
                 (project-onto to-schema))))

           (define l (sequence->list s))
           
           (if (empty? l)
             #f
             (first l))

           ))]))

(define-syntax (has-many stx)
  (syntax-parse stx
    [(_ from to)
     (define from->tos
       (format-id #'from
                  "~a->~a" 
                  (syntax->datum #'from)
                  (plural (syntax->datum #'to))))
     (define to-name
       (~a (syntax->datum #'to)))

     (define x.from_id
       (format-id #'from
                  "x.~a_id" 
                  (syntax->datum #'from)))

     (define from-id
       (format-id #'from
                  "~a-id" 
                  (syntax->datum #'from)))

     (define to-schema
       (format-id #'to
                  "~a-schema" 
                  (syntax->datum #'to)))

     #`(begin
         (provide #,from->tos)
         (define (#,from->tos model)
           (define to-schema
             (dynamic-find-base-function
               '#,to-schema))
           (define s
             (in-entities
               (conn)
               (~>
                 (#,'from #,(sqlify (plural to-name)) #:as x)
                 (where (= #,x.from_id
                           ,(#,from-id model)))
                 (project-onto to-schema))))

           (sequence->list s)

           ))]))

(define (dynamic-find-base-function f-name)
  (dynamic-require
    (string->symbol 
      (~a (pkg-name) "/models/base"))
    f-name))

(define-syntax (belongs-to stx)
  (syntax-parse stx
    [(_ from to)
     (define to-name
       (~a (syntax->datum #'to)))

     (define from->to
       (format-id #'from
                  "~a->~a" 
                  (syntax->datum #'from)
                  (syntax->datum #'to)))

     (define to-id
       (format-id #'to
                  "~a-~a-id" 
                  (syntax->datum #'from)
                  (syntax->datum #'to)))

     (define to-schema
       (format-id #'to
                  "~a-schema" 
                  (syntax->datum #'to)))

     (define from-module
       (format-id #'to
                  (~a (pkg-name) "/models/~a/base")
                  (plural (syntax->datum #'from))))


     #`(begin
         (provide #,from->to)
         (define (#,from->to model)
           (define to-schema
             (dynamic-find-base-function '#,to-schema))
           (define to-id
             (dynamic-find-base-function
               '#,to-id))
           (define s
             (in-entities
               (conn)
               (~>
                 (#,'from #,(sqlify (plural to-name)) #:as x)
                 (where (= x.id
                           ,(to-id model)))
                 (project-onto to-schema))))

           (first (sequence->list s))

           ))]))

(define-for-syntax (sqlify s)
  (local-require racket/string)
  (string-replace (~a s) "-" "_"))

(define-syntax (my-define-schema stx)
  (syntax-parse stx
    [(_ name ([field-name field-things ...] ...))  

     (define name-fields
       (format-id #'name "~a-fields"
                  (syntax-e #'name)))

     (define plural-name
       (plural (syntax->datum #'name)))

     (define all-names
       (format-id #'name "all-~a"
                  plural-name))

     (define (create-finders f)
       (define find-Xs-by-F
         (format-id #'name
                    "find-~a-by-~a"
                    plural-name
                    f))

       (define find-X-by-F
         (format-id #'name
                    "find-~a-by-~a"
                    #'name
                    f))

       (define x.field
         (format-id #'name
                    "x.~a"
                    f))

       (define name-schema
         (format-id #'name
                    "~a-schema"
                    #'name))

       #`(begin
           (provide #,find-Xs-by-F
                    #,find-X-by-F) 

           (define (#,find-Xs-by-F v)
             (define s
               (in-entities
                 (conn)
                 (~>
                   (#,'from name #:as x)
                   (where (= #,x.field ,v))
                   (project-onto #,name-schema))))


             (sequence->list s))
           
           (define (#,find-X-by-F v)
             (define s
               (in-entities
                 (conn)
                 (~>
                   (#,'from name #:as x)
                   (where (= #,x.field ,v))
                   (limit 1)
                   (project-onto #,name-schema))))


             (define l (sequence->list s)) 
             (if (empty? l) #f (first l)))
           ))

     #`(begin
         (define-schema name
                        #:table #,(sqlify plural-name)
                        ([field-name field-things ...] ...)) 

         (module+ schema-info
           (provide schema-info) 

           (define schema-info
             '(name
                ([field-name field-things ...] ...))))

         (provide #,name-fields)

         (define (#,name-fields)
           '(field-name ...))
         

         (provide #,all-names)

         (define (#,all-names)
           (all name))

         #,@(map create-finders (syntax->datum #'(field-name ...)))

         )]))

(define (get model field-name)
  (define type (get-type model))
  (define f (dynamic-require (string->symbol 
                               (~a (pkg-name) "/models")) 
                             (string->symbol (~a type "-" field-name))))

  (f model))

(define (get-type model)
  (string->symbol
    (second
      (string-split 
        (symbol->string
          (vector-ref 
            (struct->vector model)
            0)) 
        ":"))))

(define (get-fields model)
  (define f 
    (dynamic-require
      (string->symbol (~a (pkg-name) "/models"))
      (string->symbol
        (~a (get-type model) "-fields"))))

  (f))

(define (get-values model)
  (map (curry get model) 
       (get-fields model)))

(define-syntax-rule (all model)
  (for/list 
    ([c (in-entities (conn)
                     (~> 
                       (from model #:as c)))])

    c))

(define-syntax-rule (find-by-id model i)
  (first
    (for/list 
      ([c (in-entities (conn)
                       (~> 
                         (from model #:as c)
                         (where (= c.id ,i))))])

      c)))

(define-syntax (define-seed stx)
  (syntax-parse stx
    [(_ name lookup def)
     #`(begin
         (provide name)
         (define-syntax (name stx) 
           (syntax-parse stx
             [val:identifier
               #'(or lookup (my-insert-one! def))])))
     ]))


