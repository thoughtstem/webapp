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

	 cache-log
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
         
         define-seed
	 get-relations)

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

(define (dynamic-find-base-function f-name)
  ;A better approach in general would be to make
  ;  has-many, has-one, and belongs-to all
  ;  register their model files in some lookup table so that we can dynamically find them later -- and we don't have to assume all models are in the main project.
  (dynamic-require
    (string->symbol 
      (~a (pkg-name) "/models/base"))
    f-name))

(define (get-relations module-base-path)
  (define schema-info-module
    `(submod ,module-base-path schema-info))

  (dynamic-require schema-info-module #f)

  (define-values (functions macros)
    (module->exports schema-info-module))

  (define relation-names
    (filter-not (curry eq? 'schema-info)
		(map first
		     (rest
		       (first functions)))))

  (apply hash
	 (flatten
	   (map 
	     (lambda (n)
	       (list n (dynamic-require schema-info-module n)))
	     relation-names)))
  )

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

(define cache-log (make-parameter #f))
(define query-cache (make-parameter (make-hash)))

(define (reset-query-cache)
  (query-cache (make-hash)))

(define (log . msg)
  #;
  (when (query-cache) ;SHould make an explicit logging param when we want to provide this feature...
        (pretty-print msg))
  (void)
  )

(define (query->hash-key query-type input-model)
  (list query-type
	(if (entity? input-model)
	    (get input-model 'id)
	    input-model ;So it can be a number, string, etc.  
	    )))

(define (get-from-cache query-type input-model)
  (if (not (query-cache))
      #f
      (begin
	(when (cache-log)
	  (displayln (list "Got from cache!" (query->hash-key query-type input-model))))
	(hash-ref (query-cache) 
		  (query->hash-key query-type input-model)
		  #f))))

(define-syntax-rule (try-cache [query-type input-model] 
			       statements ...)

  (let ([cache-value (get-from-cache query-type input-model)])
    (or cache-value
        (let ()
	  (when (cache-log)
	    (displayln (list "Cache miss" (query->hash-key query-type input-model))))

	  (define new-value-to-cache
	    (let () statements ...))
	  
	  (when (query-cache)
	    (hash-set! (query-cache)
		       (query->hash-key query-type input-model)
		       new-value-to-cache))
	 
	  new-value-to-cache
	  ))))


(define (my-insert-one! model)
  (log 'insert-one!)
  (reset-query-cache)
  (catch-model-errors
    (insert-one! (conn) model)))

(define (my-update! . models)
  (log 'update!)
  (reset-query-cache)
  (apply update-one! (conn) models))

(define (my-update-one! model)
  (log 'update-one!)
  (reset-query-cache)
  (catch-model-errors
    (update-one! (conn) model))   )

(define (my-delete! . model)
  (log 'delete!)
  (reset-query-cache)
  (catch-model-errors
    (apply delete! (conn) model)))

(define (my-delete-one! model)
  (log 'delete-one!)
  (reset-query-cache)
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
           (log '#,from->to model)

	   (try-cache ['#,from->to model]

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
		 (first l)))
	   
	   )
	 
	 (module+ schema-info
		  (provide #,from->to) 
		  (define #,from->to 'has-one))
	 )]))

(define-syntax (has-many stx)
  (syntax-parse stx
    [(_ from to additional-db-constraints ...)
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
	   (log '#,from->tos model)

	   (try-cache ['#,from->tos model]
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
		    ;Additional constraints?
		    additional-db-constraints ...
		    (project-onto to-schema))))

	      (sequence->list s)))

	 (module+ schema-info
		  (provide #,from->tos) 
		  (define #,from->tos 'has-many))
	 )]))

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
	   (log '#,from->to model)

	   (try-cache ['#,from->to model]
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

		      (define l
			(sequence->list s))  
		      (if (empty? l)
			  #f
			  (first l)) ))
	 
	 (module+ schema-info
		  (provide #,from->to) 
		  (define #,from->to 'belongs-to))
	 )]))

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

     (define search-for-names-base
       (format-id #'name "search-for-~a-base"
		  plural-name))

     (define search-for-names
       (format-id #'name "search-for-~a"
		  plural-name))

     (define reload-name
       (format-id #'name "reload-~a"
		  (syntax-e #'name)))

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
	     (log '#,find-Xs-by-F v)

	     (try-cache ['#,find-Xs-by-F v]
			(define s
			  (in-entities
			    (conn)
			    (~>
			      (#,'from name #:as x)
			      (where (= #,x.field ,v))
			      (project-onto #,name-schema))))


			(sequence->list s))
	     )
           
           (define (#,find-X-by-F v)
	     (log '#,find-X-by-F v)

	     (try-cache ['#,find-X-by-F v]
			(define s
			  (in-entities
			    (conn)
			    (~>
			      (#,'from name #:as x)
			      (where (= #,x.field ,v))
			      (limit 1)
			      (project-onto #,name-schema))))


			(define l (sequence->list s)) 
			(if (empty? l) #f (first l))))))

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
         
         (provide #,reload-name)

         (define (#,reload-name x)
           (find-by-id name (get x 'id)))

         (provide #,all-names)

         (define (#,all-names)
           (all name))

         (provide #,search-for-names-base)

	 (define-syntax (#,search-for-names-base stx) 
	     (syntax-parse stx
			   [(search-for-names params) 
			    #`(search-for-names params (where (= 0 0)))] 
			   [(search-for-names params other) 
			   #'(let ()
			       (local-require webapp/environment/util 
					      threading)

			       (define the-offset 
				 (hash-ref params 'offset 0))
			       (define the-limit  
				 (hash-ref params 'limit 10))

			       (when (string? the-offset)
				 (set! the-offset 
				   (string->number the-offset)))

			       (when (string? the-limit)
				 (set! the-limit 
				   (string->number the-limit)))

			       (for/list 
				 ([a (in-entities (conn)
						  (~> 
						    (from name #:as a)
						    (offset ,the-offset)
						    (limit ,the-limit)
						    other
						    ))])

						 a))]))


         (provide #,search-for-names)
         (define (#,search-for-names ps)
	   (#,search-for-names-base ps))

         #,@(map create-finders (syntax->datum #'(field-name ...)))

         )]))

(define/contract (get model field-name)
  (-> entity? (or/c string? symbol?) any/c)
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
  (begin
  (log 'all)
  (for/list 
    ([c (in-entities (conn)
                     (~> 
                       (from model #:as c)))])

    c)))


(define-syntax-rule (find-by-id model i)
(begin
  (log 'find-by-id 'model i)
    (first
      (for/list 
        ([c (in-entities (conn)
                         (~> 
                           (from model #:as c)
                           (where (= c.id ,i))))])

        c))))

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


