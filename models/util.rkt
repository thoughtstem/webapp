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
	 with-query-cache
	 reset-query-cache

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

         define-seed

	 with-read-only-database

	 dumping-cached-models

	 (all-from-out 
	   webapp/models/util/reflection))

(require webapp/environment/util
	 webapp/models/util/reflection
	 english
         (for-syntax english
                     webapp/environment/util)
         deta
         threading
         (for-syntax racket/syntax
                     racket/format)
         syntax/parse/define
         file/glob
         (only-in db query-exec))


(define read-only (make-parameter #f))

(define-syntax-rule (with-read-only-database statements ...)
		    (parameterize ([read-only #t])
		      statements ...))

(define (dynamic-find-base-function f-name)
  ;A better approach in general would be to make
  ;  has-many, has-one, and belongs-to all
  ;  register their model files in some lookup table so that we can dynamically find them later -- and we don't have to assume all models are in the main project.
  (dynamic-require
    (string->symbol 
      (~a (pkg-name) "/models/base"))
    f-name))


(struct model-error (e) #:transparent)

(define-syntax-rule (catch-model-errors lines ...)
  (with-handlers ([exn:fail?
                    (lambda (e)
                      (model-error e))])
                 lines ...))

(define cache-log (make-parameter #f))
(define query-cache (make-hash))

(define-syntax-rule (with-query-cache statements ...)
   (begin
     (reset-query-cache)
     ;(displayln "with-query-cache as non parameter / caching enabled.......")
     statements ...))

(define-syntax-rule (dumping-cached-models statements ...)
   (begin
     (reset-query-cache)
     statements ...
     (dump-cache-to-models)))


;query-cache is a hash from function names, like 'course->meetings and an input model (stored as an id) or a one like 'find-courses-by-name and "bob", to models or lists of models (stored as actual values)
;  We can just grab/map over all the cached values, ignoring inputs.
(define (dump-cache-to-models)
  (define vs (hash-values query-cache))

  (flatten vs))

(define (reset-query-cache)
  (set! query-cache (make-hash)))

(define (log . msg)
  #;
  (when query-cache ;SHould make an explicit logging param when we want to provide this feature...
        (pretty-print msg))
  (void))

(define (query->hash-key query-type input-model)
  (list query-type
	(if (entity? input-model)
	    (get input-model 'id)
	    input-model ;So it can be a number, string, etc.  
	    )))

(define (get-from-cache query-type input-model)
  (if (not query-cache)
      #f
      (begin
	(when (cache-log)
	  (displayln (list "Got from cache!" (query->hash-key query-type input-model))))
	(hash-ref query-cache 
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
	  
	  (when query-cache
	    (hash-set! query-cache
		       (query->hash-key query-type input-model)
		       new-value-to-cache))
	 
	  new-value-to-cache
	  ))))


(define (my-insert-one! model)
  (when (read-only)
    (error "The database is in read only mode"))

  (log 'insert-one!)
  (reset-query-cache)
  (catch-model-errors
    (insert-one! (conn) model))
  )

(define (my-update! . models)
  (when (read-only)
    (error "The database is in read only mode"))

  (log 'update!)
  (reset-query-cache)
  (apply update-one! (conn) models))

(define (my-update-one! model)
  (when (read-only)
    (error "The database is in read only mode"))

  (log 'update-one!)
  (reset-query-cache)
  (catch-model-errors
    (update-one! (conn) model))   )

(define (my-delete! . model)
  (when (read-only)
    (error "The database is in read only mode"))

  (log 'delete!)
  (reset-query-cache)
  (catch-model-errors
    (apply delete! (conn) model)))

(define (my-delete-one! model)
  (when (read-only)
    (error "The database is in read only mode"))

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
    [(_ name ([id-field id-field-things ...] [field-name field-things ...] ...))  

     (define unsafe-name
       (format-id #'name "unsafe-~a"
                  (syntax-e #'name)))

     (define export-name
       (format-id #'name "export-~a"
                  (syntax-e #'name)))

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
	 (provide (schema-out name)
		  (schema-out #,unsafe-name)
		  #,export-name)
	 (define-schema name
			#:table #,(sqlify plural-name)
			([id-field id-field-things ...]
			 [field-name field-things ...] 
			 ...)) 

	 (define-schema #,unsafe-name
			#:table #,(sqlify plural-name)
			([id-field id/f]
			 [field-name field-things ...] 
			 ...)) 

	 (define (#,export-name x)
	   (~a
	     "(" (string->symbol
		   (~a "make-" '#,unsafe-name))
	     " "
	     (~a "#:" 'id-field) " " (get x 'id)
	     " "
	     (string-join
	       (map ~a
		    (flatten
		      (list
			(list (~a "#:" 'field-name)
			      (~v (get x 'field-name)) ;Serialize this: moments -> constructors...
			      )
			...)))
		    " ")
	     
	     ")"))

         (module+ schema-info
           (provide schema-info) 

           (define schema-info
             '(name
                ([id-field id-field-things ...]
		 [field-name field-things ...] ...))))

         (provide #,name-fields)

         (define (#,name-fields)
           '(id-field field-name ...))
         
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

         #,@(map create-finders (syntax->datum #'(id-field field-name ...)))

         )]))

(define/contract (get model field-name)
  (-> entity? (or/c string? symbol?) any/c)
  (define type (get-type model))
  (define f (dynamic-require (string->symbol 
                               (~a (pkg-name) "/models")) 
                             (string->symbol (~a type "-" field-name))))

  (f model))

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


