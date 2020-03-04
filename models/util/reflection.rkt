#lang racket

(provide
  get-relations
  get-relations-hash
  get-related
  relations-list
  model-name->relations-hash
  get-type
  
  all-models-plural
  all-models-singular
  )

(require webapp/environment/util
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


(define (get-type model)
  (string->symbol
    (second
      (string-split 
        (symbol->string
          (vector-ref 
            (struct->vector model)
            0)) 
        ":"))))

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
	     relation-names))))


(define (get-relations-hash module-path)
  (with-handlers ([exn:fail? (thunk* (hash))])
		 (get-relations module-path)))

(define (relations-list [color1 "black"] [color2 "gray"])
  (local-require racket/hash webapp/js)
  (define all-relations (apply hash-union
			       (map model-name->relations-hash 
				    (all-models-plural))))
  (define sorted-list (sort (hash->list all-relations) 
			    string<?
			    #:key (compose ~a car)))
  (apply ul (map (curry relation->list-item
                        #:mode 'plural
                        #:color1 color1
                        #:color2 color2)
		 sorted-list)))


;THIS SHOULD NOT GO HERE.
;  Rendering stuff needs to get separated, even if it's rendering for reflection purposes...
(define (relation->list-item relation #:mode [mode 'default] #:color1 [color1 "black"] #:color2 [color2 "gray"])
  (local-require (only-in webapp/js span li style: color: properties))

  (define id     (~a (car relation)))
  (define type   (string-replace (~a (cdr relation)) "-" " "))
  (define source (if (eq? mode 'plural)
		     (plural (first  (string-split id "->")))
		     (first  (string-split id "->"))))
  (define target (cond [(eq? mode 'singular) (singular (second
							 (string-split id "->")))]
		       [(eq? mode 'plural)   (plural   (second
							 (string-split id "->")))]
		       [else (second (string-split id "->"))])
    )
  (define (colorize str)
    (define color (if (string-contains? str "assignment")
		      color2
		      color1
		      ))
    (span style: (properties color: color) str))

  (li (colorize source) " " type " " (colorize target)))



(define (model-name->relations-hash model-name)
  (local-require racket/hash)

  (define relations 
    (hash-union
     #:combine/key (lambda (k v0 v) v)
     (with-handlers ([exn:fail? (thunk* (hash))])
       (get-relations 
        (string->symbol
         (~a (pkg-name) "/models/" 
             model-name  "/base"))))
     (with-handlers ([exn:fail? (thunk* (hash))])
       (get-relations 
        (string->symbol
         (~a (pkg-name) "/models/" 
             model-name "/main"))))))
  relations)
  
(define (get-related x (kind #f))
  (local-require racket/hash)

  (define relations 
    (hash-union
      #:combine/key (lambda (k v0 v) v)
      (with-handlers ([exn:fail? (thunk* (hash))])
		     (get-relations 
		       (string->symbol
			 (~a (pkg-name) "/models/" 
			     (plural (get-type x))  "/base"))))
      (with-handlers ([exn:fail? (thunk* (hash))])
		     (get-relations 
		       (string->symbol
			 (~a (pkg-name) "/models/" 
			     (plural (get-type x))  "/main"))))))

  (when kind
    (set! relations
      (make-hash
	(filter
	  (compose (curry eq? kind)
		   cdr)
	  (hash->list relations)))))

  (define relation-names (hash-keys relations))


  (define relation-functions
    (map 
      (lambda (n)
	(dynamic-require
	  (string->symbol
	    (~a (pkg-name) "/models")
	   ; "/models/serializable"
	    ) n ))
      relation-names))

  (filter-not (curry eq? #f)
    (map 
      (lambda (f)
	(f x))
      relation-functions)))

(define (all-models-plural)
  ;If we want a singular version, we should probably track them as define-schema is used,
  ;  rather than assuming that the file structure is some kind of source of truth (even though is sort of is for now)

  (define model-files (glob (~a "/" (pkg-name) "/models/*/base.rkt")) )
  (map (compose 
         string->symbol
         (lambda (f)
           (list-ref (string-split (~a f) "/") 2))) 
       model-files))

(define (all-models-singular)
  (map (compose string->symbol
                singular)
       (all-models-plural)))
