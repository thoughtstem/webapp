#lang at-exp web-server

(provide module-browser
	 module-tree)

(require webapp/js
	 webapp/js/components/util
	 webapp/js/components/graph
	 webapp/models/util
	 webapp/server/client-communication
	 json)

(define edge  (lambda (a b)
		(list a b)))
(define (nodes->edges ns) 

  
  (define neighbors-1 (map imports->list ns))
  
   (apply append  (map (lambda (from tos)  
			 (map (curry edge from) tos))
		       ns
		       neighbors-1))
  
  )


(define (module-tree start-module-path #:with-prefixes (with-prefixes (list )))
  (local-require graph)
  
  (define es-1 
    (nodes->edges (cons start-module-path
		(filter
		  (curryr has-prefix-in? with-prefixes)
		(imports->list start-module-path)))))
  
  
  (layout (cose-layout))
  (graph-component
    (unweighted-graph/directed  es-1)
    ))

(define module-path->symbol
  (compose
    string->symbol
    (lambda (x) 
      (regexp-replaces x
		       '([#rx".rkt" ""]
			 [#rx"\"" ""]
			 [#rx"^/" ""])))

    ~a	
    module-path-index-resolve))

(define (imports->list module-path)
  (define ret (rest (first (module->imports module-path))))

  (map module-path->symbol ret))

(define (module-browser module-path #:with-prefixes (with-prefixes '()))
  (card
    (card-header "Browsing module path: " module-path)
    (card-body 
      (card-text
	(click-to-expand  
	  (badge-pill-warning 
	    (badge-pill-info 
	      (length 
		(filter
		  (curryr has-prefix-in? with-prefixes)
		  (imports->list module-path))))
	    "View " module-path " imports.") 
	  (lambda () (module-imports-list module-path #:with-prefixes with-prefixes)))
	(click-to-expand 
	  (badge-pill-warning 
	    (badge-pill-info 
	      (length 
		(filter-not lifted-function-name?
			    (exports->list module-path))))
	    "View " module-path " exports.") 
	  (lambda () (module-exports-list module-path)))))))


(define (has-prefix-in? i ps)
  (member (first (string-split (~a i) "/")) ps string=?))

(define (module-imports-list module-path #:with-prefixes with-prefixes)
  (define imports 
    (imports->list module-path))
  (define filtered-imports 
    (filter
      (curryr has-prefix-in? with-prefixes)
      imports))
  (let ()
    (map (compose li
		  (lambda (x) 
		    (click-to-expand
		      (badge-pill-warning "Browse: " x) 
		      (lambda () (with-handlers 
			([exn:fail? (thunk* "failed to render module-browser")])
			(module-browser x #:with-prefixes with-prefixes)))))
		  )
	filtered-imports)
    ))

(define (lifted-function-name? x)
  (string-prefix? (~a x) "lifted/"))

(define (module-exports-list module-path)
  (define e 
    (filter-not lifted-function-name?
      (exports->list module-path)))
  (ol (map (compose li (curry explore-identifier module-path))
	   e)))


(define (explore-identifier module-path identifier)
  (click-to-expand 
    (pre identifier)
    (lambda ()
      (identifier-details 
	module-path identifier))))

(define (identifier-details module-path identifier) 
  (local-require syntax/modresolve)
  (local-require lang-file/read-lang-file)

  (define-values (exp-fs exp-ms)
    (module->exports module-path))

  (define id-info 
    (filter 
      (lambda (info)
	(eq? identifier (first info)))
      (rest (first exp-fs))))

  (define defining-modules-info
    (and (not (empty? id-info))
	 (rest (first id-info))))

  (define defining-module-info
    (and defining-modules-info
	 (first defining-modules-info)))


  (define s (file->string
	      (resolve-module-path module-path)))
  (define file-syntax (read-lang-file (resolve-module-path module-path)))
  (define definition (find-definition identifier file-syntax))
  (define docs 
    (with-handlers 
      ([exn:fail? (lambda (e) "No docs")])
      (dynamic-require `(submod ,module-path docs) identifier)))
  
  (if (and defining-module-info (not (empty? defining-module-info)))
      (div (map (compose module-browser module-path->symbol) defining-module-info))
      (list
	docs
	(hr)
	(pre definition))))

(define (find-definition identifier file-syntax)
 (local-require syntax/to-string)
  (define (is-definition? syntax)
    (define d (syntax->datum syntax))
    (and (list? d)
	 (not (empty? d))
	 (eq? 'define (first d))))
 
  (define (definition-has-name? name syntax)
    (define d (syntax->datum syntax))
    (or
      (and (not (list? (second d))) 
	   (eq? name (second d)))
      (and (list? (second d))
	   (eq? name (first (second d))))))

  
  (define definitions (filter is-definition? (syntax-e (fourth (syntax-e file-syntax)))))
  
  (define the-definition
    (findf  (curry definition-has-name?
		   identifier)
	    definitions))
    (if the-definition
	(list 
         "("
         (syntax->string the-definition)
         ")" )
	"Could not find definition")
  )
