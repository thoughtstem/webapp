#lang at-exp web-server

(provide module-browser)

(require webapp/js
	 webapp/js/components/util
	 webapp/models/util
	 webapp/server/client-communication
	 json)

(define (module-browser module-path #:with-prefixes (with-prefixes '()))
  (card
    (card-header "Browsing module path: " module-path)
    (card-body 
      (card-text
	(click-to-expand  
	  (badge-pill-warning "View " module-path " imports.") 
	  (lambda () (module-imports-list module-path #:with-prefixes with-prefixes)))
	(click-to-expand 
	  (badge-pill-warning "View " module-path " exports.") 
	  (lambda () (module-exports-list module-path)))))))



(define (module-imports-list module-path #:with-prefixes with-prefixes)
  (define imports 
    (map
      (compose
	string->symbol
	(lambda (x) 
	  (regexp-replaces x
			   '([#rx".rkt" ""]
			     [#rx"\"" ""]
			     [#rx"^/" ""])))

	~a	
	module-path-index-resolve)
      (rest (first (module->imports module-path)))))
  (define filtered-imports 
    (filter
      (lambda (i) 
	(member (first (string-split (~a i) "/")) with-prefixes string=?))
      imports))
  (let ()
    (map (compose li
		  (lambda (x) 
		    (click-to-expand
		      (badge-pill-warning "Browse: " x) 
		      (lambda () (with-handlers 
			([exn:fail? (thunk* "failed to render module-browser")])
			(module-browser x)))))
		  )
	filtered-imports)
    ))


(define (module-exports-list module-path)
  (define e 
    (filter-not (lambda (x) (string-prefix? (~a x) "lifted/"))
      (exports->list module-path)))
  (ul (map (compose li (curry explore-identifier module-path))
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
  (define s (file->string
	      (resolve-module-path module-path)))
  (define file-syntax (read-lang-file (resolve-module-path module-path)))
  (define definition (find-definition identifier file-syntax))
  (define docs 
    (with-handlers 
      ([exn:fail? (lambda (e) "No docs")])
      (dynamic-require `(submod ,module-path docs) identifier)))
  (list
    docs
    (hr)
    (pre
     definition)))

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
