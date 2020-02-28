#lang at-exp web-server

(provide js-url
	 js-url/stateless/call
	 js-url/stateless/call-hash
	 stateless/embed/url
	 demartial-client-args
	 stateless/serve-function
	 ; current-req
	 )

(require 
  net/uri-codec
  webapp/server/util/current-req
  webapp/server/util/responses
  )

(define stateless/embed/url 
  (make-web-parameter #f))

;Constructs a url based on string parts and code (e.g. created by (js-val ...))
(define (js-url . parts)
  (define (fix s)
    (if (string? s)
	(~s s)
	(~a s)))
  (string-join
    (map fix parts)
    "+"))

;Like js-url/call, but the function must be explicitly defined at the top-level of a module -- can't be a lambda or thunk :(
;   But it seems that if we write our own versions of functions like curry/curryr -- those work...

(require webapp/models/util)
(define (js-url/stateless/call f . args)

  (apply js-url
	 (~s
	   ((stateless/embed/url)
	    (lambda (r . args)
	      (with-query-cache
		(apply 
		  (stateless/serve-function ;Fixed it???
		    (lambda ()
		      (response/html
			(apply f (demartial-client-args r)))))
		  (cons r args))
		))
	    ))

	 "/"

	 (flatten
	   (add-between 
	     (map 
	       (lambda (a) 
		 (string->symbol (~a "encodeURIComponent(" a ")")))
	       args)
	     "/"))))


(define (js-url/stateless/call-hash f h)
  (js-url
    (~s
      ((stateless/embed/url)
       (lambda (r . args)
	 (with-query-cache
	   (define h (request->params r))
	   (apply 
	     (stateless/serve-function ;Fixed it???
	       (lambda ()
		 (response/html
		   (f h))))
	     (cons r args))))))

    ;Client-side: Package up the hash as query params in the URL.
    "?"
    (string->symbol
      @~a{
      Object.entries(@h).map((kv)=>
			     "&" + kv[0] + "=" + encodeURI(kv[1])
			     ).join("") })))


(define (demartial-client-args req)
  ;Now, we destructure the url and find the arguments.
  (define url-string
    (url->string (request-uri req)))

  (define path-parts
    (string-split 
      url-string
      "/"))

  (define continuation-param 
    (index-where
      path-parts
      (lambda (p)
	(string-contains? (~a p) ";"))))

  (define (maybe-string->number s)
    (define maybe-n
      (string->number s))

    (or maybe-n s))

  (define actual-args
    (map (compose maybe-string->number uri-decode)
	 (drop path-parts 
	       (add1 continuation-param))))

  actual-args)


(define (stateless/serve-function f)

  ;Without params?
  ;  Does that work?
  (lambda (req . args)
    (send/suspend/dispatch
      (lambda (e)
	(web-parameterize 
	  ([stateless/embed/url e]
	   [current-req req])
	  (apply f args))
	))))





