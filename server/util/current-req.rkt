#lang web-server

(provide current-req
	 request->params)

(require racket/hash json net/uri-codec)

(define current-req (make-web-parameter #f))

(define (request->params req)
  (hash-union
    (request->post-data-hash req)
    (request->query-string-hash req)))



(define (request->post-data-hash req)
  (define raw-data
    (request-post-data/raw req))   

  (cond 
    [(not raw-data) (hash)]
    [(is-json? raw-data) 
     (hash 'json
           (string->jsexpr 
             (bytes->string/utf-8 raw-data)))]
    [else
     (let ([posted-data 
             (bytes->string/utf-8 raw-data)])

       (hashify-data posted-data))]))

(define (is-json? b)
  (with-handlers ([exn:fail? (thunk* #f)])
    (string->jsexpr 
      (bytes->string/utf-8 b))))

(define (request->query-string-hash req)
  (define query
    (url-query (request-uri req)))   

  (if (not query)
    (hash)
    (make-hash 
      (map 
	(lambda (p)
	  (cons (car p)
		(maybe-string->number
		  (cdr p))))
	query))))

(define (hashify-data d)
  (define raw-pairs
    (string-split d "&"))

  (define pairs
    (map (curryr string-split "=")
         raw-pairs))

  (define decoded-pairs
    (map (lambda (p)
           (list-set p 1 (uri-decode (second p))))
         pairs) )      

  (apply hash
         (apply append decoded-pairs))) 



(define (maybe-string->number s)
  (define maybe-n
    (and (string? s)
	 (string->number s)))

  (or maybe-n s))

