#lang racket

;TODO: Replace by stateless...
(provide serve-function
	 (rename-out
	   [stateless/embed/url embed/url])
	 function->link)

(require web-server/servlet

	 webapp/server/util/stateless-serve-function
	 webapp/server/util/current-req)

(define (serve-function #:returning returning
			f . arg-funcs)
  (define (call f x) (f x))

  (stateless/serve-function
    (lambda (a . args)
      (returning
	(apply f
	       (map call
		    arg-funcs
		    (cons a args))))))

  #;
  (lambda (req . args)
    (displayln "HERE??")
    (web-parameterize ([current-req req])
	    (define martialed-args
	      (map call
		   arg-funcs
		   args)) 

	    (send/suspend/dispatch
	      (lambda (e)
		(parameterize [(embed/url e)]
		  (returning (apply f martialed-args))))))))


#;
(define embed/url 
  (make-parameter #f))

(define (function->link f)
  ((stateless/embed/url) f))


