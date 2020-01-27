#lang racket

(provide scaffold-index
         scaffold-show 
         
         response/html
         response/html/content
         request->params
         response/jsexpr
         (all-from-out
           web-server/servlet
           web-server/servlet-env
           website/bootstrap))

(require (except-in website/bootstrap
                    select header)
         web-server/http/response-structs
         web-server/servlet
         web-server/servlet-env
         webapp/models/util
         english
         racket/hash
	 net/uri-codec
         json)


(define-syntax-rule (scaffold-index model-name)
  (lambda (req)
    (define models (all model-name))
    (response/html
      (content
        (scaffold-index-html 'model-name models))))) 

(define-syntax-rule (scaffold-show model-name)
  (lambda (req i)
    (define model 
      (find-by-id model-name i))

    (response/html
      (content
        (scaffold-show-html model))))) 

(define (scaffold-index-html model-name models)
  ;TODO: This will break if models is empty
  ;  Use the model name to look up the schema info.  Make better utils for that. 
  (define fields (get-fields (first models)))

  (define (my-td f v)
    (td
      (cond 
        [(eq? 'id f)
         (a href: (~a "/" (plural model-name) "/" v)
            v)]
        [(string-suffix? (~a f) "-id")
         (define other-model-name (first (string-split (~a f) "-")))
         (a href: (~a "/" (plural other-model-name) "/" v)
            v)]
        [else v])))

  (define (fields->tds m)
    (map my-td 
         (get-fields m)
         (get-values m)))

  (container
    (h1 (string-titlecase (~a model-name)) " Index:") 
    (card
      (table class: "table"
             (thead
               (tr
                 (map (curry th 'scope: "col")
                      fields)))

             (tbody
               (map (compose tr fields->tds) 
                    models))))))

(define (scaffold-show-html model)
  (define fields (get-fields model))  
  (define values (get-values model))  

  (define (my-row f v)
    (tr (td f) (td v)))

  (container
    (h1 (string-titlecase (~a (get-type model))) 
        " Show")
    (button-link 
      (a href: (~a "/" (plural (get-type model)))
         "Back to Index"))
    (card
      (table class: "table"
        (thead 
          (tr (td "Field")
              (td "Value")))
        (tbody
          (map my-row fields values))))))

(define (response/html html)
  (response/full
    200 #"Success"
    (current-seconds) TEXT/HTML-MIME-TYPE
    '()
    (list 
      (string->bytes/utf-8 
        (xml->string html)))))

(define (response/html/content html)
  (response/html
    (content html)))

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

  (displayln query)

  (if (not query)
    (hash)
    (make-hash query)))

(define (hashify-data d)
  ;something=va1&something_else=val2
  ;  Gotta parse this stuff out into a hash

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



;This is supposedly in webserver already, but we are on Racket 7.0, so I think that is why we aren't getting it.  Need to try updating the Racket docker base image
(define (response/jsexpr jsexpr) 
  (response/full
    200 #"OK"
    (current-seconds) #"application/json; charset=utf-8" 
    empty
    (list (jsexpr->bytes jsexpr))))



