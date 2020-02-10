#lang racket

(provide scaffold-index
         scaffold-show 
         
         request->params
         (except-out
           (all-from-out
             web-server/servlet
             web-server/servlet-env
             website/bootstrap)

	   ;Random identifiers that we rarely use and which commonly conflict with user's vocabulary in the domain of webapps
           header
           script
	   address)
         
         serve-function
	 current-req
	 (rename-out [identity as-is])
	 function->link
	 
	 (all-from-out webapp/server/util/responses)
	 (all-from-out webapp/server/util/serve-function)
	 (all-from-out webapp/server/util/current-req)
	 (all-from-out webapp/server/util/stateless-serve-function)
	 )

(require (except-in website/bootstrap select header)
	 webapp/server/util/responses
	 webapp/server/util/serve-function
	 webapp/server/util/current-req
	 webapp/server/util/stateless-serve-function
         web-server/http/response-structs
         web-server/servlet
         web-server/servlet-env
         webapp/models/util
         webapp/views/util
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

    (response/html/content
      (basic-show-table model))))

(define (scaffold-index-html model-name models)
  (if (empty? models)
    (container (h1 "Table is empty")) 
    (let ()
      (basic-index-table models))))



