#lang racket

(provide back-to-index
         basic-index-table
         basic-show-table
         chip)

(require (except-in website/bootstrap select)
         webapp/models/util
         english)

(define (back-to-index model-name)
 (a href: (~a "/" (plural model-name))
       (button-link 
         (~a "View All "
             (string-titlecase
               (plural model-name))))))

(define (chip url content)
  (a href: url
     (badge-pill-info 
       content)))


(define (basic-index-table models
                           #:renderers (renderers (hash)))

  (define model-name (get-type (first models)))
  (define fields (get-fields (first models)))

  (define (my-td o f v)
    (define special-renderer 
      (hash-ref renderers f #f))
    (td
      (cond 
        [special-renderer (special-renderer o v)]
        [(eq? 'id f)
         (a href: (~a "/" (plural model-name) "/" v)
            v)]
        [(string-suffix? (~a f) "-id")
         (define other-model-name (first (string-split (~a f) "-")))
         (a href: (~a "/" (plural other-model-name) "/" v)
            v)]
        [else v])))

  (define (fields->tds m)
    (map (curry my-td m) 
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

(define (basic-show-table model)
  (define fields (get-fields model))  
  (define values (get-values model))  

  (define (my-row f v)
    (tr (td f) (td v)))

  (div
    (table class: "table"
           (thead 
             (tr (td "Field")
                 (td "Value")))
           (tbody
             (map my-row fields values)))) )
