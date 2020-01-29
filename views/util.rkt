#lang racket

(provide back-to-index
         chip)

(require website/bootstrap
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
