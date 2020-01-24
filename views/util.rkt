#lang racket

(provide back-to-index)

(require website/bootstrap
         english)

(define (back-to-index model-name)
 (a href: (~a "/" (plural model-name))
       (button-link 
         (~a "View All "
             (string-titlecase
               (plural model-name))))))
