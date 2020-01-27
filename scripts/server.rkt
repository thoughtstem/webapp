#lang at-exp racket

(provide server)

(define (server . args)
  @system{
   @~a{
    racket server/main.rkt 
   }
  })
