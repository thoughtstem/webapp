#lang at-exp racket

(provide console)

(require webapp/environment/util)

(define (console . args)
  @system{
    @~a{
     racket -l racket -l @(pkg-name)/models -l @(pkg-name)/db/seeds -i
    }
  })
