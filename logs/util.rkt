#lang racket

(provide displayln-color)

(require raart)

(define (clear-fg)
  (draw-here (fg 'black (text " "))))

(define (displayln-color c s)
  (draw-here (fg c (text s)))
  (clear-fg))

