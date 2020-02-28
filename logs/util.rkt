#lang racket

(provide displayln-color)

(require ansi-color)
;(require raart)

(define (clear-fg)
  ;(draw-here (fg 'black (text " ")))
  (with-colors 'white
    (lambda () (displayln " ")))
  )

(define (displayln-color c s)
  ;(draw-here (fg c (text s)))
  (with-colors c
    (lambda () (displayln s)))
  (clear-fg)
  )

