#lang racket

(provide init-db)

(require webapp/db/util)

(define (init-db)
  (create-db)
  (seed-db))
