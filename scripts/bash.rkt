#lang at-exp racket

(provide bash)

(require webapp/environment/util)

(define (bash)
  (load-dev-prefs!)
  @system{
    @~a{docker exec -it @(get-cid) bash} 
  })
