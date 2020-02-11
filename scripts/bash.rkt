#lang at-exp racket

(provide bash)

(require webapp/environment/util)

(define (bash)
  @system{
    @;@~a{docker exec -it $(docker ps -q --filter ancestor=@(pkg-name) --format="{{.ID}}") bash}
    @~a{docker exec -it @(get-cid) bash} 
  })
