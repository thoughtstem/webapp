#lang at-exp racket

(provide build)

(require webapp/environment/util)

(define (build . args)
  (displayln "Stopping any running containers") 
  @system{
    @~a{
      docker rm $(docker stop $(docker ps -a -q --filter ancestor=@(pkg-name) --format="{{.ID}}"))
    }
  }

  (displayln "Building image") 
  @system{
    @~a{docker build -t @(pkg-name) .}
  })
