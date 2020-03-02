#lang at-exp racket

(provide build)

(require webapp/environment/util)

(define (build . args)
  (load-dev-prefs!)

  (displayln "Stopping any running containers") 
  @system{
    @~a{
      docker rm $(docker stop $(docker ps -a -q --filter ancestor=@(docker-image-name) --format="{{.ID}}"))
    }
  }

  (displayln "Building image") 
  @system{
    @~a{docker build -t @(docker-image-name) .}
  })
