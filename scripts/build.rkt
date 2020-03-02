#lang at-exp racket

(provide build docker-image-name)

(require webapp/environment/util)

(define custom-docker-image-name (make-parameter #f))

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
