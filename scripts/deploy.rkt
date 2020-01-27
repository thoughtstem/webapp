#lang at-exp racket

(provide deploy)

(require webapp/environment/util)

(define (deploy . args)
  (when (empty? args)
    (error "deploy requires a tag -- which will be given to the most recent build and pushed to dockerhub"))

  @system{
    @~a{
    docker tag @(pkg-name) @(first args) && docker login && docker push @(first args) 
    }
  })

