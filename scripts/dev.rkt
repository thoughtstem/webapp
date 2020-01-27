#lang at-exp racket

(provide dev)

(require webapp/environment/util)

(define (dev . args)
  (define dev-pkgs 
    args
    #;
    (vector->list (current-command-line-arguments)))

  (displayln "Stopping any running containers") 
  @system{
    @~a{
      docker rm $(docker stop $(docker ps -a -q --filter ancestor=@(pkg-name) --format="{{.ID}}"))   
    }
  }

  (displayln "Starting new container") 
  (define (patch-in pkg)
    (displayln (~a "  Patching in " pkg))
    (define pkg-name (last (string-split pkg "/")))
    @~a{-v @|pkg|:/root/.racket/7.0/pkgs/@|pkg-name|})

  @system{
  @~a{docker run -dt -p 8080:8080 -v `pwd`:/@(pkg-name) @(string-join (map patch-in dev-pkgs) " ") @(pkg-name) } 
  }

  (displayln "Starting postgres and seeding the database within the container") 
  @system{
    @~a{
      docker exec `docker ps -q --filter ancestor=@(pkg-name) --format="{{.ID}}"` bash -c "/etc/init.d/postgresql start;" 
    } 
  })

