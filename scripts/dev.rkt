#lang at-exp racket

(define dev-pkgs (vector->list (current-command-line-arguments)))

(displayln "Stopping any running mc-data containers") 
@system{
 docker rm $(docker stop $(docker ps -a -q --filter ancestor=mc-data --format="{{.ID}}"))
}

(displayln "Starting new mc-data container") 
(define (patch-in pkg)
  (displayln (~a "  Patching in " pkg))
  (define pkg-name (last (string-split pkg "/")))
  @~a{-v @|pkg|:/root/.racket/7.0/pkgs/@|pkg-name|})

@system{
  @~a{docker run -dt -p 8080:8080 -v `pwd`:/mc-data @(string-join (map patch-in dev-pkgs) " ") mc-data } 
}

(displayln "Starting postgres and seeding the database within the mc-data container") 
@system{
  docker exec `docker ps -q --filter ancestor=mc-data --format="{{.ID}}"` bash -c "/etc/init.d/postgresql start;"
}




