#lang racket

(provide conn 
         env
         app-name
         pkg-name
         
         db-host
         db-port
         db-name
         db-user
         db-password)

(require db)

(define (environment-is? s)
  (define e
    (getenv "ENVIRONMENT"))
  (and e (string=? e s)))

(define env 
  (make-parameter 
    (if (environment-is? "prod") 
      "prod"
      "dev")))

;Used as prefix for 
(define app-name 
  (make-parameter 
    "my_app"))

;The main app installed at the root of the docker container
(define (pkg-name) 
  ;TODO: fix this 
  "mc-data"
  #;
  (last 
    (string-split
      (~a (current-directory))
      "/")))

(define (db-host)
  (or 
    (getenv "DB-HOST")
    (~a "localhost")))

(define (db-port)
  (~a 5432))

(define (db-name)
  (~a "metacoders_" (env)))

(define (db-user)
  (~a "metacoders_" (env)))

(define (db-password)
  (~a "metacoders_" (env)))

(define last-connection #f)

(define (conn)
  (when (not last-connection)
    (set! last-connection
      (postgresql-connect
        #:server   (db-host)
        #:user     (~a "metacoders_" (env)) 
        #:database (~a "metacoders_" (env))
        #:password (~a "metacoders_" (env)))))

  ;TODO: Pooling and virtual connections.
  last-connection)

