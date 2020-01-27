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

;Used as prefix for db stuff
(define app-name 
  (make-parameter 
    "my_app"))

;Name of the racket package containing the app 
(define pkg-name 
  ;Another possibility is to set this based on what's at the root of the docker container.    
  (make-parameter
    (last 
      (string-split
        (~a (current-directory))
        "/"))))

(define (db-host)
  (or 
    (getenv "DB-HOST")
    (~a "localhost")))

(define (db-port)
  (~a 5432))

(define (db-name)
  (~a (app-name) "_" (env)))

(define (db-user)
  (~a (app-name) "_" (env)))

(define (db-password)
  (~a (app-name) "_" (env)))

(define last-connection #f)

(define (conn)
  (when (not last-connection)
    (set! last-connection
      (postgresql-connect
        #:server   (db-host)
        #:user     (~a (app-name) "_" (env)) 
        #:database (~a (app-name) "_" (env))
        #:password (~a (app-name) "_" (env)))))

  ;TODO: Pooling and virtual connections.
  last-connection)

