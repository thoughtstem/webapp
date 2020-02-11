#lang at-exp racket

(provide conn 
         env
         app-name
         pkg-name
         
         db-host
         db-port
         db-name
         db-user
         db-password
         load-current-env!

         get-cid
         )

(require db file/glob)

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
(define (pkg-name) 
  ;Very much assumes we are in a docker container and that the web app is at the filesystem root "/"
  (with-handlers ([exn:fail?
                    (thunk*
                      (define dir 
                        (~a (last (explode-path (current-directory)))))

                      ;(displayln (~a "Not in docker container.  Assuming current directory is the app directory: " dir))

                      dir 
                      )])
    (~a (second (explode-path (first (glob "/*/info.rkt"))))))
  
  )

(define (db-host)
  (or 
    (getenv "DB_HOST")
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


(define (load-current-env!)
  (with-handlers ([exn:fail? 
                    (thunk* 
                      #;
                      (displayln "No current environment/main.rkt file found")
                      (void) )])
                 (dynamic-require 
                   (string->symbol
                     (~a 
                       (pkg-name)
                       '/environment/main))  
                   #f)))

(define (get-cid)
    (string-trim (with-output-to-string
      (lambda () @system{
                   @~a{docker ps -q --filter ancestor=@(pkg-name) --format="{{.ID}}"}}))))