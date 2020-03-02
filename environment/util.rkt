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
	 
	 prod?
	 dev?
	 test?
	 
	 load-dev-prefs!
	 host-port
	 docker-image-name)

(require db file/glob)

(define (prod?)
  (environment-is? "prod"))

(define (dev?)
  (environment-is? "dev"))

(define (test?)
  (environment-is? "test"))

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

(define (no-dashes s)
  (string-replace s "-" "_"))

(define (db-port)
  (~a 5432))

(define (db-name)
  (~a (no-dashes (app-name))
      "_" (env)))

(define (db-user)
  (~a (no-dashes (app-name))
      "_" (env)))

(define (db-password)
  (~a (no-dashes (app-name))
      "_" (env)))

(define last-connection #f)

(define (conn)
  (when (not last-connection)
    (set! last-connection
      (with-handlers ([exn:fail? (thunk*
				   (postgresql-connect
				     #:server   (db-host)
				     #:user     (db-user) 
				     #:database (db-name)
				     #:password (db-password) )
				   )])
		     (sqlite3-connect
		       #:database (~a "/" (pkg-name) "/data.sqlite")))))

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

(define docker-image-name (make-parameter #f))
(define host-port (make-parameter 8080))

(define (load-dev-prefs!)
  (define prefs-file
    (build-path (current-directory)
		".webapp-dev-prefs.rkt"))

  (when (file-exists? prefs-file)
    (displayln "Prefs file detected.  Loading...")
    (dynamic-require prefs-file #f)

    (displayln
      (~a "Host port is: " 
	  (host-port)))

    (displayln
      (~a "Docker image name: " 
	  (docker-image-name)))))

(define (get-cid)
  (string-trim (with-output-to-string
		 (lambda () @system{
		   @~a{docker ps -q --filter ancestor=@(or (docker-image-name) (pkg-name)) --format="{{.ID}}"}}))))


