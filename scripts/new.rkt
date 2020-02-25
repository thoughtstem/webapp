#lang at-exp racket

(provide new)

(require website)

(define (new . args)
  (define name (first args))

  (define project-directory
    (make-directory
      (build-path
        (current-directory)
        name)))

  (render (new-project-files name) 
          #:to name)
  
  ;Empty directories can get created here
  (new-project-dir name "db" "migrations"))

(define (new-project-dir name . args)
  (make-directory
    (apply build-path
           (current-directory)
           name
           args)))

(define (new-project-files name)
  (list 
    (Dockerfile name)
    (db/seeds.rkt name)
    (tests/util.rkt name)
    (models.rkt name)
    (models/base.rkt name)
    (models/main.rkt name)
    (server/main.rkt name)
    (server/routes.rkt name)
    (info.rkt name)
    (scribblings/manual.scrbl name)
    (environment/main.rkt name)))

(define (tests/util.rkt name)
  (page tests/util.rkt
        @~a{
        #lang racket
  
        (provide (all-from-out
                   webapp/tests/util
                   @|name|/environment/main
                   @|name|/models
                   @|name|/db/seeds
                   rackunit))

        (require webapp/tests/util
                 @|name|/environment/main
                 @|name|/models
                 @|name|/db/seeds
                 rackunit)

        }))

(define (server/routes.rkt name)
  (page server/routes.rkt
        @~a{
        #lang racket

        (require webapp/server/util
                 @|name|/models)

        (provide server-dispatch)

        (define-values (server-dispatch server-url)
          ;Add your routes here
          (dispatch-rules
            [("home")
             (lambda (req)
               (response/html/content
                 (h1 "Welcome to @|name|"))) ]
            ))
          }))

(define (server/main.rkt name)
  (page server/main.rkt
        @~a{
        #lang racket

        (require webapp/server/util
                 webapp/environment/util 
                 @|name|/server/routes
                 (only-in website/bootstrap render bootstrap-files))

        (define (server)
          (load-current-env!)
          (render #:to "public"
                  (list
                    (bootstrap-files)))

          (serve/servlet #:port 8080
                         #:servlet-regexp #rx""
                         #:servlet-path "/home"
                         #:listen-ip "0.0.0.0"
                         #:extra-files-paths (list (build-path "public"))
                         server-dispatch))

        (module+ main
          (server))
        }))

(define (models/base.rkt name)
  (page models/base.rkt
        @~a{
        #lang reprovide
        (glob-in "./*/base.rkt") 
        }))

(define (models/main.rkt name)
  (page models/main.rkt
        @~a{
        #lang reprovide
        (glob-in "./*/main.rkt") 
        }))

(define (models.rkt name)
  (page '("models.rkt")
        @~a{
        #lang reprovide
        "./models/main.rkt"
        webapp/models/util 
        }))

(define (info.rkt name)
  (page '("info.rkt")
        @~a{
        #lang info
        (define collection "@|name|")
        (define deps '("base" 
                       "reprovide-lang"
                       "https://github.com/thoughtstem/webapp.git"
                       ))
        (define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
        (define scribblings '(("scribblings/manual.scrbl" ())))
        (define pkg-desc "Description Here")
        (define version "0.0")
        }))

(define (db/seeds.rkt name)
  (page db/seeds.rkt
        @~a{
        #lang racket

        (provide insert-seeds!)

        (require webapp/models/util)

        #;
        (define-seed unicorns
                     (find-course-by-name "Becoming Unicorn")
                     (make-course #:name "Becoming Unicorn"
                                  #:description "How to be the unicorn you feel you are"))

        (define (insert-seeds!)
          ;Touch your seeds here, e.g.
          #;
          unicorns

          (void))  
        }))


(define (fix-dashes s)
  (string-replace s "-" "_"))

(define (Dockerfile name)  
  (page '("Dockerfile")
        @~a{
        FROM srfoster/mc-racket-docker

        RUN apt-get update
        RUN apt-get -y install postgresql postgresql-client postgresql-contrib 
        RUN service postgresql start

        USER postgres

        RUN /etc/init.d/postgresql start && sleep 5 &&\
        psql --command "CREATE USER @|(fix-dashes name)|_dev WITH SUPERUSER PASSWORD '@|(fix-dashes name)|_dev';" &&\
        psql --command "CREATE USER @|(fix-dashes name)|_test WITH SUPERUSER PASSWORD '@|(fix-dashes name)|_test';" &&\
        createdb -O @|(fix-dashes name)|_dev @|(fix-dashes name)|_dev &&\
        createdb -O @|(fix-dashes name)|_test @|(fix-dashes name)|_test

        USER root

        COPY . @|name|

        RUN raco pkg install --no-docs --auto @|name|/

        WORKDIR "/@|name|"  
        }))


(define (scribblings/manual.scrbl name)
  (page scribblings/manual.scrbl
        @~a{
        #lang scribble/manual
        @"@"require[@"@"for-label[racket/base]
                     webapp/scribblings/util]

        @"@"title{@name} 
        }))


(define (environment/main.rkt name)
  (page environment/main.rkt  
        @~a{
        #lang racket

        (provide (all-from-out
                   webapp/environment/util))

        (require webapp/environment/util)

        (app-name "@name")
        }))


