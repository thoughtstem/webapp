#lang at-exp racket

(provide run-migrations
         create-migration
         delete-migration)

(require webapp/logs/util
         webapp/environment/util
         (only-in webapp/scripts/generate/util
                  migration-name
                  maybe-delete-file
                  ))

(define (create-migration name
                          (after (thunk* (void))))
  (parameterize ([current-directory "db"])
    (if (system @~a{DATABASE_URL=postgres://@(db-user):@(db-password)@"@"@(db-host):@(db-port)/@(db-name) raco north create @(migration-name name)})
      (begin 
        (after)
        (displayln-color 'green  "Migration created"))     
      (displayln-color 'yellow "Migration exists"))))


(define (delete-migration name)
  (local-require file/glob)

  (define g
    (glob
      (~a "db/migrations/*" (migration-name name) ".sql")))

  (if (empty? g)
    (displayln-color 'yellow
                     "  Already gone, skipping")
    (maybe-delete-file (first g)))) 

(define (run-migrations)
  (displayln (~a "Running migrations on Env: " (env)))
  (parameterize ([current-directory "/mc-data/db"])
    (system @~a{DATABASE_URL=postgres://@(db-user):@(db-password)@"@"@(db-host):@(db-port)/@(db-name) raco north migrate -f})
    ))

