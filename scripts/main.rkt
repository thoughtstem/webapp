#lang racket

(require raco/command-name
         webapp/environment/util
         webapp/scripts/build
         webapp/scripts/dev
         webapp/scripts/bash
         webapp/scripts/init-db
         webapp/scripts/console
         webapp/scripts/server
         webapp/scripts/deploy
         webapp/scripts/migrate
         webapp/scripts/generate
         webapp/scripts/destroy
         webapp/scripts/new)

(load-current-env!)

(define sub-commands
  (vector->list
      (current-command-line-arguments))
  #;
  (command-line
   #:program (short-program+command-name)
   #:args sub-commands
   sub-commands))

(when (empty? sub-commands)
  (displayln "What's your subcommand?")
  (exit))

(define sub-command (first sub-commands))

(define function
  (match sub-command
    ("new" new)
    ("build" build)
    ("dev"   dev)
    ("enter-bash"  bash)
    ("console"  console)
    ("init-db"  init-db)
    ("server"  server)
    ("deploy"  deploy)
    ("migrate"  run-migrations)
    ("generate" generate)
    ("destroy"  destroy)))

(apply function 
       (rest sub-commands))  



