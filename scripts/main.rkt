#lang racket

(require raco/command-name
         webapp/scripts/build
         webapp/scripts/dev
         webapp/scripts/bash
         webapp/scripts/init-db
         webapp/scripts/console
         webapp/scripts/server)

(define sub-commands
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
    ("build" build)
    ("dev"   dev)
    ("enter-bash"  bash)
    ("enter-console"  console)
    ("init-db"  init-db)
    ("server"  server)))

(apply function 
       (rest sub-commands))  
