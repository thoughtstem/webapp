#lang at-exp racket

(define dev-pkgs 
  (vector->list (current-command-line-arguments)))

(for ([pkg-name dev-pkgs])
  @system{
    @~a{cd /root/.racket/7.0/pkgs/@|pkg-name| && raco pkg install} 
  })
