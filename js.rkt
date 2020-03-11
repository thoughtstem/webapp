#lang at-exp racket

(provide
  (except-out
    (all-from-out website-js)
    select
    address)
  late-include-js)

(require website-js)

(define (late-include-js from (then @js{// Nothing here}))
  (define the-script (gensym 'script))

  @js{
  var @the-script = document.createElement("script");
  @the-script .src = "@from"
  @the-script .type= 'text/javascript'
  @the-script .onload=function(){
    @then
  }
  document.head.appendChild(@the-script);
  }
)
