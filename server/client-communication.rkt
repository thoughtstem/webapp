#lang at-exp racket


;Pivot to stateless, but with guarantees:
;  * You can call server side functions with js/call
;    - Those functions will, in turn be called with current-req set.  Author them with that assumption.
;  * You can call js/call anywhere you could write @js code.  All endpoints will set up embed/url approrpriately.  Whatever that means...

(provide 
  js-inject
  js-fetch
  js-url
  js-val ;Do we need?
  
  js/call
  js/call-hash)

(require 
  (except-in website-js select)
  webapp/server/util
  webapp/server/util/current-req
  )

(require 
  webapp/server/util/stateless-serve-function)


(define/contract (js-fetch method path #:then (then (const "")))
   (->* (string? (or/c string? symbol?))
	[#:then procedure?]
	any/c)
    @js{
    fetch(@|path|, 
	  {
	  method: "@method".replace(/ /g,""), 
	  })
    .then((response) => {
          response.text().then(function(last_response){
	      console.log(last_response);
	      @(then 'last_response)
          })
    }) 
 })


;Evaluates all script tags and injects the html.
(define (js-inject id stuff)
  @js{
   setTimeout(function(){
	     var script_match = @stuff .match(/<script>([\s\S]*?)<\/script>?/g)
	     console.log(script_match)
	     if(script_match)
	       for(var i = 0; i < script_match.length; i++){
		 var to_eval = script_match[i].replace("<scr"+"ipt>\n//<![CDATA[","").replace("//]]>\n</scr"+"ipt>","") 
		 console.log("Evaluating:", to_eval)
	         eval(to_eval)
	       }
	     }, 1)
		      
   @getEl{@id}.innerHTML = @stuff
 })

(define (js-val id)
  @js{@getEl{"@(ns id)".trim()}.value})


(define (js/call f #:then (then (const ""))
		 . args)
  (js-fetch "GET"
	    (apply js-url/stateless/call f args)
	    #:then then))


(define (js/call-hash f h #:then (then (const "")))
  (js-fetch "GET"
	    (js-url/stateless/call-hash f)
	    #:then then))

