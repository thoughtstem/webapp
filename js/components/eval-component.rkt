#lang at-exp web-server

(provide eval-component
	 string->component
	 ;Move this...
	 editor-component)

(require webapp/js
         webapp/models/util
         webapp/server/client-communication)


(define (editor-component initial-value
			  #:on-change (on-change #f))
  (enclose
    (span id: (ns "main")
	  (include-js "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.32.0/codemirror.min.js")
	  (include-js "https://codemirror.net/mode/scheme/scheme.js")
	  (include-css "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.32.0/codemirror.min.css")

	  (textarea id: (ns "input")
		    initial-value))
    (script ([input  (ns "input")]
	     [main   (ns "main")]
	     [editor
	       @js{
	       function(){
	          if(!window.CodeMirror){ //Load it if the include-js above didn't work, which happens if the component is injected after the page loads.  The script tag doesn't run
  

		  @(late-include-js "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.32.0/codemirror.min.js"
				    (late-include-js
				      "https://codemirror.net/mode/scheme/scheme.js"
				      (call 'setupEditor)))

		  /*
		  var codemirror = document.createElement("script");
		  codemirror.src = "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.32.0/codemirror.min.js"
		  codemirror.type= 'text/javascript'
		  codemirror.onload=function(){
		    var scheme = document.createElement("script");
		    scheme.src = "https://codemirror.net/mode/scheme/scheme.js";
		    scheme.type= 'text/javascript'
		    scheme.onload = function(){
		      @(call 'setupEditor)
		    }
		    document.head.appendChild(scheme);
		  }
		  document.head.appendChild(codemirror);
		  */

		  } else {
		    return @(call 'setupEditor)
		  }
		}()
	       }])

       (function (setupEditor)
         @js{		 
	  var editor = CodeMirror.fromTextArea(@getEl{@input}, { lineNumbers: true });

	  @(if on-change
	     @js{
	     editor.on("change",
		       ()=>@(on-change @js{editor.getValue()}))}
	     @js{})

	  editor.setOption("mode", "scheme");

	  return editor })
       )))

(define (eval-component text-value
                        edit-function
                        module-name
			#:eval? (eval? #t)
			#:wrapper (wrapper (lambda (x) x))
                        #:pre-content (pre-content #f))

  (define rendered-value
    (if eval? 
	(string->component text-value module-name wrapper)
	"[Non-eval mode on]"))


  (enclose
    (span id: (ns "main")
	  (card
	    (card-body
	      pre-content 
	      (editor-component text-value
				#:on-change (callback 'rerender))

	      (hr)

	      (div id: (ns "output")
		     rendered-value ))))
    (script ([output (ns "output")]
	     [main   (ns "main")])
       (function (rerender val)
         (js/call
	   (lambda (val)
	     (edit-function val)
	     (if eval?
		 (string->component val module-name wrapper)
		 ""))
	   val
	   #:then (callback 'updateUI)))
       
       (function (updateUI ui)
		 (js-inject output ui)))))



(require scribble/reader)
(define (string->component s module-name (wrapper identity))
  (dynamic-require module-name #f)
  (if (and (not (string-prefix? s "("))
	   (not (string-prefix? s "@")))
      (div s)
      ;Otherwise, try to read it as code:
      (with-handlers ([exn:fail? 
			(lambda (e)
			  (div class: "alert alert-danger"
			       (p "There was an error with the code.")

			       (div class: "alert alert-warning"
				    (pre (exn-message e)))))])

		     (with-read-only-database
		       (wrapper
			 (eval
			   (parameterize ([current-readtable (make-at-readtable)])
			     (read (open-input-string (string-append "(let () " s ")"))))
			   (module->namespace
			     module-name)))))))

