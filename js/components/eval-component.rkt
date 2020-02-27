#lang at-exp web-server

(provide eval-component)

(require webapp/js
         webapp/models/util
         webapp/server/client-communication)

(define (eval-component text-value
                        edit-function
                        module-name
                        #:pre-content (pre-content #f))

  (define rendered-value
    (string->component text-value module-name))


  (enclose
    (span id: (ns "main")
	  (include-js "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.32.0/codemirror.min.js")
	  (include-css "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.32.0/codemirror.min.css")

	  (card
	    (card-body
	      pre-content 
	      (textarea id: (ns "input")
			text-value)

	      (hr)

	      (div id: (ns "output")
		   rendered-value) )))
    (script ([input  (ns "input")]
	     [output (ns "output")]
	     [main   (ns "main")]
	     [editor
	       @js{
	       function(){
	          if(!window.CodeMirror){ //Load it if the include-js above didn't work, which happens if the component is injected after the page loads.  The script tag doesn't run
  
                  fetch("https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.32.0/codemirror.min.js").then((r)=>r.text()).then((t)=>{
															       eval(t); 
															       @editor = @(call 'setupEditor) 
															       })

		  } else {
		    return @(call 'setupEditor)
		  }
		}()
	       }])

       (function (setupEditor)
         @js{		 
	  var editor = CodeMirror.fromTextArea(@getEl{@input}, { lineNumbers: true });
	  editor.on("change",
		    ()=>@(call 'rerender))

	  return editor })

       (function (rerender)
         (js/call
	   (lambda (val)
	     (edit-function val)
	     (string->component val module-name))
	   @js{@editor .getValue()}
	   #:then (callback 'updateUI)))

       (function (updateUI newUI)
         (js-inject output newUI)))))



(define (string->component s module-name)
  (dynamic-require module-name #f)
  (if (not (string-prefix? s "("))
      (div s)
      ;Otherwise, try to read it as code:
      (with-handlers ([exn:fail? 
			(lambda (e)
			  (div class: "alert alert-danger"
			       (p "There was an error with the code.")
			       (pre s)

			       (div class: "alert alert-warning"
				    (exn-message e))))])

		     (with-read-only-database
		       (eval
			 (read (open-input-string s))
			 (module->namespace
                          module-name))))))
