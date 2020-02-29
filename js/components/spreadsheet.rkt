#lang at-exp web-server

(provide spreadsheet)

(require webapp/js
         webapp/models/util
         webapp/server/client-communication
	 json)

(define (spreadsheet 
	  #:columns (columns (list (hash 'type "text" 'title "Unnamed Column")))
	  #:data    (data (list)))
  (enclose
    (span id: (ns "main")
	  ;Works after injection?

	  (include-js "https://bossanova.uk/jsuites/v2/jsuites.js")
	  (include-js "https://bossanova.uk/jexcel/v3/jexcel.js")
	  (include-css 
	    "https://bossanova.uk/jexcel/v3/jexcel.css")
	  (include-css 
	    "https://bossanova.uk/jsuites/v2/jsuites.css")

	  (div id: (ns "spreadsheet")))
    (script ([spreadsheet  (ns "spreadsheet")]
	     [main   (ns "main")]

	     [dummy
	       @js{
	       function(){
	        var data = @(jsexpr->string data)
	        jexcel(@getEl{@spreadsheet}, {
					     data:data,
					     columns: @(jsexpr->string columns)
					     });
		}()
	       }])

	    #;
       (function (setupEditor)
         @js{		 
	  var editor = CodeMirror.fromTextArea(@getEl{@input}, { lineNumbers: true });
	  editor.on("change",
		    ()=>@(on-change @js{editor.getValue()}))

	  return editor })
       )))
