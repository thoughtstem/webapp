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

	  (include-js "https://mc-social-media.s3-us-west-1.amazonaws.com/js/jsuites.js")
          (include-js "https://cdnjs.cloudflare.com/ajax/libs/jexcel/3.9.1/jexcel.js")

	  #;
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
	          if(!window.jexcel){ 

		  var jexcel = document.createElement("script");
		  jexcel.src = "https://cdnjs.cloudflare.com/ajax/libs/jexcel/3.9.1/jexcel.js"
		  jexcel.type= 'text/javascript'
		  jexcel.onload=function(){
		    var jsuites = document.createElement("script");
		    jsuites.src = "https://mc-social-media.s3-us-west-1.amazonaws.com/js/jsuites.js";
		    jsuites.type= 'text/javascript'
		    jsuites.onload = function(){
		      @(call 'setupSpreadsheet)
		    }
		    document.head.appendChild(jsuites);
		  }
		  document.head.appendChild(jexcel);


		  } else {
		    @(call 'setupSpreadsheet)
		  }
		}()
	       }])

       (function (setupSpreadsheet)
         @js{		 
	        console.log("setting up spreadsheet")
	        var data = @(jsexpr->string data)
	        jexcel(@getEl{@spreadsheet}, {
					     data:data,
					     columns: @(jsexpr->string columns)
					     });
	  })
       )))
