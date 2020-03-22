#lang at-exp web-server

(provide click-to-expand
	 click-to-replace
	 click-to-call
	 catch)

(require webapp/js
	 webapp/models/util
	 webapp/server/util/stateless-serve-function 
	 webapp/server/client-communication
	 json)



;For any kind of interaction that starts small
; and then gets big.
;TODO: Could add a click to shrink after expanding.
(define (click-to-expand small-chip
			 component-function)

  (enclose
    (span
      (span
	id: (ns "main")
	on-click: (call 'expand)
	style: (properties cursor: "pointer")
	small-chip)
      (hr)
      (span id: (ns "expandedArea")))

    (script ([main (ns "main")]
	     [expandedArea (ns "expandedArea")])

	    (function (expand)
		      (js-fetch "GET"
				(js-url/stateless/call component-function)
				#:then (callback 'updateUI)))

	    (function (updateUI newUI)
		      (js-inject expandedArea newUI)) )))



(define (click-to-replace to-click f)

  (enclose
    (span
      (span
	id: (ns "main")
	on-click: (call 'act)
	style: (properties cursor: "pointer")
	to-click))

    (script ([main (ns "main")])

	    (function (act)
		      (js/call 
			(lambda ()
			  (catch (f)))

			#:then (callback 'updateUI)))

	    (function (updateUI newUI)
		      (js-inject main newUI)))))


(define-syntax-rule (catch statements ...)
  (with-handlers ([exn:fail? (lambda (e)
			       (alert-danger
				 "There was an error"
				 (alert-warning
				   (code
				     (pre
				       (exn-message e))))))
			       ])
		 
    statements ...))


(define (click-to-call to-click f)

  (enclose
    (span
      (span
	id: (ns "main")
	on-click: (call 'act)
	style: (properties cursor: "pointer")
	to-click))

    (script ([main (ns "main")])

	    (function (act)
		      (js/call 
			(lambda ()
			  (catch (f)))

			#:then (callback 'updateUI)))

	    (function (updateUI newUI)
		      ;Nothing to do here.
		      ;Confirmation?
		      ))))


