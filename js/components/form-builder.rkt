#lang at-exp web-server

(define (fa-check)
  ;TODO: Why can't we use thunk*?
  (lambda ([a #f] . args) 
    ;TODO: Move this out to website
    (i class: "fas fa-check"))
  )

(provide editable-text-field
	 editable-text-area
	 editable-time-field
	 editable-time-of-day-field
	 editable-date-field
	 editable-toggle-field
	 editable-dropdown-field
	 update-field-section
	 
	 year-select
	 month-select
	 day-select
	 hour-select
	 minute-select
	 )

(require 
  webapp/js
  webapp/models/util
  webapp/server/client-communication
  (prefix-in w: (only-in website-js select)) ;Need select in this file.  Prefix fixes the name collision with webapp/models/util's db select function
  )

#;
(require (except-in website-js select address)
	 
	;mc-data/views/js-util
	 ;mc-data/views/components/util
;	 mc-data/models
	 )


(define (update-field-section 
	  x
	  the-label
	  render-function ;Takes a current val and update function.  Should call the update function as necessary
	  getter
	  setter
	  finder)

  (define x-id (get x 'id))

  (div class: "form-group"
       (label the-label)

       (render-function 
	 (getter x)

	 (lambda (val) 
	   ((compose update-one!
		     (curry setter 
			    (finder x-id)
			    )) 
	    val)))))


(define (editable-time-field 
	  current-value  ;Should be a moment.  
	  edit-function)
  (local-require gregor)

  (editable-field 
    (lambda ()
      ;These are outside the enclosure so they can refer to the function in the editable-field's script area,
      ;  And so that the input's id will be in the editable-field's namespace
      (define show-edit (callback 'show_edit))
      (define input-id (ns "input"))

      (enclose
	(div
	  (input id: input-id
		 type: "hidden"
		 'value: "")

	  (div
	    (year-select 2020 2025
			 #:selected (~t current-value "YYYY")
			 #:on-change (call 'consolidateInput 'this "year"))
	    "/"

	    (month-select 
	      #:selected (~t current-value "MMMM")
	      #:on-change (call 'consolidateInput 'this "month"))

	    "/"
	    (day-select
	      #:selected (~t current-value "dd")
	      #:on-change (call 'consolidateInput 'this "day")))

	  (div
	    (hour-select
	      #:selected (~t current-value "HH")
	      #:on-change (call 'consolidateInput 'this "hour"))

	    ":"

	    (minute-select
	      #:selected (~t current-value "mm")
	      #:on-change (call 'consolidateInput 'this "minute"))))

	(script ([timeHash 
		   @js{
		   {
		   year: @(~s (~t current-value "YYYY")),
		   month: @(~s (~t current-value "MMMM")),
		   day: @(~s (~t current-value "dd")),
		   hour: @(~s (~t current-value "HH")),
		   minute: @(~s (~t current-value "mm"))
		   }
		   }]
		 [mainInput input-id]

		 ;Hack to call the js function to set the initial value in the hidden field.
		 [dummy @js{@(ns 'updateMainInput)(@timeHash)}])
		(function (consolidateInput elem part)
			  @js{@timeHash [@part] = @elem .options[ @elem .selectedIndex] .value}
			  @js{@updateMainInput(@timeHash)}
			  (show-edit))
		(function (updateMainInput h)
			  @js{@getEl{@mainInput} .value = 
			  @h .year + "-" + 
			  @h .month + "-" + 
			  @h .day + "-" + 
			  @h .hour + "-" +
			  @h .minute
			  }))))

    ;for stateless, had to convert from curryr to this.  Remember: lambdas are magic
    (let ([tz (->timezone current-value)])
      (lambda (client-val)
	(edit-function
	  (parameterize ([current-timezone tz])
	    (define m 
	      (parse-moment client-val "y-MMMM-dd-HH-mm"))

	    m))))))

(define (editable-time-of-day-field 
	  current-value  ;Should be a gregor time
	  edit-function)
  (local-require gregor)

  (editable-field 
    (lambda ()
      ;These are outside the enclosure so they can refer to the function in the editable-field's script area,
      ;  And so that the input's id will be in the editable-field's namespace
      (define show-edit (callback 'show_edit))
      (define input-id (ns "input"))

      (enclose
	(div
	  (input id: input-id
		 type: "hidden"
		 'value: "")

	  (div
	    (hour-select
	      #:selected (~t current-value "HH")
	      #:on-change (call 'consolidateInput 'this "hour"))

	    ":"

	    (minute-select
	      #:selected (~t current-value "mm")
	      #:on-change (call 'consolidateInput 'this "minute")))

	  )

	(script ([timeHash 
		   @js{
		   {
		   hour: @(~s (~t current-value "HH")),
		   minute: @(~s (~t current-value "mm"))
		   }
		   }]
		 [mainInput input-id]

		 [dummy @js{@(ns 'updateMainInput)(@timeHash)}])
		(function (consolidateInput elem part)
			  @js{@timeHash [@part] = @elem .options[ @elem .selectedIndex] .value}
			  @js{@updateMainInput(@timeHash)}
			  (show-edit))
		(function (updateMainInput h)
			  @js{@getEl{@mainInput} .value = 
			  @h .hour + "-" +
			  @h .minute
			  }))))

    ;for stateless, had to convert from curryr to this.  Remember: lambdas are magic
    (lambda (client-val)
      (edit-function
	(parse-time client-val "HH-mm")))
    ))

(define (editable-dropdown-field . options)
  (lambda (current-value edit-function)
    (editable-field 
      (lambda ()
	(define show-edit (callback 'show_edit))
	(define input-id (ns "input"))

	(enclose
	  (div
	    (input id: input-id
		   type: "hidden"
		   'value: "")

	    (w:select 
	      id: (ns "dropdown")
	      'onChange: (call 'propagateToHidden)
	      (map 
		(curryr
		  basic-selection-option 
		  current-value)
		options)))

	  (script ([mainInput input-id]
		   [dropdown (ns "dropdown")])
            (function (propagateToHidden)
	      @js{console.log(@dropdown)}
              @js{@getEl{@mainInput} .value = @getEl{@dropdown} .options[ @getEl{@dropdown} .selectedIndex] .value}
	      (show-edit)
	      ))))

      edit-function)))

(define (editable-date-field 
	  current-value  ;Should be a gregor date  
	  edit-function)
  (local-require gregor)

  (editable-field 
    (lambda ()
      ;These are outside the enclosure so they can refer to the function in the editable-field's script area,
      ;  And so that the input's id will be in the editable-field's namespace
      (define show-edit (callback 'show_edit))
      (define input-id (ns "input"))

      (enclose
	(div
	  (input id: input-id
		 type: "hidden"
		 'value: "")

	  (div
	    (year-select 1950 2025
			 #:selected (~t current-value "YYYY")
			 #:on-change (call 'consolidateInput 'this "year"))
	    "/"

	    (month-select 
	      #:selected (~t current-value "MMMM")
	      #:on-change (call 'consolidateInput 'this "month"))

	    "/"
	    (day-select
	      #:selected (~t current-value "dd")
	      #:on-change (call 'consolidateInput 'this "day")))

	  )

	(script ([timeHash 
		   @js{
		   {
		   year: @(~s (~t current-value "YYYY")),
		   month: @(~s (~t current-value "MMMM")),
		   day: @(~s (~t current-value "dd")),
		   }
		   }]
		 [mainInput input-id]

		 [dummy @js{@(ns 'updateMainInput)(@timeHash)}])
		(function (consolidateInput elem part)
			  @js{@timeHash [@part] = @elem .options[ @elem .selectedIndex] .value}
			  @js{@updateMainInput(@timeHash)}
			  (show-edit))
		(function (updateMainInput h)
			  @js{@getEl{@mainInput} .value = 
			  @h .year + "-" + 
			  @h .month + "-" + 
			  @h .day}))))

    ;for stateless, had to convert from curryr to this.  Remember: lambdas are magic
    (lambda (client-val)
	(edit-function
	  (parse-moment client-val "y-MMMM-dd")))
    ))


(define (basic-selection-option v (selected #f))
  (apply option 'value: v 
	 (flatten
	   (list
	     (when (string=? (~a v) (~a selected))
	       (list 'selected: #t))
	     v))))


(define (year-select start end 
		     #:on-change (on-change noop)
		     #:selected (selected #f))
  (define options 
    (map 
      (curryr basic-selection-option selected)
      (range start end)))
  (w:select 'onChange: on-change 
	    options))

(define (day-select #:on-change (on-change noop)
		    #:selected (selected #f))
  (define options 
    (map 
      (curryr basic-selection-option selected)
      (map
	(curry ~a #:min-width 2 #:left-pad-string "0" #:align 'right)
	(range 1 32))))

  (w:select 'onChange: on-change 
	    options))

(define (month-select 
	  #:on-change (on-change noop)
	  #:selected (selected #f))

  (define options 
    (list
      (basic-selection-option "January" selected)
      (basic-selection-option "February" selected)
      (basic-selection-option "March" selected)
      (basic-selection-option "April" selected)
      (basic-selection-option "May" selected)
      (basic-selection-option "June" selected)
      (basic-selection-option "July" selected)
      (basic-selection-option "August" selected)
      (basic-selection-option "September" selected)
      (basic-selection-option "October" selected)
      (basic-selection-option "November" selected)
      (basic-selection-option "December" selected)))

  (w:select 'onChange: on-change 
	    options))

(define (hour-select #:on-change (on-change noop)
		     #:selected (selected #f))
  (define options 
    (map 
      (curryr basic-selection-option selected)
      (map
	(curry ~a #:min-width 2 #:left-pad-string "0" #:align 'right)
	(range 0 24))))

  (w:select 'onChange: on-change 
	    options))

(define (minute-select #:on-change (on-change noop)
		       #:selected (selected #f))
  (define options 
    (map 
      (curryr basic-selection-option selected)
      (map 
	(curry ~a #:min-width 2 #:left-pad-string "0" #:align 'right)
	(range 0 60))))

  (w:select 'onChange: on-change 
	    options))

(define (editable-text-area current-value
			    edit-function)
  (editable-field
    (thunk*
      (textarea 
	'onFocus: (call 'show_edit) ;Will break...
	id: (ns "input")
	class: "form-control"
	current-value))
    edit-function))


(define (editable-text-field current-value 
			     edit-function)
  (editable-field 
    (thunk*
      (input 'onFocus: (call 'show_edit) ;Will break...
	     id: (ns "input")
	     type: "text"
	     class: "form-control"
	     'value: current-value))

    edit-function ))

(define (editable-toggle-field current-value 
			       edit-function
			       #:yes (yes #t)
			       #:no (no #f))
  (editable-field 
    (thunk*
      (define show-edit (call 'show_edit))
      (define inputId (ns "input"))
      (enclose
	(input 'onFocus: show-edit 
	       on-click: (call 'switch)
	       id: inputId
	       type: "checkbox"
	       class: "form-control"
	       'checked: (equal? current-value yes)
	       'value: current-value)
	(script ([state (if (equal? current-value yes)
			    'true 'false)]
		 [input inputId])
	  (function (switch)
	    @js{@state = !@state}
	    @js{if(@state){
	          @getEl{@input}.value = @yes
	        }else{
	          @getEl{@input}.value = @no
		}}
	    ))))

    edit-function ))


(define (editable-field field-function 
			edit-function)
  
  
  (enclose 
    (span
      (field-function)
      (badge-pill-warning 
	id: (ns "save_button")
	style: (properties display: "none"
			   cursor: "pointer")
	on-click: (call 'save)
	"Save"))

    (script ([input (ns "input")]
	     [save_button (ns "save_button")])

	    (function (show_edit)
		      @js{@getEl{@save_button}.innerHTML = "Save?"}
		      @js{ @getEl{@save_button}.style.display = "inline" })

	    (function (save)
		      @js{
		      var val = @getEl{@input}.value
		      @(js/call
			 (lambda (val) 
			   (define result (edit-function val))

			   (if (model-error? result) ;make abstraction
			       (badge-pill-danger
				 (exn-message
				   (model-error-e 
				     result)))
			       (fa-check)))
			 'val
			 #:then
			 (callback 'doneSave))
		      })

	    (function (doneSave newUI)
		      @js{@getEl{@save_button}.innerHTML = newUI}
		      @js{setTimeout(()=>$(@getEl{@save_button}).fadeOut(), 7000)}))))





