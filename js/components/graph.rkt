#lang at-exp web-server

;TODO: This component still breaks on injections into a note.
;  If the js is loaded beforehand it works, but if not... white screen.

(provide graph-component
	 graph-editor-component
	 node->xy
	 node->id
	 on-dragfreeon
	 
	 layout
	 cose-layout
	 preset-layout
	 dagre-layout
	 )

(require webapp/js
         webapp/models/util
         webapp/server/client-communication
	 graph)


(define current-graph (make-parameter #f))

;Do these need to be web parameters??
(define color1 (make-parameter "black"))
(define color2 (make-parameter "orange"))

(define on-dragfreeon (make-parameter #f))




(define (cose-layout)
  @js{
  layout: {
   name: 'cose',
   nodeDimensionsIncludeLabels: true,
   fit: false,
   padding: 5,
   nestingFactor: 5.0,
   nodeRepulsion: 4096,
   nodeOverlap: 100,
   idealEdgeLength: 100,
   randomize: false,
   edgeElasticity: 128,
   }
  })

(define (preset-layout)
  @js{
  layout: {
   name: 'preset',
  }})

(define (dagre-layout)
  @js{
  layout: {
   name: 'dagre',
   ranker: 'longest-path',
   rankDir: 'BT',
   nodeSep: 150
  }})

(define layout
  (make-parameter
    (preset-layout)))

;Incomplete, but would provide hooks into graph-component that could be propagated out to other components that are basically re-renderings/re-compilings of a graph data structure.
(define (graph-editor-component g)
  ;Proof of concept. TODO: Pass back the full graph structure on each callback to server?
  ;  Otherwise, have to shadow it here...
  (parameterize ([on-dragfreeon 
		   (lambda (n x y) 
		     (displayln "Node dropped!")
		     (div n ": " x ", " y))])
  (enclose
    (div
      (span id: (ns "output"))
      (graph-component g
		       #:on-dragfreeon (callback 'handleOnDragfreeon)))
    (script ([output (ns "output") ])
	    (function (handleOnDragfreeon newUI)
	      (js-inject output newUI))
	    ))))

(define (graph-component g
			 #:on-dragfreeon (js-on-dragfreeon noop))

  (parameterize ([current-graph g])
    (enclose
      (div
	(include-js "https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.14.0/cytoscape.min.js")
	(include-js "https://unpkg.com/dagre@0.7.4/dist/dagre.js")
	(include-js "https://cytoscape.org/cytoscape.js-dagre/cytoscape-dagre.js")
	(div id: (ns "cytoscape")
	     class: "d-block border border-secondary w-100 my-3"
	     style: (properties height: 720)))
      (script ([cytoscape (ns "cytoscape")]
	       [dummy 
		 @js{
		 function(){
		 if(window.cytoscape){
		 @(call 'construct)
		 }else{
		 @(late-include-js "https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.14.0/cytoscape.min.js" 
				   (late-include-js "https://unpkg.com/dagre@0.7.4/dist/dagre.js"
						    (late-include-js "https://cytoscape.org/cytoscape.js-dagre/cytoscape-dagre.js"
								     @js{
								     @(call 'construct)
								     })))        
		 }
		 }()
		 }])
	      (function (construct)
			@js{
			console.log("CONSTRUCT") 
			var cy = cytoscape({

					    container: @getEl{@cytoscape}, // container to render in

					    boxSelectionEnabled: false,

					    style: [
						    {
						    selector: 'node',
						    css: {
						    'shape': 'round-rectangle',
						    'content': 'data(id)',
						    'text-valign': 'center',
						    'text-halign': 'center'
						    }
						    },
						    {
						    selector: ':parent',
						    css: {
						    'text-valign': 'top',
						    'text-halign': 'center',
						    }
						    },
						    {
						    selector: 'edge[label]',
						    css: {
						    'curve-style': 'bezier',
						    'target-arrow-shape': 'triangle',
						    'label': 'data(label)'
						    }
						    },
						    {
						    "selector": ".autorotate",
						    "style": {
						    "edge-text-rotation": "autorotate"
						    }
						    },
						    {
						    "selector": ".outline",
						    "style": {
						    "color": "#fff",
						    "text-outline-color": "#888",
						    "text-outline-width": 2
						    }
						    },
						    {
						    "selector": ".color1",
						    "style": {
						    "text-outline-color": "@(color1)",
						    }
						    },
						    {
						    "selector": ".color2",
						    "style": {
						    "text-outline-color": "@(color2)",
						    }
						    }
						    ],

					    elements: {
					    nodes: [
						    @(string-join (map node->cyto-node (get-vertices g)) ",\n")

						    ],
					    edges: [
						    @(string-join (map edge->cyto-edge
								       (get-edges g)) ",\n")

						    ]
					    },


					    @(layout),

					    wheelSensitivity: 0.2

					    });


			@(if (on-dragfreeon)
			     @js{
			     cy.on('dragfreeon', 'node', function(evt){
				   @(js/call
				      (on-dragfreeon)
				      @js{evt.target.id()}
				      @js{evt.target.position().x}
				      @js{evt.target.position().y}
				      #:then js-on-dragfreeon)
				   })
			     }

			     @js{})
			}
			))))
  )


(define node->id 
  (make-parameter 
    (lambda (n)
      (~a n))))

(define edge->id 
  (make-parameter 
    (lambda (e)
      (~a ((node->id) 
	   (first e)) "->" 
	  ((node->id) 
	   (second e))))))

(define edge->label
  (make-parameter 
    (lambda (e)
      (edge-weight
	(current-graph)
	(first e)
	(second e)))))

(define node->color
  (make-parameter 
    (lambda (n)
      "color1"  ;I don't like this.  Don't provide node->color.  It's not working anyway...
      )))

(define node->xy
  (make-parameter 
    (lambda (n)
      (list
	(random 0 100)
	(random 0 100)
	))))

(define (edge->cyto-edge e)
  @~a{
      { data: { id: '@((edge->id) e)', 
        source: '@((node->id) (first e))', target: '@((node->id) (second e))', 
	label: '@((edge->label) e)' }, classes: 'autorotate outline'
	}
  })

(define (node->cyto-node n)
  @~a{
      { data: { id: '@((node->id) n)'}, 
        classes: 'outline @((node->color) n)',
	renderedPosition:{
          x: @(first  ((node->xy) n)),
          y: @(second ((node->xy) n))
	}
      }
  })





