#lang at-exp web-server

(provide graph-component
	 node->xy)

(require webapp/js
         webapp/models/util
         webapp/server/client-communication
	 graph)

(define color1 (make-parameter "black"))
(define color2 (make-parameter "orange"))

(define (graph-component g)

  (enclose
    (div
      (include-js "https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.14.0/cytoscape.min.js")
      (div id: (ns "cytoscape")
           class: "d-block border border-secondary w-100 my-3"
	   style: (properties height: 720)))
    (script ([cytoscape (ns "cytoscape")]
	     ;TODO: Support late-load/injection...
             [dummy (call 'construct)])
            (function (construct)
                      @js{
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

   /*
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
   },
   */

   layout: {
     name: 'preset',
   },

  wheelSensitivity: 0.2

  });
   
}
)))
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
      "")))

(define node->color
  (make-parameter 
    (lambda (n)
      "black")))

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





