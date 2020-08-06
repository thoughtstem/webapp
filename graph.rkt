#lang at-exp web-server

;TODO: Refactor this to use webapp/js/components/graph
(provide cytoscape-overview
	 relation->models
	 get-all-models)

(require webapp/js
	 english)

(define (cytoscape-overview all-models all-relations [color1 "black"] [color2 "gray"])
  (local-require racket/hash)
  (set! all-models (map ~a all-models))

  (enclose
    (div
      (p "Below is an interactive graph of all data models and relations. Click and drag the nodes to move them around. Use the mouse wheel to zoom in and out.")
      (include-js "https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.14.0/cytoscape.min.js")
      (div id: (ns "cytoscape")
           class: "d-block border border-secondary w-100 my-3"
	   style: (properties height: "100vh")))
    (script ([cytoscape (ns "cytoscape")]
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
    'text-halign': 'center',
    'text-wrap' : 'wrap'
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
    "text-outline-color": "@color1",
   }
   },
  {
   "selector": ".color2",
   "style": {
    "text-outline-color": "@color2",
   }
  }
  ],

  elements: {
   nodes: [
     @(string-join (map model->node all-models) ",\n")
	  	  
   ],
   edges: [
     @(string-join (map relation->edge 
			(hash->list all-relations)) ",\n")
     
   ]
   },

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

  wheelSensitivity: 0.2

  });
   
}
))))




(define (relation->models relation)
  (define id     (~a (car relation)))
  (define type   (cdr relation))

  (define source 
    (plural (first  (string-split id "->"))))

  (define target 
    (cond [(not (eq? type 'has-many))
	   (plural (second (string-split id "->")))]
	  [else 
	    (second (string-split id "->"))]))

  (list source target))

(define (get-all-models relations #:mode [mode 'default])
  (remove-duplicates (flatten (map (curry relation->models #:mode mode) (hash->list relations)))))

(define (model->node model)
  (define id     model)
  (define color (if (string-contains? model "assignment")
                    "color2"
                    "color1"))
  @~a{
      { data: { id: '@id'}, classes: 'outline @color'}
      }
  )

(define (relation->edge relation)
  (define id     (~a (car relation)))
  (define type   (string-replace (~a (cdr relation)) "-" " "))
  (define source 
    (plural (first  (string-split id "->"))))

  (define target 
    (cond [(not (eq? type "has many"))
	   (plural (second (string-split id "->")))]
	  [else 
	    (second (string-split id "->"))]))

  @~a{
      { data: { id: '@id', source: '@source', target: '@target', label: '@type' }, classes: 'autorotate outline' }
      }
  )




