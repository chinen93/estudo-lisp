
;;; DOC
;;
;; Graph oriented represented by a list of nodes and its adjacents.
;; 
;; Graph will save nodes into a hash table to fast access.
;;
;; Example:
;; ((1 2 3 4) . ((1 2) (1) (2 3) (1)))


;;; CODE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst +phuc-distance+ 1
  "Distance between two nodes in the a phuc-graph.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Node
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct phuc-node
  value)

(defun phuc-compare-nodes (nodeX nodeY)
  "Compare a pair of nodes and tell something about their relationship:

- NODEX > NODEY returns 1
- NODEX = NODEY returns 0
- NODEX < NODEY returns -1"

  ;; Check if parameters are the expected ones.
  (unless (and (phuc-node-p nodeX) (phuc-node-p nodeY))
    (error "nodeX and nodeY must be phuc-node"))
  
  (let ((valX (phuc-node-value nodeX))
        (valY (phuc-node-value nodeY)))
    (cond
     ((< valX valY) -1)
     ((> valX valY) 1)
     (t 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct phuc-graph
  nodes
  edges)

(defun phuc-graph-show (graph)
  "Just return a list as (nodes edges)"

  (list (phuc-graph-nodes graph) (phuc-graph-edges graph)))

(defun phuc-graph-create ()
  "Create a graph G=(NODES EDGES). Both NODES and EDGES must be lists with the 
same length. Each element i of EDGES refers to edges of the node i. If a node
has no edges then its element is ().

Example: ((1 2 3 4) . ((1 2) (1) (2 3) (1)))
"
  (make-phuc-graph :nodes () :edges ()))

(defun phuc-graph-add-node (graph node)
  "Add NODE into GRAPH.

So nodes of GRAPH will have a new element and the edges will have an element as well."

  (let ((nodes (phuc-graph-nodes graph))
        (edges (phuc-graph-edges graph)))
    (setf (phuc-graph-nodes graph) (append nodes (list node))) 
    (setf (phuc-graph-edges graph) (append edges (list nil))) 
    graph))

(provide 'phuc-graph)
;;; end of phuc-graph.el
