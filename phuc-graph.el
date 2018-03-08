
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct phuc-graph
  nodes
  edges)

(defun phuc-graph-create (nodes edges)
  "Create a graph G=(NODES EDGES). Both NODES and EDGES must be lists with the 
same length. Each element i of EDGES refers to edges of the node i. If a node
has no edges then its element is ().

Example: ((1 2 3 4) . ((1 2) (1) (2 3) (1)))
"
  )
  

(provide 'phuc-graph)
;;; end of phuc-graph.el
