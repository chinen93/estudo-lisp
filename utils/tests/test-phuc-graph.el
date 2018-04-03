;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests for phuc-graph.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load (expand-file-name "../phuc-graph.el") 'noerror 'nomessage)

(ert-deftest compare-nodes-greater ()
  (should (equal (phuc-compare-nodes (make-phuc-node :value 2)
				     (make-phuc-node :value 1))
		 1)))

(ert-deftest compare-nodes-equal ()
  (should (equal (phuc-compare-nodes (make-phuc-node :value 1)
				     (make-phuc-node :value 1))
		 0)))

(ert-deftest compare-nodes-lesser ()
  (should (equal (phuc-compare-nodes (make-phuc-node :value 1)
				     (make-phuc-node :value 2))
		 -1)))

(ert-deftest compare-nodes-error-second-parameter ()
  (should-error (phuc-compare-nodes (make-phuc-node :value 1) 
				    1)))

(ert-deftest compare-nodes-error-first-parameter ()
  (should-error (phuc-compare-nodes 1
				    (make-phuc-node :value 1))))

(ert-deftest create-graph ()
  (should (equal (phuc-graph-p (phuc-graph-create)) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar test-graph nil "Graph to be used on tests")
(defvar test-node nil "Node to be used on tests")

(defmacro with-test-setup-teardown (setup teardown &rest body)
  `(unwind-protect
       (progn ,setup
	      ,body)
     ,teardown))

(defun test-setup-add-graph ()
  (setq test-graph (phuc-graph-create))
  (setq test-node (make-phuc-node :value 1)))

(defun test-teardown-add-graph ()
  (setf test-graph nil)
  (setf test-node nil))

;; (with-test-setup-teardown 'test-setup-add-graph  
;; 			  'test-teardown-add-graph 
;; 			  (lambda () 
;; 			    (phuc-graph-show (phuc-graph-add-node test-graph 
;; 								  test-node))))

(ert-deftest add-node-graph ()
  (should (equal (with-test-setup-teardown 'test-setup-add-graph  
					   'test-teardown-add-graph 
					   (lambda () 
					     (phuc-graph-show (phuc-graph-add-node test-graph 
										   test-node))))
		 '(([cl-struct-phuc-node 1])(())))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest show-graph ()
  (should (equal (phuc-graph-show (phuc-graph-create))
		 '(()()))))

;; End of tests.
