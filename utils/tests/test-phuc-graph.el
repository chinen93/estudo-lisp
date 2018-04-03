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

;; End of tests.
