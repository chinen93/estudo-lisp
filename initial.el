;;========================================
;; Initial touch with the language
;;========================================

;; simple arithmetics
(+ 20 2)
(- 2  1)
(* 2  2)
(/ 2  1)
;; (/ 2  0) ERROR

(cl-labels ((soma10 (n)
		    (+ n 10))
	    (subtrai3 (n)
		      (- n 3)))
  (soma10 (subtrai3 5)))
