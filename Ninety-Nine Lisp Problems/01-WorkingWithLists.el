;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Working with lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; P01 (*) Find the last box of a list.
; Example:
; * (my-last '(a b c d))
; (D)

;; my-last list
(defun my-list (list)
  
  ;; check if the parameter list is null.
  (if (null list)
      
      ;; if is: return null.
      nil
    
    ;; if is not:  check if cdr of list is null.
    (if (null (cdr list))
	
	;; if is: return list.
	list
      
      ;; if is not: call the function recursively with
      ;;            the cdr of the list as parameter.
      (my-list (cdr list)))))


;;; Tests
(ert-deftest my-list-01 ()
  (should (equal (my-list '(a b c d)) '(d))))

(ert-deftest my-list-02 ()
  (should (equal (my-list '()) '())))

(ert-deftest my-list-03 ()
  (should (equal (my-list '(a)) '(a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; P02 (*) Find the last but one box of a list.
; Example:
; * (my-but-last '(a b c d))
; (C D)

;; my-but-last list
(defun my-but-last (list)

  ;; check if the paraeter list is null.
  (if (null list)

      ;; if is: return null.
      nil

    ;; check if the cdr of parameter list is null.
    (if (null (cdr list))

	;; if is: return null.
	nil

      ;; check if the cdr of the cdr is null.
      (if (null (cdr (cdr list)))

	  ;; if is: return rest of list.
	  list

	;; if is not: call the function recursively with
	;;            the cdr of the list as parameter.
	(my-but-last (cdr list))))))

;;; Tests
(ert-deftest my-but-last-01 ()
  (should (equal (my-but-last '(a b c d)) '(c d))))

(ert-deftest my-but-last-02 ()
  (should (equal (my-but-last '()) '())))

(ert-deftest my-but-last-03 ()
  (should (equal (my-but-last '(a)) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; P03 (*) Find the K'th element of a list.
; The first element in the list is number 1.
; Example:
; * (element-at '(a b c d e) 3)
; C

;; element-at list pos
(defun element-at (list pos)

  ;; check if list is null.
  (if (null list)

      ;; if is: return null.
      nil

    ;; check if pos < 0
    (if (< pos 0)

	;; if is: return null.
	nil

      ;; call elemtent-at-rec list pos 0
      (element-at-rec list pos 0))))

;; element-at-rec list pos num
(defun element-at-rec (list pos num)

  ;; check if list is null.
  (if (null list)

      ;; if is: return null.
      nil

    ;; check if pos == num.
    (if (= pos num)

	;; if is: return car list.
	(car list)

      ;; if is not: call element-at-rec (cdr of list) pos (num + 1).
      (element-at-rec (cdr list) pos (+ num 1)))))

;;; Tests
(ert-deftest element-at-01 ()
  (should (equal (element-at '(a b c d e) 0) 'a)))

(ert-deftest element-at-02 ()
  (should (equal (element-at '() 3) '())))

(ert-deftest element-at-03 ()
  (should (equal (element-at '(a b c d e) 2) 'c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; P04 (*) Find the number of elements of a list.

;; list-length-rec list
(defun list-length-rec (list)
  
  ;; check if list is null.
  (if (null list)
      
      ;; if is: return 0.
      0

    ;; if is not: return 1 + call list-length-rec (cdr list).
    (+ 1 (list-length-rec (cdr list)))))



;; list-length list
(defun list-length (list)
  ;; check if list is null.
  (if (null list)
      
      ;; if is: return 0.
      0
    
    ;; if is not: return call list-length-aux (cdr list) 1.
    (list-length-aux (cdr list) 1)))

;; list-length-aux list length
(defun list-length-aux (list length)
  
  ;; check if list is null.
  (if (null list)
      
      ;; if is: return length.
      length

    ;; if is not: return call list-length-aux (cdr list) (lenght + 1).
    (list-length-aux (cdr list) (+ length 1))))

;;; Tests
(ert-deftest list-length-rec-01 ()
  (should (equal (list-length-rec '(a b c d)) 4)))

(ert-deftest list-length-rec-02 ()
  (should (equal (list-length-rec '()) 0)))

(ert-deftest list-length-01 ()
  (should (equal (list-length '(a b c d)) 4)))

(ert-deftest list-length-02 ()
  (should (equal (list-length '()) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; P05 (*) Reverse a list.

;; reverse list
(defun list-reverse (list)

  ;; return revert-aux list ().
  (reverse-aux list ()))

;; reverse-aux list ret
(defun reverse-aux (list ret)
  
  ;; check if list is null.
  (if (null list)
      
      ;; if is: return ret.
      ret
    
    ;; if is not: return reverse-aux (cdr list) (cons (car list) ret).
    (reverse-aux (cdr list) (cons (car list) ret))))

;; list     ret
;; (a b c)  ()
;; (b c)    (a)
;; (c)      (b a)
;; ()       (c b a)

;;; Tests
(ert-deftest list-reverse-01()
  (should (equal (list-reverse '(a b c)) '(c b a))))

(ert-deftest list-reverse-02()
  (should (equal (list-reverse '()) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; P06 (*) Find out whether a list is a palindrome.
; A palindrome can be read forward or backward; e.g. (x a m a x).

;;;; Uses Exercise 05

;; palindrome list
(defun palindrome (list)

  ;; return if the list is equal its reverse.
  (equal list (list-reverse list)))

;;; Tests
(ert-deftest palindrome-01 ()
  (should (equal (palindrome '(x a m a x)) 't)))

(ert-deftest palindrome-02 ()
  (should (equal (palindrome '(x a m a y)) 'nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; P07 (**) Flatten a nested list structure.
; Transform a list, possibly holding lists as elements into a `flat' list by
; replacing each list with its elements (recursively).
;
; Example:
; * (my-flatten '(a (b (c d) e)))
; (A B C D E)
;
; Hint: Use the predefined functions list and append.

;; my-flatten elem
(defun my-flatten (elem)
  
  ;; check if elem is null.
  (if (null elem)

      ;; return null.
      nil

    ;; check if CAR is a list.
    (if (listp (car elem))

	;; if is: append (my-flatten CAR) (my-flatten CDR).
	(append (my-flatten (car elem)) (my-flatten (cdr elem)))

      ;; if is not: append (list CAR) (my-flatten CDR).
      (append (list (car elem)) (my-flatten (cdr elem))))))

;;; Tests
(ert-deftest my-flatten-01 ()
  (should (equal (my-flatten '(a (b (c d) e))) '(a b c d e))))

(ert-deftest my-flatten-02 ()
  (should (equal (my-flatten '()) '())))

(ert-deftest my-flatten-03 ()
  (should (equal (my-flatten '(a b c d)) '(a b c d))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; P08 (**) Eliminate consecutive duplicates of list elements.
; If a list contains repeated elements they should be replaced with a single
; copy of the element. The order of the elements should not be changed.
;
; Example:
; * (compress '(a a a a b c c a a d e e e e))
; (A B C A D E)

;; compress list
(defun compress (list)
  
  ;; check if list is null.
  (if (null list)
      
      ;; if is: return null.
      nil
    
    ;; return compress-aux list null.
    (compress-aux list nil nil)))

;; compress-aux list last-elem ret
(defun compress-aux (list last ret)

  ;; check if list is null.
  (if (null list)

      ;; if is: return the result.
      (list-reverse ret)

    ;; check if (car list) is equal to last-elem.
    (if (equal (car list) last)

	;; if is: return (compress-aux (cdr list) (car list) ret).
	(compress-aux (cdr list) (car list) ret)
      
      ;; if is not: return (compress-aux (cdr list)
      ;;                                 (car list)
      ;;                                 (append (list (car list)) ret))).
      (compress-aux (cdr list) (car list) (append (list (car list)) ret)))))

;;; Tests
(ert-deftest compress-01 ()
  (should (equal (compress '(a a a a b c c a a d e e e e)) '(a b c a d e))))

(ert-deftest compress-02 ()
  (should (equal (compress '()) '())))

(ert-deftest compress-03 ()
  (should (equal (compress '(a a a a)) '(a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; P09 (**) Pack consecutive duplicates of list elements into sublists.
; If a list contains repeated elements they should be placed in separate
; sublists.
;
; Example:
; * (pack '(a a a a b c c a a d e e e e))
; ((A A A A) (B) (C C) (A A) (D) (E E E E))

;; pack-duplicates list
(defun pack-duplicates (list)

  ;; check if list is null.
  (if (null list)

      ;; if is: return null.
      nil

    ;; return compress-aux list null null.
    (cons (pack-duplicates-atom (car list) list)
	  (pack-duplicates (remove-duplicates-first-atom (car list) list)))))

;; pack-duplicates-atom elem list
(defun pack-duplicates-atom (elem list)

  ;; check if list is null.
  (if (null list)

      ;; if is: return null.
      nil

    ;; if is not: check if elem is equal to car of list.
    (if (equal elem (car list))

	;; if is: create a list with the elem and function with cdr of list.
	(append (list elem) (pack-duplicates-atom elem (cdr list)))

      ;; if is not: return nil to close the list.
      nil)))

;; remove-duplicates-first-atom elem list
(defun remove-duplicates-first-atom (elem list)

  ;; check if list is null.
  (if (null list)

      ;; if is: return nil.
      nil

    ;; if is not: check if elem is equal to car of list.
    (if (equal elem (car list))

	;; if is: create a list without the duplicates of the first atom.
	(append (remove-duplicates-first-atom elem (cdr list)))

      ;; if is not: return the rest of the list.
      list)))

;; (a a a a)  nil  nil       nil
;; (a a a)    a    (a)       nil
;; (a a)      a    (a a)     nil
;; (a)        a    (a a a)   nil
;;  nil       a    (a a a a) nil
;;  nil       nil  (a a a a) (a a a a)

 
;;; Tests
(ert-deftest pack-duplicates-01 ()
  (should (equal (pack-duplicates  '(a a a a))
		 '((a a a a)))))

(ert-deftest pack-duplicates-02 ()
  (should (equal (pack-duplicates  '())
		 '())))

(ert-deftest pack-duplicates-03 ()
  (should (equal (pack-duplicates  '(a a a a b c c a a d e e e e))
		 '((a a a a) (b) (c c) (a a) (d) (e e e e)))))

(ert-deftest pack-duplicates-atom-01 ()
  (should (equal (pack-duplicates-atom 'a '(a a a a))
		 '(a a a a))))

(ert-deftest pack-duplicates-atom-02 ()
  (should (equal (pack-duplicates-atom 'a '())
		 '())))

(ert-deftest pack-duplicates-atom-03 ()
  (should (equal (pack-duplicates-atom 'a '(a a a b b b))
		 '(a a a))))

(ert-deftest remove-duplicates-first-atom-01 ()
  (should (equal (remove-duplicates-first-atom 'a '(a a a a))
		 '())))

(ert-deftest remove-duplicates-first-atom-02 ()
  (should (equal (remove-duplicates-first-atom 'a '())
		 '())))

(ert-deftest remove-duplicates-first-atom-03 ()
  (should (equal (remove-duplicates-first-atom 'a '(a a b b))
		 '(b b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; P10 (*) Run-length encoding of a list.
; Use the result of problem P09 to implement the so-called run-length
; encoding data compression method. Consecutive duplicates of elements are
; encoded as lists (N E) where N is the number of duplicates of the element E.
;
; Example:
; * (encode '(a a a a b c c a a d e e e e))
; ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

; P11 (*) Modified run-length encoding.
; Modify the result of problem P10 in such a way that if an element has no
; duplicates it is simply copied into the result list. Only elements with
; duplicates are transferred as (N E) lists.
;
; Example:
; * (encode-modified '(a a a a b c c a a d e e e e))
; ((4 A) B (2 C) (2 A) D (4 E))

; P12 (**) Decode a run-length encoded list.
; Given a run-length code list generated as specified in problem P11.
; Construct its uncompressed version.

; P13 (**) Run-length encoding of a list (direct solution).
; Implement the so-called run-length encoding data compression method
; directly. I.e. don't explicitly create the sublists containing the
; duplicates, as in problem P09, but only count them. As in problem P11,
; simplify the result list by replacing the singleton lists (1 X) by X.
;
; Example:
; * (encode-direct '(a a a a b c c a a d e e e e))
; ((4 A) B (2 C) (2 A) D (4 E))

; P14 (*) Duplicate the elements of a list.
; Example:
; * (dupli '(a b c c d))
; (A A B B C C C C D D)

; P15 (**) Replicate the elements of a list a given number of times.
; Example:
; * (repli '(a b c) 3)
; (A A A B B B C C C)
;

; P16 (**) Drop every N'th element from a list.
; Example:
; * (drop '(a b c d e f g h i k) 3)
; (A B D E G H K)

; P17 (*) Split a list into two parts; the length of the first part is given.
; Do not use any predefined predicates.
;
; Example:
; * (split '(a b c d e f g h i k) 3)
; ( (A B C) (D E F G H I K))

; P18 (**) Extract a slice from a list.
; Given two indices, I and K, the slice is the list containing the elements
; between the I'th and K'th element of the original list (both limits
; included). Start counting the elements with 1.
;
; Example:
; * (slice '(a b c d e f g h i k) 3 7)
; (C D E F G)

; P19 (**) Rotate a list N places to the left.
; Examples:
; * (rotate '(a b c d e f g h) 3)
; (D E F G H A B C)
;
; * (rotate '(a b c d e f g h) -2)
; (G H A B C D E F)
;
; Hint: Use the predefined functions length and append, as well as the result
; of problem P17.

; P20 (*) Remove the K'th element from a list.
; Example:
; * (remove-at '(a b c d) 2)
; (A C D)

; P21 (*) Insert an element at a given position into a list.
; Example:
; * (insert-at 'alfa '(a b c d) 2)
; (A ALFA B C D)

; P22 (*) Create a list containing all integers within a given range.
; If first argument is smaller than second, produce a list in decreasing order.
; Example:
; * (range 4 9)
; (4 5 6 7 8 9)

; P23 (**) Extract a given number of randomly selected elements from a list.
; The selected items shall be returned in a list.
; Example:
; * (rnd-select '(a b c d e f g h) 3)
; (E D A)
;
; Hint: Use the built-in random number generator and the result of problem P20.

; P24 (*) Lotto: Draw N different random numbers from the set 1..M.
; The selected numbers shall be returned in a list.
; Example:
; * (lotto-select 6 49)
; (23 1 17 33 21 37)
;
; Hint: Combine the solutions of problems P22 and P23.

; P25 (*) Generate a random permutation of the elements of a list.
; Example:
; * (rnd-permu '(a b c d e f))
; (B A D C E F)
;
; Hint: Use the solution of problem P23.

; P26 (**) Generate the combinations of K distinct objects chosen from the N
; elements of a list
; In how many ways can a committee of 3 be chosen from a group of 12 people?
; We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the
; well-known binomial coefficients). For pure mathematicians, this result may
; be great. But we want to really generate all the possibilities in a list.
;
; Example:
; * (combination 3 '(a b c d e f))
; ((A B C) (A B D) (A B E) ... )

; P27 (**) Group the elements of a set into disjoint subsets.
; a) In how many ways can a group of 9 people work in 3 disjoint subgroups of
; 2, 3 and 4 persons? Write a function that generates all the possibilities
; and returns them in a list.
;
; Example:
; * (group3 '(aldo beat carla david evi flip gary hugo ida))
; ( ( (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA) )
; ... )
;
; b) Generalize the above predicate in a way that we can specify a list of
; group sizes and the predicate will return a list of groups.
;
; Example:
; * (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
; ( ( (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA) )
; ... )
;
; Note that we do not want permutations of the group members; i.e.
; ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO) ...). However, we
; make a difference between ((ALDO BEAT) (CARLA DAVID) ...) and
; ((CARLA DAVID) (ALDO BEAT) ...).
;
; You may find more about this combinatorial problem in a good book on
; discrete mathematics under the term "multinomial coefficients".

; P28 (**) Sorting a list of lists according to length of sublists
; a) We suppose that a list contains elements that are lists themselves. The
; objective is to sort the elements of this list according to their length.
;  E.g. short lists first, longer lists later, or vice versa.
;
; Example:
; * (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
; ((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L))
;
; b) Again, we suppose that a list contains elements that are lists
; themselves. But this time the objective is to sort the elements of this
; list according to their length frequency; i.e., in the default, where
; sorting is done ascendingly, lists with rare lengths are placed first,
; others with a more frequent length come later.
;
; Example:
; * (lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
; ((i j k l) (o) (a b c) (f g h) (d e) (d e) (m n))
;
; Note that in the above example, the first two lists in the result have
; length 4 and 1, both lengths appear just once. The third and forth list
; have length 3 which appears twice (there are two list of this length).
; And finally, the last three lists have length 2. This is the most
; frequent length.
