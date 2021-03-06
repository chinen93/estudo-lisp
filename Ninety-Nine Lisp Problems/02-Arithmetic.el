﻿;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Arithmetic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; P31 (**) Determine whether a given integer number is prime.
; Example:
; * (is-prime 7)
; T

(defun is-prime (num)

  "Determine whether NUM is prime."

  (cond

   ((or (= num 2)
	(= num 3)
	(= num 5)
	(= num 7)
	(= num 11)
	(= num 19)
	(= num 23)
	(= num 29)
	(= num 31))
    t)
   
   ((or (< num 1)
	(= 1 num)
	(= (mod num 2) 0)
	(= (mod num 3) 0)
	(= (mod num 5) 0)
	(= (mod num 7) 0)
	(= (mod num 11) 0)
	(= (mod num 19) 0)
	(= (mod num 23) 0)
	(= (mod num 29) 0)
	(= (mod num 31) 0))
    nil)
   
   (t
    'dont-know)))

;;; Tests

(ert-deftest is-prime-01 ()
  (should (equal (is-prime 7)
		 t)))

(ert-deftest is-prime-02 ()
  (should (equal (is-prime 8)
		 nil)))

(ert-deftest is-prime-03 ()
  (should (equal (is-prime 1)
		 nil)))

(ert-deftest is-prime-04 ()
  (should (equal (is-prime 103)
		 'don-know)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; P32 (**) Determine the greatest common divisor of two positive integer
; numbers.
; Use Euclid's algorithm.
; Example:
; * (gcd 36 63)
; 9

;; gcd numA numB
(defun gcd (numA numB)

  "Determine the greatest common divisor of positive integer NUMA
and positive integer NUMB, Use Euclid's algorithm."

  (cond

   ;; If A = 0 then GCD(A,B)=B, since the GCD(0,B)=B, and we can stop.
   ((zerop numA)
    numB)

   ;; If B = 0 then GCD(A,B)=A, since the GCD(A,0)=A, and we can stop.  
   ((zerop numB)
    numA)
   
   ;; Write A in quotient remainder form (A = B⋅Q + R)
   ;; Find GCD(B,R) using the Euclidean Algorithm since GCD(A,B) = GCD(B,R)
   (t
    (gcd numB (% numA numB)))))

;;; Tests

(ert-deftest gcd-01 ()
  (should (equal (gcd 36 63)
		 9)))

(ert-deftest gcd-02 ()
  (should (equal (gcd 2 6)
		 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; P33 (*) Determine whether two positive integer numbers are coprime.
; Two numbers are coprime if their greatest common divisor equals 1.
; Example:
; * (coprime 35 64)
; T

;; coprime numA numB
(defun coprime (numA numB)

  "Determine whether positive integer NUMA and positive integer
  NUMB are coprime.

Two number are coprime if their greatest common divisor equals
1."

  ;; conditional:
  (cond

   ;; check if the greatest common divisor of A and B is equal 1.
   ((= (gcd numA numB) 1)

    ;; if is: return true.
    t)

   ;; default:
   (t

    ;; return nil.
    nil)))

;;; Tests

(ert-deftest coprime-01 ()
  (should (equal (coprime 35 64)
		 t)))

(ert-deftest coprime-02 ()
  (should (equal (coprime 5 25)
		 nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; P34 (**) Calculate Euler's totient function phi(m).
; Euler's so-called totient function phi(m) is defined as the number of
; positive integers r (1 <= r < m) that are coprime to m.
; Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case:
; phi(1) = 1.
;
; * (totient-phi 10)
; 4
;
; Find out what the value of phi(m) is if m is a prime number. Euler's
; totient function plays an important role in one of the most widely used
; public key cryptography methods (RSA). In this exercise you should use the
; most primitive method to calculate this function (there are smarter ways
; that we shall discuss later).
;

;; totient-phi num
(defun totient-phi (num)

  "Calculate Euler's totient function phi(NUM).

Euler's so-called totient function phi(NUM) is defined as the number of
positive integers r (1 <= r < NUM) that are coprime to NUM.
Example: NUM = 10: r = 1,3,7,9; thus phi(NUM) = 4."

  ;; conditional
  (cond

   ;; if num = 1.
   ((= num 1)

    ;; return 1
    1)

   ;; if num neg.
   ((< num 1)

    ;; return that I don't know
    'dont-know)

   ;; default. 
   (t

    ;; call a recusive function.
    (totient-phi-rec num (1- num)))))

;; totient-phi-rec num num-rec
(defun totient-phi-rec (num num-rec)

  ;; conditional.
  (cond

   ;; if num-rec = 1.
   ((= num-rec 1)

    ;; return 1.
    1)

   ;; check if num e the num-rec are coprime
   ((coprime num num-rec)

    ;; if they are add 1 and call recursive again.
    (1+ (totient-phi-rec num (1- num-rec))))

   ;; they are not coprime.
   (t

    ;; just call the recursive again.
    (totient-phi-rec num (1- num-rec)))))

;;; Tests

(ert-deftest totient-phi-01 ()
  (should (equal (totient-phi 10)
		 4)))

(ert-deftest totient-phi-02 ()
  (should (equal (totient-phi -10)
		 'dont-know)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; P35 (**) Determine the prime factors of a given positive integer.
; Construct a flat list containing the prime factors in ascending order.
; Example:
; * (prime-factors 315)
; (3 3 5 7)

(defun prime-factors (number)
  "Determine the prime factors of NUMBER a given positive integer.
Construct a flat list containing the prime factors in ascending order.
"

  ;; Conditionals
  (cond

   ((equal (mod number 2) 0)
    (append '(2) (prime-factors (/ number 2))))
    
   (t
    (list number))))

;;; Tests

(ert-deftest prime-factors-01 ()
  (should (equal (prime-factors 315)
		 '(3 3 5 7))))

(ert-deftest prime-factors-02 ()
  (should (equal (prime-factors 312)
		 '(3 3 5 7))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; P36 (**) Determine the prime factors of a given positive integer (2).
; Construct a list containing the prime factors and their multiplicity.
; Example:
; * (prime-factors-mult 315)
; ((3 2) (5 1) (7 1))
; Hint: The problem is similar to problem P13.
;

;; prime-factors-mult num
;; let primes (prime-factors num)
;; encode-direct primes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; P37 (**) Calculate Euler's totient function phi(m) (improved).
; See problem P34 for the definition of Euler's totient function. If the list
; of the prime factors of a number m is known in the form of problem P36 then
; the function phi(m) can be efficiently calculated as follows:
;Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their
; multiplicities) of a given number m. Then phi(m) can be calculated with the
; following formula:
; phi(m) = (p1 - 1) * p1 ** (m1 - 1) + (p2 - 1) * p2 ** (m2 - 1) + (p3 - 1) * p3 ** (m3 - 1) + ...
;
; Note that a ** b stands for the b'th power of a.
;

;; phi-improved num
;; let primes (prime-factors-mult num)
;; for each elem of list
;;;; let prime (car elem)
;;;; let mult (car (cdr elem))
;;;; collect sum of (* (1- prime) (power prime (1- mult)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; P38 (*) Compare the two methods of calculating Euler's totient function.
; Use the solutions of problems P34 and P37 to compare the algorithms. Take
; the number of logical inferences as a measure for efficiency. Try to
; calculate phi(10090) as an example.

; P39 (*) A list of prime numbers.
; Given a range of integers by its lower and upper limit, construct a list of
; all prime numbers in that range.

;; prime-range lower upper
;; for lower to upper
;;;; collect lower if it is prime

; P40 (**) Goldbach's conjecture.
; Goldbach's conjecture says that every positive even number greater than
; 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the
; most famous facts in number theory that has not been proved to be correct
; in the general case. It has been numerically confirmed up to very large
; numbers (much larger than we can go with our Prolog system). Write a
; predicate to find the two prime numbers that sum up to a given even integer.
; Example:
; * (goldbach 28)
; (5 23)
;

;; goldbach num
;; if num is odd
;;;; return nil
;; let primes (primes-range 2 num)
;; for lower (first primes)
;;     upper (last primes)
;; until lower > upper
;; do
;;;; if lower + upper < num
;;;;;; lower = next prime
;;;; if lower + upper > num
;;;;;; upper = previus prime
;;;; if lower + upper == num
;;;;;; return (list lower upper) 

; P41 (**) A list of Goldbach compositions.
; Given a range of integers by its lower and upper limit, print a list of all
; even numbers and their Goldbach composition.
; Example:
; * (goldbach-list 9 20)
; 10 = 3 + 7
; 12 = 5 + 7
; 14 = 3 + 11
; 16 = 3 + 13
; 18 = 5 + 13
; 20 = 3 + 17
;
; In most cases, if an even number is written as the sum of two prime
; numbers, one of them is very small. Very rarely, the primes are both bigger
; than say 50. Try to find out how many such cases there are in the
; range 2..3000.
;
; Example (for a print limit of 50):
; * (goldbach-list 1 2000 50)
; 992 = 73 + 919
; 1382 = 61 + 1321
; 1856 = 67 + 1789
; 1928 = 61 + 1867

;; goldbash-list lower upper 
;; let primes (primes-range lower upper)
;; for high-prime (first primes) to (last primes)
;;;; for low-prime (first primes) to (last primes)
;;;;;; if high-prime + low-prime < lower or > upper
;;;;;;;; break for low-prime
;;;;;; collect (list low-prime high-prime (+ low-prime high-prime))



