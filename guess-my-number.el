;;====================================================================
;; Guess my Number
;; 
;; It's a simple game where player one chose a number between 1 and
;; 100. The other player guess a number. The player one say if the
;; guessed number is smaller or bigger than the chose number. The
;; other player tries again until the guessed number is the chose one.
;;====================================================================
;; Exemples of use
;; 
;; chosen number: 47
;; (guess-my-number)
;; 50
;; (smaller)
;; 25
;; (bigger)
;; 37
;; (bigger)
;; 43
;; (smaller)
;; 46
;; (smaller)
;; 44
;; (bigger)
;; 45
;; ganhei
;;
;;
;;===
;; To play with the computer guessing the numbers do
;; (play 45) 

;; Boundaries for the game
(defvar *small* 1)
(defvar *big* 100)

;; Function to guess the number
(defun guess-my-number ()
  (ash (+ *small* *big*) -1))

;; Function to guess a smaller number
(defun smaller ()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))

;; Function to guess a bigger number
(defun bigger ()
  (setf *small* (1+ (guess-my-number)))
  (guess-my-number))

;; Function to restart the boundaries
(defun start-over ()
  (setf *small* 1)
  (setf *big* 100)
  (guess-my-number))

;; (start-over)
;; (smaller)
;; (bigger)

;; Function to play with the computer, where the computer tries to
;; guess the number
(defun play (n)
  (setf *small* 1)
  (setf *big* 100)
  (setf guess (guess-my-number))
  (play-rec n guess))

;; Auxiliary recursion function to guess the right number
(defun play-rec (n guess)
  (if (eq n guess)
      (progn 
	(message (number-to-string guess))
	(message "ganhei"))
    (progn
      (message (number-to-string guess))
      (if (< n guess)
	  (play-rec n (smaller))
	(play-rec n (bigger))))))

;; (play 55)
