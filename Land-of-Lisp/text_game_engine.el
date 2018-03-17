;;=================================================
;; Text Game Engine
;;=================================================


;; Contain descriptions of the locations that exist in our game
(setf *nodes* 
      '((living-room 
	 (you are in the living-room. a wizard is snoring loudly on the couch.))
	(garden 
	 (you are in a beautiful garden. there is a well in front of you.))
	(attic
	 (you are in the attic. there is a giant welding torch in the corner.))))

;; Function to get the description of a location
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(setf *edges* 
      '((living-room (garden west door) 
		     (attic upstairs ladder))
	(garden (living-room east door))
	(attic (living-room downstairs ladder))))

(defun describe-path (edge)
  `(there is a ,(cl-caddr edge) going ,(cadr edge) from here.))

(describe-path '(garden west door))
;; (describe-location 'living-room *nodes*) 

(defun describe-paths (location edges)
  (apply #'append (mapcar #' describe-path (cdr (assoc location edges)))))

;; (describe-paths 'living-room *edges*)

(setf *objects* '(whiskey bucket frog chain))

(setf *object-location* 
      '((whiskey living-room)
	(bucket living-room)
	(chain garden)
	(frog garden)))

(defun objects-at (loc objs obj-locs)
  (cl-labels ((at-loc-p (obj)
		     (eq (cadr (assoc obj obj-locs)) loc)))
    (cl-remove-if-not #'at-loc-p objs)))

;; (objects-at 'living-room *objects* *object-location*)


(defun describe-objects (loc objs obj-loc)
  (cl-labels ((describe-obj (obj)
			 `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))


;; (describe-objects 'living-room *objects* *object-location*)

(setf *location* 'living-room)

(defun look ()
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
	  (describe-objects *location* *objects* *object-location*)))

;; (look)

(defun walk (direction)
  (let ((next (cl-find direction (cdr (assoc *location* *edges*))
		       :key #'cadr)))
    (if next
	(progn (setf *location* (car next))
	       (look))
      '(you cannot go that way.))))

;; (walk 'west)

(defun pickup (object) 
  (cond ((member object 
		 (objects-at *location* *objects* *object-location*))
	 (push (list object 'body) *object-location*)
	 `(you are now carrying the ,object))
	(t '(you cannot get that.))))

;; (pickup 'whiskey)

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-location*)))

;; (inventory)

(defun game-read ()
  (let ((cmd (read-from-string 
	      (cl-concatenate 'string "(" (read-from-minibuffer "cmd: ") ")"))))
    (cl-flet ((quote-it (x)
			(list 'quote x)))
      (cons (car (car cmd)) (mapcar #'quote-it (cdr (car cmd)))))))

;; (game-read)


(setf *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
    '(i do not know that command.)))


;; (game-eval '(look))
;; (game-eval '(test))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
	  (rest (cdr lst)))
      (cond ((eq item (cl-coerce " " 'list)) 
	     (cons item (tweak-text rest caps lit)))

	    ((member item (cl-coerce "?." 'list)) 
	     (cons item (tweak-text rest t lit)))

	    ((member item (cl-coerce "!" 'list)) 
	     (cons item (tweak-text rest t (not lit))))

	    ((member item (cl-coerce "\""  'list)) 
	     (tweak-text rest caps lit))

	    ((member item (cl-coerce "\\"  'list)) 
	     (tweak-text rest caps (not lit)))

	    (lit 
	     (cons item (tweak-text rest caps (not lit))))

	    ((or caps lit) 
	     (cons (upcase item) (tweak-text rest nil lit)))

	    (t 
	     (cons (downcase item) (tweak-text rest nil nil)))))))

;; (cl-coerce (tweak-text (cl-coerce (string-remove-suffix ")" (string-remove-prefix "(" (prin1-to-string '(MY? NA. IS! Oi "TO," "iPad")))) 'list) t nil) 'string)

(defun game-print (lst)
  (princ (cl-coerce (tweak-text (cl-coerce  
				 (string-remove-suffix 
				  ")" 
				  (string-remove-prefix 
				   "(" 
				   (prin1-to-string (prin1-to-string lst)))) 
					   'list) 
				t 
				nil) 
		    'string)))

;; (game-print '(not only does hava a "comma," "iPad."))

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

;; (game-repl)
