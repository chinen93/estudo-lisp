;;;; my/queue.el Queue data structure-*- lexical-binding: t; -*-


(cl-defstruct (queue
               (:copier nil))
  head
  tail
  elements
  size)


(defun queue-create ()
  "Create an empty queue data structure."
  (let ((queue (make-queue)))
    (setf (queue-size queue) 0)
    queue))


(defun queue-to-list (queue)
  "Return `head' and `tail' of the queue in a list."
  (let ((head (queue-head queue))
        (tail (queue-tail queue))
        (elements (queue-elements queue))
        (size (queue-size queue)))
     (list head tail elements size)))


(defun queue-empty (queue)
  "return t if the queue is empty. nil otherwise."
  (null (queue-elements queue)))


(defun queue-enqueue (queue elem)
  "Enqueue `elem' as the last element of `queue'.
First In First Out strategy. (FIFO)"

  ;; Check if it is the last.
  (when (queue-empty queue)
    (setf (queue-head queue) elem))

  ;; Put elements in the back of the list.
  (let* ((elements (queue-elements queue))
         (size (queue-size queue))
         (new-size (1+ size)))

    (setf (queue-elements queue) (nconc elements (list elem)))

    (setf (queue-tail queue) elem)
    (setf (queue-size queue) new-size))

  queue)


(defun queue-dequeue (queue)
  "Dequeue first element of `queue'
First In First Out strategy. (FIFO)"

  ;; Check for empty queue
  (when (queue-empty queue)
    (error "Empty Queue"))

  ;; Put the new first element in place.
  (let* ((elements (queue-elements queue))
         (size (1- (queue-size queue)))
         (elem (queue-head queue)))

    ;; Remove first element of queue
    (setf (queue-head queue) (car (cdr elements)))
    (setf (queue-elements queue) (cdr elements))
    (setf (queue-size queue) size)

    ;; Check if queue is empty now.
    (when (queue-empty queue)
      (setf (queue-tail queue) nil))

    ;; Return dequeued element.
    elem))


(defun queue-push (queue elem)
  "Push `elem' into `queue' as the first element.
First In Last Out. (FILO)"

  ;; Check if it is the last.
  (when (queue-empty queue)
    (setf (queue-head queue) elem))

  ;; Put element as the first one.
  (let* ((elements (queue-elements queue))
         (new-elements (cons elem elements))
         (new-size (1+ (queue-size queue))))
    (setf (queue-tail queue) elem)
    (setf (queue-elements queue) new-elements)
    (setf (queue-size queue) new-size))

  queue)


(defun queue-pop (queue)
  "Pop first element of `queue'.
First In Last Out. (FILO)"

  (when (queue-empty queue)
    (error "Empty Queue"))

  (let ((elem (queue-tail queue))
        (elements (queue-elements queue))
        (new-size (1- (queue-size queue))))

    (setf (queue-tail queue) (car (cdr elements)))
    (setf (queue-elements queue) (cdr elements))
    (setf (queue-size queue) new-size)
    elem))

(provide 'queue-stack)
