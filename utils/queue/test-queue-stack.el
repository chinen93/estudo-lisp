;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests for queue.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ert)
(unless (require 'queue-stack nil t)
  (let ((queue-dir (expand-file-name "./")))
    (add-to-list 'load-path queue-dir)
    (require 'queue-stack)))

(defvar test-queue nil
  "Queue structure for tests.")

(defun setup ()
  (setq test-queue (queue-create)))

(ert-deftest test-to-list ()
  (setup)
  (should (equal (queue-to-list test-queue) '(nil nil nil 0))))

(ert-deftest test-enqueue-one-element ()
  (setup)
  (queue-enqueue test-queue '42)
  (should (equal (queue-to-list test-queue) '(42 42 (42) 1))))

(ert-deftest test-enqueue-more-than-one-element ()
  (setup)
  (queue-enqueue test-queue '42)
  (queue-enqueue test-queue '8)
  (queue-enqueue test-queue '7)
  (should (equal (queue-to-list test-queue) '(42 7 (42 8 7) 3))))

(ert-deftest test-dequeue-empty-queue ()
  (setup)
  (should-error (queue-dequeue test-queue)))

(ert-deftest test-dequeue-one-element ()
  (setup)
  (queue-enqueue test-queue '42)
  (queue-dequeue test-queue)
  (should (equal (queue-to-list test-queue) '(nil nil nil 0))))

(ert-deftest test-dequeue-return ()
  (setup)
  (queue-enqueue test-queue '42)
  (queue-enqueue test-queue '8)
  (should (equal (queue-dequeue test-queue) '42)))

(ert-deftest test-dequeue-more-than-one-element ()
  (setup)
  (queue-enqueue test-queue '42)
  (queue-enqueue test-queue '8)
  (queue-enqueue test-queue '7)
  (queue-dequeue test-queue)
  (should (equal (queue-to-list test-queue) '(8 7 (8 7) 2))))

(ert-deftest test-push-one-element ()
  (setup)
  (queue-push test-queue '42)
  (should (equal (queue-to-list test-queue) '(42 42 (42) 1))))

(ert-deftest test-push-more-than-one-element ()
  (setup)
  (queue-push test-queue '42)
  (queue-push test-queue '8)
  (queue-push test-queue '7)
  (should (equal (queue-to-list test-queue) '(42 7 (7 8 42) 3))))

(ert-deftest test-push-return ()
  (setup)
  (queue-push test-queue '42)
  (queue-push test-queue '8)
  (should (equal (queue-pop test-queue) '8)))

(ert-deftest test-pop-empty-queue ()
  (setup)
  (should-error (queue-dequeue test-queue)))

(ert-deftest test-pop-one-element ()
  (setup)
  (queue-push test-queue '42)
  (queue-pop test-queue)
  (should (equal (queue-to-list test-queue) '(nil nil nil 0))))

(ert-deftest test-pop-more-than-one-element ()
  (setup)
  (queue-push test-queue '42)
  (queue-push test-queue '8)
  (queue-push test-queue '7)
  (queue-pop test-queue)
  (should (equal (queue-to-list test-queue) '(42 8 (8 42) 2))))

(ert-deftest test-copy-queue ()
  (setup)
  (queue-push test-queue '42)
  (queue-push test-queue '8)
  (queue-push test-queue '7)
  (let ((new-queue (queue-copy test-queue)))
    (should (equal (queue-to-list new-queue)
                   (queue-to-list test-queue)))))

(ert-deftest test-copy-queue-and-change ()
  (setup)
  (queue-push test-queue '42)
  (queue-push test-queue '8)
  (queue-push test-queue '7)
  (let ((new-queue (queue-copy test-queue)))
    (queue-push new-queue '200)
    (should-not (equal (queue-to-list new-queue)
                       (queue-to-list test-queue)))))
