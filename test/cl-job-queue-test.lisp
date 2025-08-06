;;; test/cl-job-queue-test.lisp
(defpackage #:cl-job-queue/test
 (:use #:cl #:fiveam #:cl-job-queue))

(in-package #:cl-job-queue/test)

(def-suite* job-queue-suite)

(test queue-creation
 "Test queue initialization"
 (let ((queue (make-job-queue)))
  (is (not (null queue)))
  (is (zerop (queue-size queue)))
  (is (eq #'identity (queue-processor queue)))))

(test basic-enqueue-dequeue
 "Test basic queue operations"
 (let ((queue (make-job-queue))
       (results nil))
  (set-processor queue (lambda (job) (push job results)))
  (enqueue queue 'job1)
  (enqueue queue 'job2)
  (start-worker queue)
  (sleep 0.1)
  (stop-worker queue)
  (is (equal '(job2 job1) results))))

(test concurrent-access
 "Test thread-safe operations"
 (let ((queue (make-job-queue))
       (threads '()))
  ;; Start multiple producer threads
  (dotimes (i 10)
   (push (bt:make-thread
          (lambda ()
           (dotimes (j 10)
            (enqueue queue (+ (* i 10) j)))))
    threads))
  ;; Wait for completion
  (mapc #'bt:join-thread threads)
  (is (= 100 (queue-size queue)))))

(test batch-scheduling
 "Test periodic batch scheduling"
 (let ((queue (make-job-queue))
       (batch-count 0))
  (set-processor queue (lambda (job)
                        (when (eq job :batch-flush)
                         (incf batch-count))))
  (set-batch-interval queue 0.1)
  (start-worker queue)
  (sleep 0.5)
  (stop-worker queue)
  ;; Should have at least 3 batches in 0.5 seconds
  (is (>= batch-count 3))))

(defun test-cl-job-queue ()
 (run! 'job-queue-suite))
