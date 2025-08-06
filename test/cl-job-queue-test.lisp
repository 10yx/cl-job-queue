;;; test/cl-job-queue-test.lisp
(defpackage #:cl-job-queue-test
 (:use #:cl #:fiveam)
 (:local-nicknames (:jq :cl-job-queue)))

(in-package #:cl-job-queue-test)

(def-suite* job-queue-suite)

(test basic-operations
 "Test basic queue operations"
 (let ((queue (jq:make-job-queue))
       (results nil))
  (jq:set-processor queue (lambda (job) (push job results)))
  (jq:enqueue queue 'job1)
  (jq:enqueue queue 'job2)
  (jq:start-worker queue)
  (sleep 0.1)
  (jq:stop-worker queue)
  (is (equal '(job1 job2) results))))

(defun test-cl-job-queue ()
 (run! 'job-queue-suite))
