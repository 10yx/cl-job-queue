;;; package.lisp
(defpackage #:cl-job-queue
 (:use #:cl)
 (:export
  ;; Class
  #:job-queue

  ;; Queue Management
  #:make-job-queue
  #:enqueue
  #:dequeue
  #:queue-size
  #:clear-queue

  ;; Worker Management
  #:start-worker
  #:stop-worker
  #:worker-running-p

  ;; Batch Scheduling
  #:start-batch-scheduler
  #:stop-batch-scheduler

  ;; Configuration
  #:set-processor
  #:set-batch-interval
  #:set-logger))
