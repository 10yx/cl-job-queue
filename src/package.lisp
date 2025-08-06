(defpackage #:cl-job-queue
 (:use #:cl)
 (:local-nicknames
  (:bt :bordeaux-threads)
  (:log :logur))
 (:export
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
  #:set-batch-interval

  ;; Configuration
  #:set-processor
  #:set-error-handler))
