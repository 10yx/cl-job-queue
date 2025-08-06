;;; worker.lisp
(in-package #:cl-job-queue)

(defmethod start-worker ((queue job-queue) &key (name "Job Worker"))
 "Start the worker thread and batch scheduler."
 (unless (worker-running-p queue)
  (setf (worker-running-p queue) t
   (worker-thread queue)
   (bt:make-thread
    (lambda ()
     (loop while (worker-running-p queue)
      for task = (dequeue queue)
      when task do
      (handler-case
       (funcall (queue-processor queue) task)
       (error (e)
        (when (queue-error-handler queue)
         (funcall (queue-error-handler queue) task e))))))
    :name name))
  (log:log-with-context :info 'job-queue "Worker started.")
  (start-batch-scheduler queue)))

(defmethod stop-worker ((queue job-queue))
 "Stop the worker thread and batch scheduler."
 (when (worker-running-p queue)
  (stop-batch-scheduler queue)
  (setf (worker-running-p queue) nil)
  (bt:with-lock-held ((slot-value queue 'lock))
   (bt:condition-notify (slot-value queue 'condition)))
  (bt:join-thread (worker-thread queue))
  (setf (worker-thread queue) nil)
  (log:log-with-context :info 'job-queue "Worker stopped.")))
