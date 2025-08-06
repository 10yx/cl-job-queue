;;; scheduler.lisp
(in-package #:cl-job-queue)

(defmethod start-batch-scheduler ((queue job-queue))
 "Start the batch scheduler thread."
 (unless (batch-scheduler queue)
  (setf (batch-scheduler queue)
   (bt:make-thread
    (lambda ()
     (loop while (batch-scheduler queue)
      do (sleep (batch-interval queue))
      ;; Only enqueue if scheduler is still running
      (when (batch-scheduler queue)
       (enqueue queue :batch-flush))))
    :name "Batch Scheduler"))))

(defmethod stop-batch-scheduler ((queue job-queue))
 "Stop the batch scheduler thread."
 (when (batch-scheduler queue)
  (setf (batch-scheduler queue) nil)))
