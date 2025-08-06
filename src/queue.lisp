;;; queue.lisp
(in-package #:cl-job-queue)

(defclass job-queue ()
 ((queue :initform nil :accessor queue-items)
  (lock :initform (bordeaux-threads:make-lock "job-queue-lock"))
  (condition :initform (bordeaux-threads:make-condition-variable))
  (processor :initform #'identity :accessor queue-processor)
  (worker-thread :initform nil :accessor worker-thread)
  (running :initform nil :accessor worker-running-p)
  (batch-scheduler-thread :initform nil :accessor batch-scheduler-thread)
  (batch-interval :initform 2.0 :accessor batch-interval)
  (worker-name :initform "Worker" :accessor worker-name)
  (logger :initform nil :accessor queue-logger)))

(defun make-job-queue (&key processor logger (batch-interval 2.0))
 "Create a new job queue."
 (let ((queue (make-instance 'job-queue)))
  (when processor
   (setf (queue-processor queue) processor))
  (when logger
   (setf (queue-logger queue) logger))
  (setf (batch-interval queue) batch-interval)
  queue))

(defmethod enqueue ((queue job-queue) task)
 "Add a task to the queue."
 (bordeaux-threads:with-lock-held ((slot-value queue 'lock))
  (push task (queue-items queue))
  (bordeaux-threads:condition-notify (slot-value queue 'condition))))

(defmethod dequeue ((queue job-queue))
 "Remove and return a task from the queue."
 (bordeaux-threads:with-lock-held ((slot-value queue 'lock))
  (loop
   (when (or (not (worker-running-p queue))
          (queue-items queue))
    (return (and (queue-items queue)
             (pop (queue-items queue)))))
   (bordeaux-threads:condition-wait (slot-value queue 'condition)
    (slot-value queue 'lock)))))

(defmethod start-worker ((queue job-queue) &key name)
 "Start the worker thread."
 (unless (worker-running-p queue)
  (when name
   (setf (worker-name queue) name))
  (setf (worker-running-p queue) t
   (worker-thread queue)
   (bordeaux-threads:make-thread
    (lambda ()
     (loop while (worker-running-p queue)
      for task = (dequeue queue)
      when task do
      (handler-case
       (funcall (queue-processor queue) task)
       (error (e)
        (when (queue-logger queue)
         (funcall (queue-logger queue)
          :error 'worker
          "Error in background task ~A : ~A"
          task e))))))
    :name (worker-name queue)))
  (when (queue-logger queue)
   (funcall (queue-logger queue) :info 'worker
    "~A started." (worker-name queue)))
  ;; Start batch scheduler if configured
  (start-batch-scheduler queue)))

(defmethod stop-worker ((queue job-queue))
 "Stop the worker thread."
 (when (worker-running-p queue)
  (stop-batch-scheduler queue)
  (setf (worker-running-p queue) nil)
  (bordeaux-threads:with-lock-held ((slot-value queue 'lock))
   (bordeaux-threads:condition-notify (slot-value queue 'condition)))
  (bordeaux-threads:join-thread (worker-thread queue))
  (setf (worker-thread queue) nil)
  (when (queue-logger queue)
   (funcall (queue-logger queue) :info 'worker
    "~A stopped." (worker-name queue)))))

(defmethod start-batch-scheduler ((queue job-queue))
 "Start the batch scheduler thread."
 (unless (batch-scheduler-thread queue)
  (setf (batch-scheduler-thread queue)
   (bordeaux-threads:make-thread
    (lambda ()
     (loop while (batch-scheduler-thread queue)
      do (sleep (batch-interval queue))
      when (batch-scheduler-thread queue)
      do (enqueue queue :batch-flush)))
    :name "batch-scheduler"))))

(defmethod stop-batch-scheduler ((queue job-queue))
 "Stop the batch scheduler thread."
 (when (batch-scheduler-thread queue)
  (when (queue-logger queue)
   (funcall (queue-logger queue) :info 'scheduler
    "Stopping batch scheduler thread."))
  (setf (batch-scheduler-thread queue) nil)))

(defmethod set-processor ((queue job-queue) fn)
 "Set the processor function."
 (setf (queue-processor queue) fn))

(defmethod set-batch-interval ((queue job-queue) interval)
 "Set the batch interval."
 (setf (batch-interval queue) interval))

(defmethod set-logger ((queue job-queue) logger-fn)
 "Set the logger function."
 (setf (queue-logger queue) logger-fn))
