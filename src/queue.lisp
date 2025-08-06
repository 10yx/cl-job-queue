;;; queue.lisp
(in-package #:cl-job-queue)

(defclass job-queue ()
 ((queue :initform nil :accessor queue-items)
  (lock :initform (bt:make-lock "job-queue-lock"))
  (condition :initform (bt:make-condition-variable))
  (processor :initform #'identity :accessor queue-processor)
  (error-handler :initform nil :accessor queue-error-handler)
  (worker-thread :initform nil :accessor worker-thread)
  (running :initform nil :accessor worker-running-p)
  (batch-scheduler :initform nil :accessor batch-scheduler)
  (batch-interval :initform 2.0 :accessor batch-interval)))

(defun make-job-queue (&key processor error-handler)
 "Create a new job queue with optional processor and error handler."
 (let ((queue (make-instance 'job-queue)))
  (when processor
   (setf (queue-processor queue) processor))
  (when error-handler
   (setf (queue-error-handler queue) error-handler))
  queue))

(defmethod enqueue ((queue job-queue) task)
 "Add a task to the queue in a thread-safe manner."
 (bt:with-lock-held ((slot-value queue 'lock))
  (push task (queue-items queue))
  (bt:condition-notify (slot-value queue 'condition))))

(defmethod dequeue ((queue job-queue))
 "Remove and return a task from the queue."
 (bt:with-lock-held ((slot-value queue 'lock))
  (loop
   (when (or (not (worker-running-p queue))
          (queue-items queue))
    (return (when (queue-items queue)
             (pop (queue-items queue)))))
   (bt:condition-wait (slot-value queue 'condition)
    (slot-value queue 'lock)))))
