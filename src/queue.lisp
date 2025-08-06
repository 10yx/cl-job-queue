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

(defmethod set-processor ((queue job-queue) fn)
 "Set the function to be used for processing jobs."
 (setf (queue-processor queue) fn))

(defmethod set-error-handler ((queue job-queue) fn)
 "Set the function to be used for handling errors."
 (setf (queue-error-handler queue) fn))

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

(defmethod queue-size ((queue job-queue))
 "Return the number of jobs in the queue."
 (bt:with-lock-held ((slot-value queue 'lock))
  (length (queue-items queue))))

(defmethod clear-queue ((queue job-queue))
 "Remove all jobs from the queue."
 (bt:with-lock-held ((slot-value queue 'lock))
  (setf (queue-items queue) nil)))
