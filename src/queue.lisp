;; src/queue.lisp
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
  (logger :initform nil :accessor queue-logger)
  ;; --- NEW: Slot for tracking scheduled tasks ---
  (scheduled-tasks :initform (make-hash-table :test 'equal) :reader scheduled-tasks)))

(defstruct (scheduled-task (:constructor make-scheduled-task-internal))
 (id (gensym "SCHEDULED-TASK-") :read-only t)
 (thread nil)
 (running-p t))

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
  (start-batch-scheduler queue)))

(defmethod stop-worker ((queue job-queue))
 "Stop the worker thread and all scheduled tasks."
 (when (worker-running-p queue)
  (stop-batch-scheduler queue)
  (cancel-all-tasks queue) ; Stop scheduled tasks
  (setf (worker-running-p queue) nil)
  (bordeaux-threads:with-lock-held ((slot-value queue 'lock))
   (bordeaux-threads:condition-notify (slot-value queue 'condition)))
  (when (worker-thread queue)
   (bordeaux-threads:join-thread (worker-thread queue)))
  (setf (worker-thread queue) nil)
  (when (queue-logger queue)
   (funcall (queue-logger queue) :info 'worker
    "~A stopped." (worker-name queue)))))

(defmethod schedule-task ((queue job-queue) task &key (interval 60) (delay 0))
 "Schedules a TASK to be enqueued periodically.
   :INTERVAL The period in seconds between enqueues.
   :DELAY    The initial delay in seconds before the first enqueue.
   Returns a unique ID for the scheduled task, which can be used with CANCEL-TASK."
 (let ((scheduled-task (make-scheduled-task-internal)))
  (setf (scheduled-task-thread scheduled-task)
   (bordeaux-threads:make-thread
    (lambda ()
     (sleep delay)
     (loop while (scheduled-task-running-p scheduled-task)
      do (enqueue queue task)
      (sleep interval)))
    :name (format nil "Scheduled Task: ~A" (scheduled-task-id scheduled-task))))
  (setf (gethash (scheduled-task-id scheduled-task) (scheduled-tasks queue)) scheduled-task)
  (when (queue-logger queue)
   (funcall (queue-logger queue) :info 'scheduler
    "Task ~A scheduled to run every ~As (ID: ~A)"
    task interval (scheduled-task-id scheduled-task)))
  (scheduled-task-id scheduled-task)))

(defmethod cancel-task ((queue job-queue) task-id)
 "Stops and removes a scheduled task by its ID."
 (let ((task (gethash task-id (scheduled-tasks queue))))
  (when task
   (setf (scheduled-task-running-p task) nil)
   ;; Note: We don't join the thread to avoid blocking. It will exit on its own.
   (remhash task-id (scheduled-tasks queue))
   (when (queue-logger queue)
    (funcall (queue-logger queue) :info 'scheduler "Cancelled scheduled task ~A" task-id))
   t)))

(defmethod cancel-all-tasks ((queue job-queue))
 "Stops all scheduled tasks associated with this queue."
 (let ((task-ids (loop for id being the hash-keys of (scheduled-tasks queue) collect id)))
  (dolist (id task-ids)
   (cancel-task queue id))
  (when (queue-logger queue)
   (funcall (queue-logger queue) :info 'scheduler "Cancelled all ~D scheduled tasks." (length task-ids)))))

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
  ;; Simply setting the variable to NIL is enough to stop the loop.
  (let ((thread (batch-scheduler-thread queue)))
   (setf (batch-scheduler-thread queue) nil)
   ;; An optional join to ensure it's fully stopped, with a timeout.
   (bordeaux-threads:thread-alive-p thread))))

(defmethod set-processor ((queue job-queue) fn)
 "Set the processor function."
 (setf (queue-processor queue) fn))

(defmethod set-batch-interval ((queue job-queue) interval)
 "Set the batch interval."
 (setf (batch-interval queue) interval))

(defmethod set-logger ((queue job-queue) logger-fn)
 "Set the logger function."
 (setf (queue-logger queue) logger-fn))
