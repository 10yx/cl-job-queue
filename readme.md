# cl-job-queue

A thread-safe job queue implementation for Common Lisp, designed to process tasks asynchronously using worker threads. It supports configurable task processing, error handling, and optional batch scheduling.

## Features

*   **Thread-Safe Queuing**: Tasks can be safely added to the queue from multiple threads.
*   **Asynchronous Processing**: Tasks are processed by a dedicated worker thread, preventing blocking of the main application thread.
*   **Configurable Processor**: Define a custom function to handle the execution of each task.
*   **Error Handling**: Specify a function to catch and handle errors that occur during task processing.
*   **Batch Scheduling**: An optional background scheduler can periodically trigger processing or signal the worker.
*   **Graceful Shutdown**: The worker thread can be stopped cleanly, ensuring no tasks are lost mid-processing.

## Installation

This library is written in Common Lisp. To use it, ensure you have the `bordeaux-threads` and `log` libraries available (typically managed via Quicklisp).

You can load the library by:

1.  **Using Quicklisp (if packaged)**:
    ```lisp
    (ql:quickload :cl-job-queue)
    ```
2.  **Loading source files directly**:
    If you have the source files (`queue.lisp`, `scheduler.lisp`, `worker.lisp`), you can load them in order:
    ```lisp
    (load "path/to/queue.lisp")
    (load "path/to/scheduler.lisp")
    (load "path/to/worker.lisp")
    (in-package #:cl-job-queue)
    ```

## Usage

### 1. Creating a Job Queue

Use `make-job-queue` to create an instance. You can optionally provide a task processor and an error handler during creation.

```lisp
;; Define your task processing function
(defun my-task-processor (task)
  (format t "Processing task: ~a~%" task)
  ;; Simulate work
  (sleep (random 2.0)))

;; Define your error handling function
(defun my-error-handler (task error)
  (format t "ERROR: Failed to process task '~a' with error: ~a~%" task error))

;; Create a queue with custom processor and error handler
(defvar *my-job-queue*
  (cl-job-queue:make-job-queue
   :processor #'my-task-processor
   :error-handler #'my-error-handler))

;; Create a queue with default processor (#'identity) and no error handler
;; (defvar *default-job-queue* (cl-job-queue:make-job-queue))
```

### 2. Starting and Stopping the Worker

The worker thread is responsible for dequeuing and processing tasks.

```lisp
;; Start the worker thread. This also starts the batch scheduler.
(cl-job-queue:start-worker *my-job-queue* :name "MyCustomWorker")

;; Check if the worker is running
(cl-job-queue:worker-running-p *my-job-queue*) ; => T

;; Stop the worker gracefully. This waits for the current task to finish.
(cl-job-queue:stop-worker *my-job-queue*)

;; Check status after stopping
(cl-job-queue:worker-running-p *my-job-queue*) ; => NIL
```

### 3. Enqueuing Tasks

Add tasks to the queue using the `enqueue` function.

```lisp
(cl-job-queue:enqueue *my-job-queue* "Task 1: Process user data")
(cl-job-queue:enqueue *my-job-queue* '(:action :send-email :recipient "test@example.com"))
(cl-job-queue:enqueue *my-job-queue* 12345)
```

### 4. Batch Scheduling

The batch scheduler runs in a separate thread and periodically enqueues a special `:batch-flush` task. This can be useful for triggering periodic work or ensuring the worker checks for new tasks even if the queue has been idle.

```lisp
;; Start the batch scheduler (it's also started by start-worker)
(cl-job-queue:start-batch-scheduler *my-job-queue*)

;; Set the interval between batch signals (default is 2.0 seconds)
;; Note: The exported function `set-batch-interval` is not implemented.
;; Use the accessor directly:
(setf (cl-job-queue:batch-interval *my-job-queue*) 5.0) ; Set to 5 seconds

;; Stop the batch scheduler
(cl-job-queue:stop-batch-scheduler *my-job-queue*)
```

### 5. Example Scenario

```lisp
(ql:quickload :cl-job-queue)

(defpackage :my-app
  (:use :cl :cl-job-queue))
(in-package :my-app)

;; Define a processor that simulates work and might fail
(defun process-item (item)
  (format t "[~a] Starting to process: ~a~%" (bt:thread-name (bt:current-thread)) item)
  (sleep (random 1.5))
  (when (string= item "Task D")
    (error "Simulated failure for Task D"))
  (format t "[~a] Finished processing: ~a~%" (bt:thread-name (bt:current-thread)) item))

;; Define an error handler
(defun handle-item-error (task error)
  (format t "[~a] ERROR processing task '~a': ~a~%" (bt:thread-name (bt:current-thread)) task error))

;; Create and configure the queue
(defvar *app-job-q* (make-job-queue
                     :processor #'process-item
                     :error-handler #'handle-item-error))

;; Start the worker
(start-worker *app-job-q* :name "AppWorker")

;; Enqueue some tasks
(enqueue *app-job-q* "Task A")
(enqueue *app-job-q* "Task B")
(enqueue *app-job-q* "Task C")
(enqueue *app-job-q* "Task D") ; This one will cause an error
(enqueue *app-job-q* "Task E")

;; Let the worker process for a few seconds
(format t "Enqueued tasks. Waiting for processing...~%")
(sleep 5)

;; Stop the worker
(format t "Stopping worker...~%")
(stop-worker *app-job-q*)
(format t "Worker stopped.~%")
```

## API Reference

### Queue Management

*   `make-job-queue (&key processor error-handler)`
    *   Creates and returns a new `job-queue` instance.
    *   `processor`: An optional function to process tasks. Defaults to `#'identity`.
    *   `error-handler`: An optional function to handle errors during task processing. Defaults to `nil`.

*   `enqueue (queue job-queue) task`
    *   Adds `task` to the queue. This operation is thread-safe.

*   `dequeue (queue job-queue)`
    *   Removes and returns a task from the queue.
    *   If the queue is empty and the worker is running, this function will block until a task is available or the worker is stopped.
    *   If the worker is stopped and the queue is empty, it returns `NIL`.

*   `queue-size (queue job-queue)`
    *   **(Not Implemented)** This function is exported but has no implementation in the provided code. To get the queue size, you would typically access the `queue-items` slot directly (e.g., `(length (queue-items my-queue))`), but this should be done with caution regarding thread safety if the worker is active.

*   `clear-queue (queue job-queue)`
    *   **(Not Implemented)** This function is exported but has no implementation. To clear the queue, you would typically set the `queue-items` slot to `NIL` (e.g., `(setf (queue-items my-queue) nil)`), but this should be done with caution regarding thread safety.

### Worker Management

*   `start-worker (queue job-queue &key (name "Job Worker"))`
    *   Starts the main worker thread responsible for processing tasks from the queue. It also automatically starts the batch scheduler.
    *   `name`: An optional string for naming the worker thread.

*   `stop-worker (queue job-queue)`
    *   Gracefully stops the worker thread and the associated batch scheduler. It signals the worker to stop, wakes up any threads waiting on the condition variable, and then waits for the worker thread to terminate using `bt:join-thread`.

*   `worker-running-p (queue job-queue)`
    *   Returns `T` if the worker thread is currently active, `NIL` otherwise. This is an accessor for the internal `running` slot.

### Batch Scheduling

*   `start-batch-scheduler (queue job-queue)`
    *   Starts a separate thread that periodically signals the queue (by enqueuing a `:batch-flush` task) at the interval specified by `batch-interval`.

*   `stop-batch-scheduler (queue job-queue)`
    *   Stops the batch scheduler thread.

*   `set-batch-interval (queue job-queue) interval`
    *   **(Exported but Not Implemented)** This function is exported but has no implementation. To change the batch interval, use the `batch-interval` accessor directly: `(setf (cl-job-queue:batch-interval my-queue) 5.0)`.

*   `batch-interval (queue job-queue)`
    *   **(Accessor)** Returns the current interval (in seconds) for the batch scheduler.

### Configuration (Accessors)

These functions are accessors to the internal slots of the `job-queue` class, allowing dynamic configuration.

*   `queue-processor (queue job-queue)`
    *   **(Accessor)** Returns the function currently used to process tasks.

*   `queue-error-handler (queue job-queue)`
    *   **(Accessor)** Returns the function currently used to handle errors during task processing.

## Dependencies

*   **Common Lisp**: SBCL is recommended.
*   **`bordeaux-threads`**: For multi-threading primitives (locks, condition variables, threads).
*   **`log`**: For logging messages (used internally by the library, e.g., for worker start/stop).

## Design Decisions & Notes

*   **Thread Safety**: All core queue operations (`enqueue`, `dequeue`) are protected by a `bordeaux-threads` lock to ensure safe concurrent access.
*   **Worker Loop**: The worker thread continuously attempts to `dequeue` tasks. If the queue is empty, it waits on a condition variable until a task is enqueued or the worker is signaled to stop.
*   **Error Handling Strategy**: Errors occurring within the `processor` function are caught. If an `error-handler` is configured, it is invoked with the task and the error object. Otherwise, the error might propagate and potentially crash the worker thread (depending on the Lisp environment's default error handling).
*   **Batch Scheduler Task (`:batch-flush`)**: The batch scheduler enqueues a special symbol `:batch-flush`. The worker thread will pick this up and pass it to the `processor` function. The library does not provide specific handling for this symbol; its purpose is to act as a signal or a trigger for the worker to re-evaluate the queue state after the `batch-interval` has elapsed.
*   **Shutdown Mechanism**: `stop-worker` is designed for graceful shutdown. It sets a flag to stop the worker loop, notifies any threads waiting on the condition variable (to unblock `dequeue`), and then uses `bt:join-thread` to wait for the worker thread to complete its current operation and exit. The batch scheduler is also stopped.
*   **Missing Implementations**: The exported symbols `queue-size`, `clear-queue`, and `set-batch-interval` do not have corresponding implementations in the provided code. The accessors `queue-processor`, `queue-error-handler`, and `batch-interval` are available and can be used to configure these aspects dynamically after queue creation.
