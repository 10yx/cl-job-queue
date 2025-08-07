# cl-job-queue

A lightweight, thread-safe job queue implementation for Common Lisp, designed for background task processing. It supports a single worker thread that processes tasks from the queue and an optional batch scheduler for periodic operations.

## Features

*   **Thread-Safe Queuing:** Ensures safe concurrent access to the job queue from multiple threads.
*   **Background Worker:** Processes tasks asynchronously in a dedicated thread.
*   **Configurable Processor:** Allows users to define a custom function to handle each task.
*   **Batch Scheduling:** Includes an optional scheduler that periodically signals for batch operations (e.g., flushing accumulated data).
*   **Error Handling:** Catches errors during task processing and provides an optional logging mechanism.
*   **Graceful Shutdown:** Supports stopping the worker thread and batch scheduler cleanly.
*   **Configurable Logging:** Accepts a custom logger function for monitoring.

## Dependencies

*   **`bordeaux-threads`**: For multi-threading capabilities.
*   **`logur`**: (Optional, but listed as a dependency) For structured logging, though the provided example logger uses standard `format`.

## Installation

1.  **Prerequisites:** Ensure you have a Common Lisp implementation (e.g., SBCL) and Quicklisp installed.
2.  **Load System:** Add the project's directory to your ASDF `find-system` path or install it via Quicklisp if packaged. Then, load the system:

    ```lisp
    (ql:quickload "cl-job-queue")
    ```

## Usage

### 1. Creating a Job Queue

You can create a new job queue instance using `make-job-queue`. You can optionally provide a task processor function, a logger function, and a batch interval.

```lisp
(ql:quickload "cl-job-queue")
(in-package #:cl-job-queue)

;; Define a function to process tasks
(defun my-task-processor (task)
  (format t "Processing task: ~a~%" task))

;; Define a simple logger function
(defun my-logger (level source message &rest args)
  (format t "[~a] [~a] ~a~%"
          level source
          (apply #'format nil message args)))

;; Create a queue with a custom processor and logger
(defvar *my-queue*
  (make-job-queue :processor #'my-task-processor
                  :logger #'my-logger
                  :batch-interval 5.0)) ; Set batch interval to 5 seconds
```

### 2. Starting the Worker

The worker thread is responsible for dequeuing tasks and executing them.

```lisp
;; Start the worker thread
(start-worker *my-queue*)

;; You can also provide a custom name for the worker thread
;; (start-worker *my-queue* :name "MyCustomWorker")
```

**Note:** Starting the worker also automatically starts the batch scheduler if it's not already running.

### 3. Enqueuing Tasks

Add tasks to the queue using the `enqueue` function.

```lisp
;; Enqueue individual tasks
(enqueue *my-queue* "First task data")
(enqueue *my-queue* "Second task data")
(enqueue *my-queue* 123) ; Tasks can be any Lisp object
```

### 4. Batch Scheduling

The batch scheduler periodically enqueues a special `:batch-flush` task. Your `processor` function should be designed to handle this special task if you intend to use batching.

```lisp
;; Example processor that handles batch flush
(defun my-batch-aware-processor (task)
  (cond
    ((eq task :batch-flush)
     (format t "Batch flush triggered! Performing batch operation...~%"))
    (t
     (format t "Processing individual task: ~a~%" task))))

;; Update the queue's processor
(set-processor *my-queue* #'my-batch-aware-processor)

;; Ensure the batch interval is set (e.g., 5 seconds)
(set-batch-interval *my-queue* 5.0)

;; If the worker is already running, it will pick up the new processor.
;; If you need to ensure the new interval is picked up, you might need to restart the worker.
;; (stop-worker *my-queue*)
;; (start-worker *my-queue*)
```

### 5. Stopping the Worker

Gracefully shut down the worker and batch scheduler threads.

```lisp
(stop-worker *my-queue*)
```

This function ensures that any currently processing task is allowed to finish (though the worker thread itself is joined), and then stops the worker and batch scheduler threads.

### 6. Configuration

You can change the queue's behavior after creation:

*   **Set Processor:**
    ```lisp
    (set-processor *my-queue* #'another-processor-function)
    ```
*   **Set Batch Interval:**
    ```lisp
    (set-batch-interval *my-queue* 10.0) ; Set interval to 10 seconds
    ```
*   **Set Logger:**
    ```lisp
    ;; Using a logger from logur (example)
    ;; (ql:quickload "logur")
    ;; (set-logger *my-queue* (logur:make-logger :level :info))

    ;; Or using a custom lambda as shown in the creation example
    (set-logger *my-queue* #'my-logger)
    ```

## API Reference

### Classes

*   **`job-queue`**: The primary class representing the job queue. It encapsulates the queue storage, synchronization primitives, worker thread, and configuration.

### Queue Management

*   **`make-job-queue (&key processor logger (batch-interval 2.0))`**:
    *   Creates and returns a new `job-queue` instance.
    *   `processor`: The function to execute for each task. Defaults to `#'identity`.
    *   `logger`: A function to handle logging messages. Defaults to `nil`.
    *   `batch-interval`: The interval (in seconds) for the batch scheduler. Defaults to `2.0`.

*   **`enqueue (queue job-queue) task`**:
    *   Adds `task` to the queue.
    *   This operation is thread-safe.

*   **`dequeue (queue job-queue)`**:
    *   Removes and returns a task from the queue.
    *   If the queue is empty, it waits for a task to be enqueued or for the worker to be stopped.
    *   If the worker is stopped while `dequeue` is waiting, it will return `NIL`.
    *   This operation is thread-safe.

*   **`queue-size (queue job-queue)`**:
    *   **Note:** This function is exported but **not implemented** in the provided code.

*   **`clear-queue (queue job-queue)`**:
    *   **Note:** This function is exported but **not implemented** in the provided code.

### Worker Management

*   **`start-worker (queue job-queue &key name)`**:
    *   Starts the background worker thread.
    *   If `name` is provided, it sets the thread's name.
    *   This function also automatically starts the batch scheduler thread if it's not already running.

*   **`stop-worker (queue job-queue)`**:
    *   Gracefully stops the worker thread and the batch scheduler thread.
    *   It signals the worker to stop, notifies any waiting threads, and then joins the worker thread to ensure it has terminated before returning.

*   **`worker-running-p (queue job-queue)`**:
    *   Returns `T` if the worker thread is currently running, `NIL` otherwise.

### Batch Scheduling

*   **`start-batch-scheduler (queue job-queue)`**:
    *   Starts the batch scheduler thread. This thread periodically enqueues a `:batch-flush` task.

*   **`stop-batch-scheduler (queue job-queue)`**:
    *   Stops the batch scheduler thread by signaling it to exit its loop.

### Configuration

*   **`set-processor (queue job-queue) fn`**:
    *   Sets the function `fn` that the worker thread will use to process tasks.

*   **`set-batch-interval (queue job-queue) interval`**:
    *   Sets the interval (in seconds) at which the batch scheduler will enqueue a `:batch-flush` task.

*   **`set-logger (queue job-queue) logger-fn`**:
    *   Sets a logging function `logger-fn`. This function is called with `(level source message &rest args)` when errors occur during task processing or when threads start/stop.

## Design Decisions & Notes

*   **Thread Safety:** All critical sections involving the queue's internal state (`queue-items`) are protected by a `bordeaux-threads:lock`.
*   **Queue Type:** The implementation uses `push` to add items and `pop` to remove them from the head of a list. This makes it a **LIFO (Last-In, First-Out) stack**.
*   **Worker Loop Behavior:** The worker thread continuously attempts to `dequeue` tasks. If the queue is empty, it waits using a `bordeaux-threads:condition-variable`. The `dequeue` operation is designed to return `NIL` if the worker is stopped while it's waiting, allowing for a clean shutdown.
*   **Batch Flush Task:** The batch scheduler sends a special task value, `:batch-flush`. Users must explicitly handle this value within their custom processor function if they wish to perform batch operations.
*   **Automatic Batch Scheduler Start:** The `start-worker` function automatically initiates the batch scheduler if it's not already active.
*   **Missing Functionality:** The exported functions `queue-size` and `clear-queue` are not implemented in the provided code.

## Testing

The project includes a test suite for verification.

*   **System:** `cl-job-queue/test`
*   **Dependency:** `fiveam`

To run the tests:

```lisp
(ql:quickload "cl-job-queue/test")
(fiveam:run-all-tests)
```
```
