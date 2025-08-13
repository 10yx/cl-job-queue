# cl-job-queue

## Overview
cl-job-queue is a small, dependency-friendly Common Lisp library that provides a thread-safe job queue with background worker(s), periodic batch scheduling, and simple task scheduling (periodic tasks). It uses bordeaux-threads for portability and is designed for background task processing in SBCL (and other Lisps supported by bordeaux-threads).

## Features (elevator pitch)
- Thread-safe enqueue/dequeue of tasks
- Background worker thread that processes queued tasks via a pluggable processor function
- Periodic batch-flush scheduler (enqueue a :batch-flush token every N seconds)
- Scheduling API to run tasks periodically with a unique ID and cancellation
- Simple pluggable logging hooks
- Minimal dependencies: bordeaux-threads (and optional logur for logging integration in your project)

---

## Requirements
- Common Lisp implementation (SBCL tested)
- ASDF to load the system
- Dependencies (declared in the .asd):
  - bordeaux-threads
  - logur (listed as a dependency but not required to use basic features)
- For running tests:
  - fiveam

---

## Installation

1. Clone or add the project to a location ASDF knows (e.g., ~/.roswell/local-projects or your ASDF source-registry).
2. Install dependencies via your preferred distribution method (Quicklisp, Roswell, distro packages).
   - Example with Quicklisp:
     - Quicklisp should already provide bordeaux-threads and fiveam. Ensure they are installed.

3. Load the system:
   - (asdf:load-system :cl-job-queue)
   - To load tests:
   - (asdf:load-system :cl-job-queue/test)

---

## Quickstart / Usage

Load the system, create a queue, set a processor, and start a worker:

```lisp
(asdf:load-system :cl-job-queue)

;; Create a job queue with an optional processor and logger:
(defparameter *queue* (cl-job-queue:make-job-queue
                       :processor (lambda (task) (format t "Processing: ~A~%" task))
                       :batch-interval 2.0))

;; Or set the processor later:
(cl-job-queue:set-processor *queue* (lambda (task) (format t "Got: ~A~%" task)))

;; Enqueue tasks:
(cl-job-queue:enqueue *queue* 'task-1)
(cl-job-queue:enqueue *queue* 'task-2)

;; Start background worker:
(cl-job-queue:start-worker *queue* :name "MyWorker")

;; Stop background worker when done:
(cl-job-queue:stop-worker *queue*)
```

Notes:
- The worker will loop, dequeueing tasks and calling the queue's processor on each task.
- The queue's logger (if set via set-logger) is called with messages for worker/scheduler lifecycle and errors.
- When start-worker is invoked it also starts an internal batch-scheduler thread that periodically enqueues the token :batch-flush using the queue's batch-interval.

---

## Scheduling Periodic Tasks

You can schedule a task to be enqueued at a fixed interval. The scheduler returns a unique ID, which you can use to cancel the scheduled task.

```lisp
;; Schedule a task to be enqueued every 60 seconds after an initial delay of 5 seconds
(defparameter *task-id*
  (cl-job-queue:schedule-task *queue* 'my-periodic-task :interval 60 :delay 5))

;; Cancel a specific task:
(cl-job-queue:cancel-task *queue* *task-id*)

;; Cancel all scheduled tasks:
(cl-job-queue:cancel-all-tasks *queue*)
```

Implementation details:
- Scheduled tasks run in their own threads. cancel-task sets a running flag to false and removes the task from the internal hash table; the task thread will exit by itself.
- cancel-all-tasks iterates over scheduled task IDs and cancels them one-by-one.

---

## API Reference

Public functions/methods (namespaced under cl-job-queue):

- make-job-queue &key processor logger (batch-interval 2.0)
  - Create a new job-queue instance. Optionally provide a processor function and logger function.

- enqueue (queue task)
  - Push a task onto the queue and notify waiting worker(s).

- dequeue (queue)
  - Remove and return the next task. Blocks until a task is available or the worker is stopped.

- start-worker (queue) &key name
  - Start the background worker thread. Optionally provide a name string. Also starts the batch scheduler.

- stop-worker (queue)
  - Stop the worker thread, stop the batch scheduler, and cancel all scheduled tasks. Joins the worker thread.

- set-processor (queue fn)
  - Set processor function for tasks. Processor is called as (funcall fn task).

- set-logger (queue logger-fn)
  - Set a logger function. Logger is called as (funcall logger-fn level component format &rest args) — the code passes :info and :error along with messages; adapt your logger accordingly.

- set-batch-interval (queue interval)
  - Set the batch scheduler interval in seconds.

- start-batch-scheduler (queue)
  - Start the internal batch scheduler thread (used internally by start-worker).

- stop-batch-scheduler (queue)
  - Stop the batch scheduler thread.

- schedule-task (queue task &key (interval 60) (delay 0))
  - Schedule a task to be enqueued repeatedly. Returns a task ID (a gensym) used with cancel-task.

- cancel-task (queue task-id)
  - Cancel a scheduled task by ID. Returns t if a task was found and cancelled.

- cancel-all-tasks (queue)
  - Cancel every scheduled task registered on the queue.

---

## Logging
The library exposes a set-logger hook; it does not enforce any logging framework. The logger is called with various levels (:info, :error) and a component keyword (e.g., 'worker, 'scheduler). You can adapt it to use logur or your own logging function.

Example logger adapter:

```lisp
(defun my-logger (level component fmt &rest args)
  (format t "[~A][~A] " level component)
  (apply #'format t fmt args)
  (terpri))
(cl-job-queue:set-logger *queue* #'my-logger)
```

---

## Tests

A basic test suite exists under test/ using fiveam.

To run tests:
```lisp
(asdf:load-system :cl-job-queue/test)
(cl-job-queue-test:test-cl-job-queue)
```

The provided test checks basic enqueue, processing, and worker start/stop behavior.

---

## Development Notes / Design Decisions
- Threads and synchronization use bordeaux-threads for portability.
- The queue uses a lock + condition variable to coordinate producers and consumers.
- Scheduled tasks are represented by small structs with a thread and running flag; cancel-task clears the flag and removes the entry from the internal hash table. Threads exit on next loop iteration or after sleep.
- stop-worker cancels scheduled tasks and joins the worker thread for safe shutdown.

---

## Contributing
- Open issues or pull requests for bug fixes and small enhancements.
- Keep changes minimal and tests passing.
- Maintain compatibility with bordeaux-threads-supported implementations.

---

## License
- The .asd currently has an empty license field. Please contact the project maintainer or add a license file before distribution.

---

## Contact / Author
- Author listed in asd: ryuei sasaki
