(in-package :com.facets.actors)

(defconstant +default-thread-count+ 5 "Default number of threads in a thread pool")

(defclass thread-pool ()
  ((priority :initarg :priority
             :accessor priority
             :documentation "Priority of queue")
   (thread-count :initarg :thread-count
                 :accessor thread-count
                 :initform +default-thread-count+
                 :documentation "The number of threads in the pool")
   (pool-status :initarg :pool-status
                :accessor pool-status
                :initform :paused
                :documentation "Pool status - whether or not jobs are being processed in the pool.
The pool can be in the following states -
    :paused - Jobs are not being processed and run. This is the default starting state for a thread-pool.
    :processing - Jobs are being processed and running.
    :cleanup - A transitory state before the pool is deleted.
    :starting - A transitory state before the pool starts processing jobs.
    :initializing - A transitory state as the pool-thread is created and set up.")
   
   (work-queue :initform ()
               :accessor work-queue
               :documentation "The queue of jobs"))
  (:documentation "Defines a thread pool. Each thread pool has a priority relative to other thread pools. (Is this needed?)"))

(defgeneric add-job (pool job)
  (:documentation "Adds a job to a thread-pool"))

(defgeneric remove-job (pool job &key (abort-running nil))
  (:documentation "Removes a job from a thread-pool. Returns nil if the job cannot be removed (because it has already been started).
The user can optionally specify that an already started job be aborted before being removed."))

(defgeneric abort-job (pool job)
  (:documentation "Aborts a job."))

(defgeneric pause (pool)
  (:documentation "Pauses the pool so that no more jobs are processed (currently running jobs are allowed to continue to their next stop point."))

(defgeneric start (pool)
  (:documentation "Starts a thread pool so that it starts processing jobs in its work queue."))