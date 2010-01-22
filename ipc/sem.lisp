(in-package :cl-lwipc)

(defclass semaphore-member ()
  ((member :initarg :member
	   :accessor sem-member
	   :documentation "The member number")))

(defclass semaphore-set ()
  ((semkey :initarg :semkey
	   :accessor semkey
	   :documentation "The semaphore key/name")
   (semid :initarg :semid
	  :accessor semid
	  :documentation "The internal id for the semaphore")
   (nsems :initarg :nsems
	  :accessor nsems
	  :documentation "The number of semaphores in this semaphore set"))
  (:documentation "The class for managing and accessing System V semaphores"))

(defclass semaphore (semaphore-set) ()
  (:documentation "A semaphore set with nsems set to 1"))

(defgeneric delete-semaphore (semaphore)
  (:documentation "Deletes the semaphore from the OS"))

(defgeneric acquire-semaphore (semaphore &key nowait)
  (:documentation "Acquires a semaphore or semaphore set"))

(defmethod delete-semaphore ((semset semaphore-set))
  (del-semaphore (semid semset)))

(defmethod delete-semaphore ((sem semaphore))
  (del-semaphore (semid sem)))

(defun del-semaphore (semid)
  "Deletes a semaphore from the system. This is a two step process:
   a. lock the semaphore
   b. remove the semaphore"
  ;; TODO: Do the locking -- just implement the removal for now
  (sem-ctl1 semid 0 ipc-rmid 0))

(defmethod initialize-instance :after ((semset semaphore-set) &key)
  nil)

(defmethod initialize-instance :after ((sem semaphore) &key)
  nil)

(defun make-semaphore (key flags &key (create t))
  "Creates a semaphore set with nsems set to 1"
  (make-semaphore-set key flags 1 :create create))

(defun make-semaphore-set (key flags nsems &key (create t))
  "Creates and/or gives access to a semaphore set"
  (let ((semid (semaphore-set-exists key)))
    (cond
      ((>= semid 0) (make-instance 'semaphore-set :semkey key :semid semid :nsems nsems))
      (create
       (setf semid (sem-get key nsems flags))
       (if (>= semid 0)
	   (make-instance 'semaphore-set :semkey key :semid semid :nsems nsems)
	   nil))
      (t nil))))

(defmacro semaphore-exists (key)
  `(semaphore-set-exists ,key))

(defun semaphore-set-exists (key)
  "Checks if semaphore exists"
  (sem-get key 0 0))

