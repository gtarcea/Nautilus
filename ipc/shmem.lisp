(in-package :cl-lwipc)

(defclass shmem ()
  ((shmemkey :initarg :shmemkey
	     :accessor shmemkey
	     :documentation "The shmem key/name")
   (shmemid :initarg :shmemid
	    :accessor shmemid
	    :documentation "The system V internal id")
   (shmem-alien :initarg :shmem-alien
		:accessor shmem-alien
		:documentation "The Lisp Alien pointer to the shared memory segment")
   (shmem-pointer :initarg :shmem-pointer
		  :accessor shmem-pointer
		  :documentation "The Lisp SAP pointer to the shared memory segment"))
  (:documentation "The class for accessing a shared memory segment"))

(defun make-shmem (key shmem-size shmem-perm &key (create t))
  "Create a shared memory segment if it doesn't exist"
  (let ((rc (shmem-exists key))
	(shmemid))
    (cond
      ((and rc create)
       ;; shmem already exists and we are supposed to create it, so fail
       (return-from make-shmem nil))
      ;; If we are here then the shared memory either doesn't exist, or does and
      ;; create was set to false
      ((not rc)
       ;; Shared memory does not exist (and we are supposed to create it
       (setf shmemid (shm-get key shmem-size shmem-perm))
       (if (< shmemid 0)
	   (return-from make-shmem nil))))
    (let* ((shmem-alien-addr (shm-attach shmemid nil 0))
	   (shmem-instance (make-instance 'shmem :shmemid shmemid
					  :shmem-alien shmem-alien-addr
					  :shmemkey key
					  :shmem-pointer (sb-alien:alien-sap shmem-alien-addr))))
      shmem-instance)))

(defmacro shmem-aref (shmem type &optional (index 0))
  `(cffi:mem-aref (shmem-pointer ,shmem) ,type ,index))

(defun shmem-exists (key)
  "Checks to see if the shared memory segment already exists. The conditions are
shm-get returns -1 and errno = eacess - shmem exists
shm-get returns -1 and errno = enoent - no such shmem
shm-get returns > 0 - shmem exists
t - assume it exists"
  (let ((rc (shm-get key 0 0)))
    (cond
      ((and (= -1 rc)
	    (= errno-enoent (get-errno))) nil)
      ((and (= -1 rc)
	    (= errno-eacess (get-errno))) t)
      ((> rc 0) t)
      (t t))))
