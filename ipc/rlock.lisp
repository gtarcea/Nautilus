(in-package :cl-lwipc)

(defclass rlock ()
  ((fd :initarg :fd
       :accessor fdescriptor
       :documentation "The OS file descriptor")
   (lock-type :initarg :lock-type
	       :documentation "The type of lock: f-rdlck or f-wrlck")
   (alien-flock :initarg :flock
		:documentation "The flock structure"))
  (:documentation "The class that implements advisory locking"))

(defgeneric make-rlock (lockf &key read-lock offset whence length)
  (:documentation "Creates an instance of rlock"))

(defgeneric lock-region (rlock cmd type offset whence len)
  (:documentation "Locks a region of the file"))

(defgeneric lock-record (rl &key wait for-read)
  (:documentation "Locks the file region. This presumes that the alien-flock structure has been set up"))

(defgeneric unlock-record (rl)
  (:documentation "Unlocks the file region. This presumes that the alien-flock structure has been set up"))

(defgeneric lock-test (rl)
  (:documentation "Tests the file region to see if it is locked. This presumes that the alien-flock structure has ben set up"))

(defun set-flock (flock type offset whence len)
  (setf (uffi:get-slot-value flock 'flock 'l-type) type)
  (setf (uffi:get-slot-value flock 'flock 'l-start) offset)
  (setf (uffi:get-slot-value flock 'flock 'l-whence) whence)
  (setf (uffi:get-slot-value flock 'flock 'l-len) len))

(defmethod make-rlock ((lockfile string) &key (read-lock t) (offset 0) (whence seek-set) (length 1))
  (let ((stream (open lockfile :direction :output :if-exists :supersede)))
    (format stream "lock file~%")
    (make-rlock (sb-sys::fd-stream-fd stream) :read-lock read-lock :offset offset :whence whence :length length)))

(defmethod make-rlock ((fd integer) &key (read-lock t) (offset 0) (whence seek-set) (length 1))
  (let ((fl (uffi:allocate-foreign-object 'flock))
	(ltype (if read-lock
		   f-rdlck
		   f-wrlck)))
    (set-flock fl ltype offset whence length)
    (make-instance 'rlock :fd fd :flock fl :lock-type ltype)))

(defmethod lock-record ((rl rlock) &key (wait t) (for-read t))
  (with-slots (alien-flock) rl
    (let ((f-command-read-write (if for-read f-rdlck f-wrlck))
	  (f-lock-command (if wait f-setlkwait f-setlk)))
      (lock-region rl f-lock-command f-command-read-write
		   (uffi:get-slot-value alien-flock 'flock 'l-start)
		   (uffi:get-slot-value alien-flock 'flock 'l-whence)
		   (uffi:get-slot-value alien-flock 'flock 'l-len)))))

(defmethod unlock-record ((rl rlock))
  nil)

(defmacro read-lock (rl offset whence len)
  `(lock-region ,rl f-setlk f-rdlck ,offset ,whence ,len))

(defmacro read-lock-wait (rl offset whence len)
  `(lock-region ,rl f-setlkwait f-rdlck ,offset ,whence ,len))

(defmacro write-lock (rl offset whence len)
  `(lock-region ,rl f-setlk f-wrlck ,offset ,whence ,len))

(defmacro write-lock-wait (rl offset whence len)
  `(lock-region ,rl f-setlkwait f-wrlck ,offset ,whence ,len))

(defmacro unlock-region (rl offset whence len)
  `(lock-region ,rl f-setlk f-unlck ,offset ,whence ,len))

(defmethod lock-test ((rl rlock))
  (with-slots (alien-flock) rl
    (rlock-test rl
		(uffi:get-slot-value alien-flock 'flock 'l-type)
		(uffi:get-slot-value alien-flock 'flock 'l-start)
		(uffi:get-slot-value alien-flock 'flock 'l-whence)
		(uffi:get-slot-value alien-flock 'flock 'l-len))))

(defun set-rlock-flock (rl type offset whence len)
  (with-slots (fd alien-flock) rl
    (setf (uffi:get-slot-value alien-flock 'flock 'l-type) type)
    (setf (uffi:get-slot-value alien-flock 'flock 'l-start) offset)
    (setf (uffi:get-slot-value alien-flock 'flock 'l-whence) whence)
    (setf (uffi:get-slot-value alien-flock 'flock 'l-len) len)))

(defun rlock-test (rl type offset whence len)
  (set-rlock-flock rl type offset whence len)
  (let ((rc (fcntl (fdescriptor rl) f-getlk (slot-value rl 'alien-flock))))
    (cond
      ((< rc 0) -1)
      ((= (uffi:get-slot-value (slot-value rl 'alien-flock) 'flock 'l-type) f-unlck) 0)
      (t (uffi:get-slot-value (slot-value rl 'alien-flock) 'flock 'l-pid)))))

(defmethod lock-region ((rl rlock) cmd type offset whence len)
  (set-rlock-flock rl type offset whence len)
  (fcntl (fdescriptor rl) cmd (slot-value rl 'alien-flock)))
  
