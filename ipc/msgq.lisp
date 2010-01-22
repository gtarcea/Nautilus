(in-package :cl-lwipc)

(defclass msgq ()
  ((msgqkey :initarg :msgqkey
	    :accessor msgqkey
	    :documentation "The msgq key/name")
   (msgqid :initarg :msgqid
	   :accessor msgqid
	   :documentation "The internal id for the msgq"))
  (:documentation "The class for accessing a message queue"))

(defun make-msgq (key flags &key (create t))
  "Creates and attaches to a message queue"
  (let ((msgqid (msgq-exists key)))
    (cond
      ((>= msgqid 0) (make-instance 'msgq :msgqid msgqid :msgqkey key))
      (create
       (setf msgqid (msg-get key flags))
       (if (>= msgqid 0)
	   (make-instance 'msgq :msgqid msgqid :msgqkey key)
	   nil))
      (t nil))))

(defun msgq-exists (key)
  "Checks to see if the msgq already exists"
  (msg-get key 0))

