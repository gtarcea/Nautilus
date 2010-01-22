(declaim (optimize (speed 0) (safety 3) (debug 3)))
(in-package :multi-format-stor)

;;;; The base class for stors

(defclass base-stor ()
    ((state :accessor stor-state
	    :initarg :stor-state
	    :documentation "The state of the stor, ie :open or :closed")
     (status :initform :available
	     :accessor stor-status
	     :initarg :stor-status
	     :documentation "The status of the store, ie :recovering, :available")
     (path :initarg :stor-path
	   :accessor stor-path
	   :documentation "Path to stor")
     (handle :accessor stor-handle
	     :documentation "The handle to the data stor"))
  (:documentation "The foundation class for all stors"))

(defgeneric close-stor (stor &key abort)
  (:documentation "Closes the stor. This should be overridden")
  (:method ((stor base-stor) &key abort)
    (declare (ignore abort))
    (ele:close-store (stor-handle stor))
    (setf (stor-state stor) :closed)))

(defun ensure-open-stor (stor)
  "Prevents access to a stor if that stor is not open"
  (let ((state (stor-state stor))
	(status (stor-status stor)))
    (unless (and (equal status :available) (not (equal state :closed)))
      (error "Attempt to use an unopened, or unavailable stor, state ~A status ~A~%" state status))))


(defmethod initialize-instance :after ((stor base-stor) &key)
  "After the data structures are set up we initiate access to the underlying object store"
  (setf (stor-handle stor) (ele:open-store `(:BDB ,(stor-path stor)))))

;;;; Protocol for making new stor classes make-stor and add-stor-class are external protocol functions
;;;; *stor-selector* and select-stor-class are internal protocol functions

(defun open-stor (path)
  "Returns an opened stor of the correct type for the arguments. Calls select-stor-class
to choose the class. Calls make-stor-stream to choose the correct arguments to make-instance
and to create the instance itself. _stor_type_ is a symbol for the storage instance type to
open such as :json, :xml, :sexp, as well as hybrid instances such as :json-sexp. _open_flags_
tells us how to open the store, such as :create or :readonly. _path_ is a string specifying
the path to the stor"
  (make-instance 'base-stor
		 :stor-state :open
		 :stor-status :available
		 :stor-path path))
