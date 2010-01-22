;;;;
;;;; PUT SOME COMMENTS HERE
;;;;
(declaim (optimize (speed 0) (safety 3) (debug 3)))
(in-package :facets)

(defclass facet ()
  ((facet-id :initarg :facet-id
	     :accessor facet-id
	     :documentation "A globally unique identifier for this facet")
   (facet-cache-type :initarg :facet-cache-type
		     :accessor facet-cache-type
		     :documentation "The type of caching to apply")))


(defmethod initialize-instance :before ((instance facet) &rest initargs &key facet-id)
  (initial-facet-setup instance :facet-id facet-id))

(defun initial-facet-setup (instance &key facet-id)
  (declare (ignore instance))
  nil)

(defclass facet-instance (facet) ()
  (:metaclass facets-metaclass)
  (:documentation "Something here"))

;;;;
;;;; MOP Stuff for Metaclass initialization
;;;;

(defmethod shared-initialize :around ((class facets-metaclass) slot-names &rest args &key direct-superclasses)
  (let* ((new-direct-superclasses (ensure-class-inherits-from class 'facet-instance direct-superclasses)))
    (apply #'call-next-method class slot-names :direct-superclasses new-direct-superclasses args)))

(defun ensure-class-inherits-from (class from-classname direct-superclasses)
  (let* ((from-class (find-class from-classname))
	 (has-facet-instance (superclass-member-p from-class direct-superclasses)))
    (if (not (or (eq class from-class) has-facet-instance))
	(append direct-superclasses (list from-class))
	direct-superclasses)))

(defun superclass-member-p (class superclasses)
  (some #'(lambda (superclass)
	    (or (eq class superclass)
		(let ((supers (class-direct-superclasses superclass)))
		  (when supers
		    (superclass-member-p class supers)))))
	superclasses))

;;
;; Is this call needed?
;(defmethod finalize-inheritance :after ((instance facets-metaclass))
;  nil)

;;;;
;;;; User classes inherit from facet-instance. In this case we are following the Elephant model and leaving
;;;; the Facet class as an internal class that might be used elsewhere and that won't do everything that
;;;; the facet-instance class does.
;;;;

;;;;
;;;; Ok, lets take a little more code from the 'Elephant' way. As we climb this learning curve of using the MOP
;;;; Elephant is proving to be a wonderful template from which to learn from, and to find nice bits and pieces
;;;; that make life easier.
;;;;

(defmacro bind-slot-defs (class slots bindings &body body)
  (with-gensyms (classref slotrefs)
    `(let* ((,classref ,class)
	    (,slotrefs ,slots)
	    ,@(compute-bindings classref slotrefs bindings))
    ,@body)))

(eval-when (:compile-toplevel :load-toplevel)
  (defun compute-bindings (class slots bindings)
    (loop for (name accessor) in bindings
	 collect `(,name (get-init-slotnames ,class #',accessor ,slots)))))

(defun get-init-slotnames (class accessor slot-names)
  (let ((slotnames (funcall accessor class)))
    (if (not (eq slot-names t))
	(intersection slotnames slot-names :test #'equal)
	slotnames)))

;;
;; We might need to do a call-next-method here to initialize class variables that are not a facet
;;

(defmethod initialize-instance :around ((instance facet-instance) &rest initargs &key something &allow-other-keys)
  (call-next-method))

(defmethod shared-initialize :around ((instance facet-instance) slot-names &rest initargs &key something &allow-other-keys) ; do we need other keys?
  (format t "facets shared-initialize :around ~%~A~%~A~%" slot-names initargs)
  (bind-slot-defs (class-of instance) slot-names
		  ((distributed-shared-slots distributed-shared-slot-names)
		   (local-shared-slots local-shared-slot-names)
		   (system-shared-slots system-shared-slot-names))
		  (when distributed-shared-slots
		    (initialize-distributed-shared-slots distributed-shared-slots))
		  (when local-shared-slots
		    (initialize-local-shared-slots local-shared-slots))
		  (when system-shared-slots
		    (initialize-system-shared-slots system-shared-slots))))

(defun initialize-distributed-shared-slots (slots)
  (declare (ignore slots))
  nil)

(defun initialize-local-shared-slots (slots)
  (declare (ignore slots))
  nil)

(defun initialize-system-shared-slots (slots)
  (declare (ignore slots))
  nil)


