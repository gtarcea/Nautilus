;;;;
;;;; Distributed shared class slot definition
;;;; A distributed-shared-slot is one that can be shared across multiple computer systems
;;;;

(in-package :facets)

(defclass facet-distributed-shared-class-slot-definition (standard-slot-definition)
  ((distributed-shared :initform t :initarg :distributed-shared)))

(defclass facet-distributed-shared-class-direct-slot-definition
    (standard-direct-slot-definition facet-system-shared-class-slot-definition) ())

(defclass facet-distributed-shared-class-effective-slot-definition
    (standard-effective-slot-definition facet-system-shared-class-slot-definition) ())

(defgeneric facet-distributed-shared-class-p (class)
  (:method ((class t)) nil)
  (:method ((class facets-metaclass)) t)
  (:method ((class facet-distributed-shared-class-slot-definition)) t))

(defmethod slot-value-using-class :around ((class facets-metaclass) (instance facet-instance)
					   (slot-class facet-distributed-shared-class-slot-definition))
  ;; Fill in after we know it will compile
  (format t "distributed slot-value-using-class :acquiring lock~%")
  (let ((result (call-next-method)))
    (format t "distributed slot-value-using-class :releasing lock~%")
    result))

(defmethod (setf slot-value-using-class) :around (new-value (class facets-metaclass) (instance facet-instance)
							    (slot-class facet-distributed-shared-class-slot-definition))
  (format t "distributed setf slot-value-using-class :acquiring lock~%")
  (let ((result (call-next-method)))
    (format t "distributed setf slot-value-using-class :releasing lock~%")
    result))

(defmethod (setf slot-value-using-class) (new-value (class facets-metaclass) (instance facet-instance)
					  (slot-class facet-distributed-shared-class-slot-definition))
  (format t "  distributed I am here!~%")
  (call-next-method))

(defmethod slot-makunbound-using-class ((class facets-metaclass) (instance facet-instance)
					(slot-class facet-distributed-shared-class-slot-definition))
  (format t "slot-makunbound-using-class :acquiring lock")
  (call-next-method))

(defmethod slot-boundp-using-class ((class facets-metaclass) (instance facet-instance)
				    (slot-class facet-distributed-shared-class-slot-definition))
  (format t "slot-boundp-using-class :acquiring lock")
  (call-next-method))

(defun distributed-shared-slot-names (class)
  (find-slot-def-names-by-type class 'facet-distributed-shared-class-effective-slot-definition))
