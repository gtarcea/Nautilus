;;;;
;;;; System shared class slot definition
;;;; A system-shared-slot is one that can be shared amongst processes
;;;;

(in-package :facets)

(defclass facet-system-shared-class-slot-definition (standard-slot-definition)
  ((system-shared :initform t :initarg :system-shared)))

(defclass facet-system-shared-class-direct-slot-definition
    (standard-direct-slot-definition facet-system-shared-class-slot-definition) ())

(defclass facet-system-shared-class-effective-slot-definition
    (standard-effective-slot-definition facet-system-shared-class-slot-definition) ())

(defmethod slot-value-using-class :around ((class facets-metaclass) (instance facet-instance)
					   (slot-class facet-system-shared-class-slot-definition))
  ;; Fill in after we know it will compile
  (format t "system slot-value-using-class :acquiring lock~%")
  (let ((result (call-next-method)))
    (format t "system slot-value-using-class :releasing lock~%")
    result))

(defmethod (setf slot-value-using-class) :around (new-value (class facets-metaclass) (instance facet-instance)
							    (slot-class facet-system-shared-class-slot-definition))
  (format t "system setf slot-value-using-class :acquiring lock~%")
  (let ((result (call-next-method)))
    (format t "system setf slot-value-using-class :releasing lock~%")
    result))

(defmethod (setf slot-value-using-class) (new-value (class facets-metaclass) (instance facet-instance)
					  (slot-class facet-system-shared-class-slot-definition))
  (format t "  system I am here!~%")
  (call-next-method))

(defmethod slot-makunbound-using-class ((class facets-metaclass) (instance facet-instance)
					(slot-class facet-system-shared-class-slot-definition))
  (format t "slot-makunbound-using-class :acquiring lock")
  (call-next-method))

(defmethod slot-boundp-using-class ((class facets-metaclass) (instance facet-instance)
				    (slot-class facet-system-shared-class-slot-definition))
  (format t "slot-boundp-using-class :acquiring lock")
  (call-next-method))

(defgeneric facet-system-shared-class-p (class)
  (:method ((class t)) nil)
  (:method ((class facets-metaclass)) t)
  (:method ((class facet-system-shared-class-slot-definition)) t))

(defun system-shared-slot-names (class)
  (find-slot-def-names-by-type class 'facet-system-shared-class-effective-slot-definition))
