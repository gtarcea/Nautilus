;;;;
;;;; Local shared class slot definition:
;;;; A local-shared-slot is one that can be shared amongst threads, but not amongst processe
;;;;

(in-package :facets)

(defclass facet-local-shared-class-slot-definition (standard-slot-definition)
  ((slot-lock :initform (bt:make-lock)
	      :accessor slot-lock)
   (local-shared :initform t :initarg :local-shared)))

(defclass facet-local-shared-class-direct-slot-definition
    (standard-direct-slot-definition facet-local-shared-class-slot-definition)())

(defclass facet-local-shared-class-effective-slot-definition
    (standard-effective-slot-definition facet-local-shared-class-slot-definition)())

(defgeneric facet-local-shared-class-p (class)
  (:method ((class t)) nil)
  (:method ((class facets-metaclass)) t)
  (:method ((class facet-local-shared-class-slot-definition)) t))

(defmethod slot-value-using-class :around ((class facets-metaclass) (instance facet-instance)
					   (slot-class facet-local-shared-class-slot-definition))
  ;; Fill in after we know it will compile
  (format t "local slot-value-using-class :acquiring lock~%")
  (bt:with-lock-held ((slot-lock slot-class))
    (let ((result (call-next-method)))
      (format t "local slot-value-using-class :releasing lock~%")
      result)))

(defmethod (setf slot-value-using-class) :around (new-value (class facets-metaclass) (instance facet-instance)
							    (slot-class facet-local-shared-class-slot-definition))
  (format t "local setf slot-value-using-class :acquiring lock~%")
  (bt:with-lock-held ((slot-lock slot-class))
    (let ((result (call-next-method)))
      (format t "local setf slot-value-using-class :releasing lock~%")
      result)))

(defmethod (setf slot-value-using-class) (new-value (class facets-metaclass) (instance facet-instance)
					  (slot-class facet-local-shared-class-slot-definition))
  (format t "  local I am here!~%")
  (call-next-method))

(defmethod slot-makunbound-using-class ((class facets-metaclass) (instance facet-instance)
					(slot-class facet-local-shared-class-slot-definition))
  (format t "slot-makunbound-using-class :acquiring lock")
  (call-next-method))

(defmethod slot-boundp-using-class ((class facets-metaclass) (instance facet-instance)
				    (slot-class facet-local-shared-class-slot-definition))
  (format t "slot-boundp-using-class :acquiring lock")
  (call-next-method))

(defun local-shared-slot-names (class)
  (find-slot-def-names-by-type class 'facet-local-shared-class-effective-slot-definition))

;(defmethod shared-initialize :around ((class facet-
