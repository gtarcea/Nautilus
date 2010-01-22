(in-package :db-facets)

(defclass facets-store-controller (store-controller facet) ()
  (:documentation "Implements the store-controller for a facet"))

(eval-when (:compile-toplevel :load-toplevel)
  (register-data-store-con-init :facet 'facet-test-and-construct))

(defun facet-test-and-construct (spec)
  "Entry function for making facet data store controllers"
  (if (facet-store-spec-p spec)
      (make-instance 'facets-store-controller :spec spec)
      (error (format nil "Not a facet spec: ~A~%" spec))))

(defun facet-store-spec-p (spec)
  (and (listp spec)
       (eq (first spec) :facet)))

(defmethod open-controller ((sc facets-store-controller) &key)
  nil)

(defmethod next-oid ((sc facets-store-controller))
  nil)

(defmethod next-cid ((sc facets-store-controller))
  nil)

;;;;

;; The routine below is from cl-prevalence and should be moved. However, we can
;; use it as the way to serialize and deserialize objects to shared memory.

(defmethod serialize-sexp-internal ((object structure-object) stream serialization-state)
  (let ((id (known-object-id serialization-state object)))
    (if id
	(progn
	  (write-string "(:REF . " stream)
	  (prin1 id stream)
	  (write-string ")" stream))
	(let ((serializable-slots (get-serializable-slots serialization-state object)))
	  (setf id (set-known-object serialization-state object))
	  (write-string "(:STRUCT " stream)
	  (prin1 id stream)
	  (write-string " :CLASS " stream)
	  (print-symbol (class-name (class-of object)) stream)
	  (when serializable-slots
	    (write-string " :SLOTS (" stream)
	    (mapc #'(lambda (slot)
		      (write-string " (" stream)
		      (print-symbol slot stream)
		      (write-string " . " stream)
		      (serialize-sexp-internal (slot-value object slot) stream serialization-state)
		      (write-string ")" stream))
		  serializable-slots))
	  (write-string " ) )" stream)))))

  
  