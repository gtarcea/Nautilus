(in-package :facets)

(defclass facets-metaclass (standard-class)
  ((%shmem :initarg :%shmem
	   :accessor %shmem
	   :initform -1
	   :documentation "The shared shared memory segment used")
   (%socket :initarg :%socket
	    :accessor %socket
	    :initform -1
	    :documentation "The socket used for distributed shared variables"))
  (:documentation "The metaclass for facets -- add more documentation"))

;;;;
;;;; Slot MOP support. Facets are only applied to classes with allocation type :class
;;;;

(defmacro bind-standard-init-arguments ((initargs) &body body)
  `(let ((allocation-key (getf ,initargs :allocation))
	 (allocation-class-p)
	 (local-shared-p (getf ,initargs :local-shared))
	 (system-shared-p (getf ,initargs :system-shared))
	 (distributed-shared-p (getf ,initargs :distributed-shared)))
     (when (eq allocation-key :class) (setq allocation-class-p t))
     (when (consp local-shared-p) (setq local-shared-p (car local-shared-p)))
     (when (consp system-shared-p) (setq system-shared-p (car system-shared-p)))
     (when (consp distributed-shared-p) (setq distributed-shared-p (car distributed-shared-p)))
     ,@body))

(defmethod direct-slot-definition-class ((class facets-metaclass) &rest initargs)
  "Checks to make sure that facets are only being applied to class allocation slots and that
the only one of the three slot definitions above is being applied"
  (format t "facets direct-slot-definition-class initargs = ~A~%" initargs)
  (bind-standard-init-arguments (initargs)
				(cond
				  ((and (not allocation-class-p) (or local-shared-p system-shared-p distributed-shared-p))
				   (error "Only :allocaton :class slots can be facets"))
				  ((> (count-specified local-shared-p system-shared-p distributed-shared-p) 1)
				   (error "Only one type of sharing can be specified for a class slot"))
				  (local-shared-p
				   ;(format t "   facet-local-shared-class-direct-slot-definition~%")
				   (find-class 'facet-local-shared-class-direct-slot-definition))
				  (system-shared-p
				   ;(format t "   facet-system-shared-class-direct-slot-definition~%")
				   (find-class 'facet-system-shared-class-direct-slot-definition))
				  (distributed-shared-p
				   ;(format t "   facet-distributed-shared-class-direct-slot-definition~%")
				   (find-class 'facet-distributed-shared-class-direct-slot-definition))
				  (t ;; default to local-shared
				   ;(format t "   standard-direct-slot-definition~%")
				   ;(find-class 'facet-local-shared-class-direct-slot-definition)))))
				   (find-class 'standard-direct-slot-definition)))))

(defmethod effective-slot-definition-class ((class facets-metaclass) &rest initargs)
  "Decide the effective type of slot, we only care about class slots, all other slots we can just return
standard-effective-slot-definition
TODO: How does this work if we combine two different meta-classes?"
  (format t "facets effective-slot-definition-class initargs = ~A~%" initargs)
  (bind-standard-init-arguments (initargs)
				(cond
				  ;((and allocation-class-p local-shared-p)
				  (local-shared-p
				   ;(format t "facet-local-shared-class-effective-slot-definition~%")
				   (find-class 'facet-local-shared-class-effective-slot-definition))
				  ((and allocation-class-p system-shared-p)
				   ;(format t "facet-system-shared-class-effective-slot-definition~%")
				   (find-class 'facet-system-shared-class-effective-slot-definition))
				  ((and allocation-class-p distributed-shared-p)
				   ;(format t "facet-distributed-shared-class-effective-slot-definition~%")
				   (find-class 'facet-distributed-shared-class-effective-slot-definition))
				  (t
				   ;(format t "standard-effective-slot-definition~%")
				   (find-class 'standard-effective-slot-definition)))))

(defmethod compute-effective-slot-definition-initargs ((class facets-metaclass) slot-definitions)
  (format t "facets compute-effective-slot-definition-initargs ~A~%" slot-definitions)
  (let ((initargs (call-next-method)))
    (cond
      ((eq (type-of (first slot-definitions)) 'facet-local-shared-class-direct-slot-definition)
       ;(format t "   facet-local-shared-class-direct-slot-definition setting :local-shared flag~%")
       (setf (getf initargs :local-shared) t))
      ((eq (type-of (first slot-definitions)) 'facet-system-shared-class-direct-slot-definition)
       (setf (getf initargs :system-shared) t))
      ((eq (type-of (first slot-definitions)) 'facet-distributed-shared-class-direct-slot-definition)
       (setf (getf initargs :distributed-shared) t)))
    initargs))

(defmethod compute-effective-slot-definition ((class facets-metaclass) slot-name direct-slot-definitions)
  (format t "facets compute-effective-slot-definition slot-name = ~A dsd = ~A~%" slot-name direct-slot-definitions)
  (call-next-method))

;;;;
;;;; Mop Stuff
;;;;

(defmethod validate-superclass ((class facets-metaclass) (super standard-class))
  t)

;;;;
;;;; Utility functions
;;;;

(defun count-specified (&rest args)
  (count t args :key #'(lambda (x) x)))

(defmethod find-slot-defs-by-type ((class facets-metaclass) type &optional (by-subtype t))
  (let ((slot-defs (class-slots class)))
    (loop for slot-def in slot-defs
	 when (if by-subtype
		  (subtypep (type-of slot-def) type)
		  (eq (type-of slot-def) type))
	 collect slot-def)))

(defmethod find-slot-def-names-by-type ((class facets-metaclass) type &optional (by-subtype t))
  (mapcar #'slot-definition-name (find-slot-defs-by-type class type by-subtype)))
