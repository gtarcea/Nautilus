(in-package :common-lisp-user)

(defpackage :facets
  (:use :cl :sb-mop :sb-pcl :bordeaux-threads)
  (:documentation "Shared distributed classes")
  (:import-from :sb-mop
		compute-class-precedence-list
		validate-superclass
		standard-slot-definition
		standard-direct-slot-definition
		standard-effective-slot-definition
		direct-slot-definition-class
		effective-slot-definition-class
		slot-definition-name
		slot-definition-initform
		slot-definition-initfunction
		compute-effective-slot-definition
		class-slots
                class-direct-slots
                class-direct-superclasses
		slot-value-using-class
		slot-boundp-using-class
		slot-makunbound-using-class
		slot-definition-allocation
		slot-definition-initargs
		class-finalized-p
		finalize-inheritance
		ensure-class-using-class
		compute-slots)
  (:import-from :sb-pcl
		initialize-internal-slot-functions
		compute-effective-slot-definition-initargs
		slot-definition-reader-function
		slot-definition-writer-function
		slot-definition-boundp-function
		slot-definition-allocation-class
		class-slot-cells
		plist-value
		+slot-unbound+)
  (:export
   ;; exported symbols here
   #:facets-metaclass
  ))

