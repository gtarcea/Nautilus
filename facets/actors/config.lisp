(in-package :com.facets.actors)

(defclass config-sexp ()
  ((filepath :accessor filepath
             :initarg :filepath)
   (config-parameters :accessor config-parameters
                      :initform nil))
  (:documentation "A configuration file class. The class stores the
configuration parameters in its config-parameters instance"))

(defun make-config-sexp (conf-name)
  (let* ((config-file (make-pathname :defaults (asdf:component-pathname (asdf:component-system (asdf:find-system "actors")))
                                     :name conf-name
                                     :type "sexp"))
         (config-obj (make-instance 'config-sexp :filepath config-file)))
    (if (null config-file)
        (error "No registration-server configuration file"))
    (with-open-file (rconfig config-file :direction :input)
      (setf (config-parameters config-obj) (read rconfig)))
    config-obj))

(defgeneric get-config-param (param config)
  (:documentation "Retrieves the named configuration parameter"))

(defmethod get-config-param (param (config config-file))
  (cdr (assoc param (config-parameters config))))
      
      