;(in-package :skipdb)

(defclass sl-observer ()
  ((skipdir :initarg :skipdir
            :accessor skipdir))
  (:documentation "Observer class for multi-methods on model for skiplists"))

(defmethod set-data (data (model model))
  (with-slots (value) model
    (setf (tstruct-data (value model)) data)
    (update-model model)
    (notify-connections model)))

(defmethod model-changed ((observer sl-observer) (model model))
  (let ((filepath (db-dir-file-path (skipnode-key (value model)))))
    (cl-store:store (skipnode-data (value model)) filepath)))