(in-package :com.facets.actors)

(define-condition box-full (error) ())

(define-condition box-empty (error) ())

(defclass box ()
  ((occupied :initform nil
             :documentation "Does the box contain a value?")
   (value :initform nil
          :documentation "The value stored in the box"))
  (:documentation "A box is a data element that can hold an element. If you try to retrieve
a value from a box without an element it will raise the box-empty condition, if you try to
store a value in a box that already contains an element it will raise the box-full condition"))

(defgeneric (setf get-value) (value box)
  (:documentation "Stores a value in a box. Raises box-full if a value is
already stored in the box"))

(defgeneric get-value (box)
  (:documentation "Retrieves a value from a box. Raises box-empty if
nothing is stored in the box. The box is empty after this call completes."))

(defgeneric has-value? (box)
  (:documentation "Returns t if the box contains a value, otherwise
returns nil"))

(defgeneric get-?value (box)
  (:documentation "Returns two values t/nil if there is a value stored
and the value/nil. For example, nil,nil means nothing in the box,
t,5 means that the box holds a value, and the value is 5. The box
is empty after this call completes."))

(defgeneric empty-box! (box)
  (:documentation "The box is emptied. Returns t is there was a value
stored in the box, returns nil if the box was already empty. No error
is signaled in either case."))

(defun make-box ()
  (make-instance 'box))

(defmethod (setf get-value) (new-value (box box))
  (with-slots (occupied value) box
    (if occupied
        (error 'box-full))
    (setf value new-value)
    (setf occupied t)))

(defmethod get-value ((box box))
  (with-slots (occupied value) box
    (if (null occupied)
        (error 'box-empty))
    (let ((ovalue value))
      (setf occupied nil)
      (setf value nil)
      ovalue)))

(defmethod has-value? ((box box))
  (with-slots (occupied) box
    occupied))

(defmethod get-?value ((box box))
  (with-slots (occupied value) box
    (let ((o-val value)
          (o-occ occupied))
      (setf occupied nil)
      (setf value nil)
      (values o-val o-occ))))

(defmethod empty-box! ((box box))
  (with-slots (occupied value) box
    (let ((o-occupied occupied))
      (setf value nil)
      (setf occupied nil)
      o-occupied)))