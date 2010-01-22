;(in-package :com.facets.actors)

(defclass model ()
  ((value :initarg :value
          :documentation "The value of the model.")
   (connections :initform ()
                :documentation "A sequence of objects implementing the model-changed method that
are to be notified when the model's value changes.")
   (dependencies :initform ()
                 :documentation "A sequence of models which should have this model added to their
sequence of connections when activated.")
   (ref-count :initform 0
              :documentation "A reference count of the number of models which depend on this one."))
  (:documentation "A model is a mutable object holding a single value. When the value is changed
a sequence of other objects are notified."))

(defgeneric add-connection (model observer)
  (:documentation "Registers an object interested in being notified of changes to the model's
value. When the value is changed the 'model-changed' method is called on the observer."))

(defgeneric remove-connection (model observer)
  (:documentation "Unregisters an object no longer interested in being notified of changes
to the model's value."))

(defgeneric value (model)
  (:documentation "Retrieves the value of the model."))

(defgeneric (setf value) (new-value model)
  (:documentation "Changes the value of the model and calls 'model-changed' on all registered observers."))

(defgeneric set-value! (new-value model)
  (:documentation "Changes the value of the model without calling any of the registered observers. This
call should be used with care."))

(defgeneric model-changed (observer model)
  (:documentation "Called to notify the observers of a model that the model value has
changed. Observers are registered with 'add-connection'.")
  (:method (observer (model model))
    t))

(defgeneric model-activated (model)
  (:documentation "Called after a model has been activated")
  (:method ((model model))
    t))

(defgeneric update-model (model)
  (:documentation "Called ?")
  (:method ((model model))
    t))

(defgeneric activate-model (model)
  (:documentation "Increments the reference count of the model. If it was previously zero this model
is added as a connection to all models registered as dependencies by the the 'add-dependency' method.
Calls to activate-model and deactivate-model should be balanced otherwise calls to 'model-changed'
might be called at the wrong time, or even not at all."))

(defgeneric deactivate-model (model)
  (:documentation "Decrements the reference count of the model. If it reaches zero this model is
removed as a connection from all models registered as a dependency bo the 'add-dependency' method."))

(defgeneric add-dependency (model dependent)
  (:documentation "Registers a dependency. When model is activated it will be added to the 'dependencies'
connections and notified when 'dependent' changes."))

(defgeneric remove-dependency (model dependent)
  (:documentation "Unregisters a dependency. This method should not be called directly unless you are implementing
your own model class"))

(defgeneric notify-connections (model)
  (:documentation "Notifies all the connected observers that the model changed value"))

(defun make-model (value)
  "Creates a new model with an initial value."
  (make-instance 'model :value value))

(defmethod add-connection ((model model) observer)
  (with-slots (connections) model
    (if (null connections)
        (activate-model model))
    (setf connections (append (list observer) connections))))

(defmethod remove-connection ((model model) observer)
  (with-slots (connections) model
    (setf connections (remove-if #'(lambda (o) (eql o observer)) connections))
    (if (null connections)
        (deactivate-model model))))

(defmethod value ((model model))
  (with-slots (value) model
    value))

;(defmethod notify-connections ((model model))
;  (with-slots (connections) model
;    (mapcar #'(lambda (m) (model-changed m model)) connections)
;    (values)))

(defmethod notify-connections ((model model))
  (with-slots (connections) model
    (dolist (observer connections)
      (model-changed observer model)))
  (values))

(defmethod (setf value) (new-value (model model))
  (with-slots (value) model
    (setf value new-value)
    (update-model model)
    (notify-connections model)))

(defmethod set-value! (new-value (model model))
  (with-slots (value) model
    (setf value new-value)))

;(defmethod activate-model ((model model))
;  (with-slots (ref-count connections) model
;    (setf ref-count (1+ ref-count))
;    (when (= ref-count 1)
;      (mapcar #'(lambda (m) (add-connection m model)) connections)
;      (model-activated model))))

(defmethod activate-model ((model model))
  (with-slots (ref-count dependencies) model
    (setf ref-count (1+ ref-count))
    (when (= ref-count 1)
      (dolist (m dependencies)
        (add-connection model m))
      (model-activated model))))

;(defmethod deactivate-model ((model model))
;  (with-slots (ref-count dependencies) model
;    (setf ref-count (1- ref-count))
;    (when (= 0 ref-count)
;      (mapcar #'(lambda (m) (deactivate-model m) (remove-connection m model)) dependencies))
;    t))

(defmethod deactivate-model ((model model))
  (with-slots (ref-count dependencies) model
    (setf ref-count (1- ref-count))
    (when (= 0 ref-count)
      (dolist (m dependencies)
        (deactivate-model m)
        (remove-connection m model)))))

(defmethod add-dependency ((model model) (dependent model))
  (with-slots (dependencies) model
    (setf dependencies (append (list dependent) dependencies))))

(defmethod remove-dependency ((model model) (dependent model))
  (with-slots (dependencies) model
    (setf dependencies (remove-if #'(lambda (d) (eql d dependent)) dependencies))))

  