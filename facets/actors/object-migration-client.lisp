(in-package :com.facets.actors)

;;;;
;;;; Client code to move objects around
;;;;

;;;;
;;;; Always send the classname (right now we don't associate with packages)
;;;; TODO: Need to handle packages/namespaces
;;;; After the classname send the class, and then the object. We let the
;;;; other side make the decision if it already knows about the class
;;;; definition.
;;;;
;;;; No error handling - this is a very simple version.
;;;;

;; Replace all writes with cl-store:store. Not sure if there are cl-store:*xx* variables that need
;; to be set on this side to control some of the behaviors
(defun migrate-object (socket obj)
  (let* ((class (class-name (class-of obj)))
         (stream (usocket:socket-stream socket)))
    (write class :stream stream)
    (finish-output stream)
    (cl-store:store (class-of obj) stream)
    (finish-output stream)
    (cl-store:store obj stream)))
      
