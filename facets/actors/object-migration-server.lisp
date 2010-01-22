(in-package :com.facets.actors)

;; Replace all reads with cl-store:restore. May need to set some of the cl-store:*xxx* variables to control
;; the behavior.
(defun receive-migrated-object (socket)
  (let* ((stream (usocket:socket-stream socket))
         (class (read stream))
         (known-class? (find-class class nil)))
    (if (null known-class?)
        ;; need to define class in environment
        (cl-store:restore stream)
        ;; class already known, just toss away
        (read stream))
    ;; create object
    (cl-store:restore stream)))

    

