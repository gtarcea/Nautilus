(in-package :com.facets.actors)

(defmacro with-condition-timeout ((milliseconds &body timeout-forms) &body body)
  #+sbcl `(handler-case (sb-ext:with-timeout (/ ,milliseconds 1000) ,@body)
            (sb-ext:timeout () ,@timeout-forms))
  #+allegro `(mp:with-timeout ((/ ,milliseconds 1000) ,@timeout-forms)
               ,@body)
  #+cmu `(mp:with-timeout ((/ ,milliseconds 1000.0) ,@timeout-forms)
           ,@body)
  #-(or sbcl allegro cmu)
  (error "Timeouts not supported for your lisp."))

;;
;; The code below does a timeout while waiting on a condition variable
;; (bt:with-lock-held (lock-var)
;;	   (handler-case (sb-ext:with-timeout 5 (bt:condition-wait cond-var lock-var))
;;	     (sb-ext:timeout () (format t "condition timed out"))))
;;

;(bt:with-lock-held (lock-var) 
;	   (with-condition-timeout (5 (format t "timeout~%")) (bt:condition-wait cond-var lock-var)))
