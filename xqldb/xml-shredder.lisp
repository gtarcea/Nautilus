;;;; 2008-04-04 20:09:09
;;;; This is your lisp file. There are many like it, but this one is yours.
(declaim (optimize (speed 0) (safety 3) (debug 3)))
(in-package :xqldb)

;;;; TODO: Turn these closure declaration into a macro to define each, eg: (make-incrementer depth) would generate the code for depth

;;;;<Counters>
;;;; Make the counters needed to track the various ids, and depth

(make-counter depth)
(make-counter tag-id)
(make-counter attribute-id)
(make-counter element-id)

;;;;<Element-Tag-Info hash, and struct>

(defvar *element-tag-hash* (make-hash-table))

(defstruct element-tag-info depth tag tag-id)

(defun get-tag-info (tag depth)
  (let ((key (concatenate 'string tag ":" (write-to-string depth))))
    ;(format t "get-tag-info get: ~A ~%" key)
    (gethash key *element-tag-hash*)))

(defun add-tag-info (tag depth tag-info)
  (let ((key (concatenate 'string tag "__" (write-to-string depth))))
    (setf (gethash key *element-tag-hash*) tag-info)))

;;;;<Test Load Functions>
(defun test-load-mimi ()
  (with-open-file (str "/tmp/mimi.out" :direction :output
                       :if-exists :supersede)
                  (klacks:with-open-source (s (cxml:make-source #p"/Users/gtarcea/Documents/workspace/xmldata/mimi10.xml"))
                                           (loop
                                            for key = (klacks:peek s)
                                            while key
                                            do
                                            (case key
                                                  (:start-element
                                                    (format str "~A {" (klacks:current-qname s)))
                                                  (:end-element
                                                    (format str "} ~%")))
                                            (klacks:consume s)))))

(defun test-load-mimi-into-elephant (file-name)
  (let ((x 1))
    (klacks:with-open-source (s (cxml:make-source file-name))
                              (loop for key = (klacks:peek s)
                                    while key
                                    do
                                    (write-batch s)
                                    ;(format t "wrote ~D ~%" (* x 2000))
                                    (setf x (1+ x))))))

;;;;<Tag stack>

(let ((tag-stack (make-array 0 :fill-pointer t :adjustable t))
      (tag-stack-size 0))
  
  (defun tag-stack-push-element (element)
    (vector-push-extend element tag-stack)
    (incf tag-stack-size))
  
  (defun tag-stack-pop-element ()
    (if (= tag-stack-size 0)
        nil
        (progn
          (decf tag-stack-size)
          (vector-pop tag-stack))))
  
  (defun tag-stack-get-top-element ()
    (if (= tag-stack-size 0)
        nil
        (aref tag-stack (1- tag-stack-size))))
  
  (defun reset-tag-stack ()
    (setf tag-stack-size 0)
    (setf tag-stack (make-array 0 :fill-pointer t :adjustable t))))


(let ((element-id-stack (make-array 0 :fill-pointer t :adjustable t))
      (element-id-stack-size 0))
  
  (defun element-id-stack-push-id (id)
    (vector-push-extend id element-id-stack)
    (incf element-id-stack-size))
  
  (defun element-id-stack-pop-element ()
    (if (= element-id-stack-size 0)
        nil
        (progn
          (decf element-id-stack-size)
          (vector-pop element-id-stack))))
  
  (defun element-id-stack-get-top-element ()
    (if (= element-id-stack-size 0)
        nil
        (aref element-id-stack (1- element-id-stack-size))))
  
  (defun element-id-stack-reset ()
    (setf element-id-stack-size 0)
    (setf element-id-stack (make-array 0 :fill-pointer t :adjustable t))))

;;;;<klacks handlers>
(defun save-attributes (element tag-info stream)
  (ensure-transaction ()
    ;(format t "save-attributes: about to assign element ~D ~%" (element-tag-info-tag-id tag-info))
    (klacks:map-attributes #'(lambda (namespace-uri local-name qualified-name attribute-value b)
                               ;(format t "inside lambda about to create attrib ~%")
                               (let ((attrib (make-xml-attribute-p (element-tag-info-tag-id tag-info) (increment-attribute-id) (increment-tag-id) attribute-value)))
                                 ;(format t "=== Made new attribute with id ~D~%" (get-attribute-id))
                                 ;(format t "adding attribute ~A value ~A to element ~A (id ~D) ~%" local-name attribute-value (element-tag-info-tag tag-info) (xml-element-id-p element))
                                 (add-attribute element attrib)))
                           stream)))

(defun handle-klacks-start-element (stream)
  "Handles the klacks :start-element event. When a start element is seen we need to determine depth,
get attributes, and save the current tag."
  (ensure-transaction ()
    (increment-depth)
    (let* ((current-tag (klacks:current-qname stream))
           (tag-info (get-tag-info current-tag (get-depth))))
      (if (null tag-info)
          (progn
            (setf tag-info (make-element-tag-info :depth (get-depth) :tag current-tag :tag-id (increment-tag-id)))
            (add-tag-info current-tag (get-depth) tag-info)))
            ;(format t "element-tag ~A was given id ~D at depth ~D ~%" current-tag (get-tag-id) (get-depth)))
          ;(format t "Already saw element-tag ~A is has id ~D, we are at depth ~D ~%" current-tag (element-tag-info-tag-id tag-info) (get-depth)))
      (setf new-element (make-xml-element-p current-tag (get-tag-id) (increment-element-id) (get-depth)))
      ;(format t "=== Make New Element with id ~D~%" (get-element-id))
      (element-id-stack-push-id (get-element-id))
      (tag-stack-push-element current-tag)
      ;(format t "  and has xml-element-tag-id-p ~D~%" (xml-element-id-p new-element))
      (save-attributes new-element tag-info stream)
      (get-element-id))))

(defun handle-klacks-end-element (stream)
  "Handles the end element tag. Pops the stack so we know which element to work on. Returns the current tag"
  (decrement-depth)
  (element-id-stack-pop-element)
  (tag-stack-pop-element)
  ;(format t "handle-klacks-end-element depth ~D ~A ~%" (get-depth) (tag-stack-get-top-element))
  (element-id-stack-get-top-element))

(defun handle-klacks-characters (stream current-tag current-element-id)
  "Handle the klacks :character event. Save the values for the current tag (if there is a value)"
  (let ((cur-value (klacks:current-characters stream)))
    (if (not (null (find-if #'(lambda (c)
                           (char/= c #\Newline)) cur-value)))
        (progn
          ;(format t "element-tag ~A (id ~D) has value '~A' ~%" current-tag current-element-id cur-value)
          (add-value-to-element current-element-id cur-value)))))

(defun add-value-to-element (element-id tag-value)
  (ensure-transaction ()
    (let ((element (get-instance-by-value 'xml-element-p 'element-id-p element-id)))
      (if (null element)
            (error "element is NULL"))
      ;(format t "Setting element ~D tag-value to ~A ~%" (xml-element-id-p element) tag-value)
      (setf (xml-element-tag-value-p element) tag-value))))

;;;;
;;;;<Batch Loader>
;;;
;;; Writes a batch of xml to the object store.
;;; TODO: Make writing to the object store really keep track of xml values
;;; TODO: consider how we might do a hierarchy of objects
;;;

(defparameter *current-tag-info* nil)
(defparameter *current-tag* nil)
(defparameter *current-element-id* nil)
(defparameter *batch-count* 1)

(defun init-globals ()
  (setf *current-tag-info* nil
        *current-tag* nil
        *current-element-id* nil
        *batch-count* 1))

(defun write-batch (s)
  ;(format t "write-batch")
  (let ((x 1))
    (with-transaction (:txn-nosync t)
      (loop for mykey = (klacks:peek s)
            while mykey
            do
            (case mykey
                  (:start-element
                    ;(format t ":start-element ~%")
                    (incf x)
                    (setf *current-element-id* (handle-klacks-start-element s))
                    ;(format t "=== : start-element ~D~%" *current-element-id*)
                    (setf *current-tag* (klacks:current-qname s)))

                  (:characters
                    ;(format t ":characters ~%") 
                    ;(format t "=== : characters ~D~%" *current-element-id*)
                    (handle-klacks-characters s *current-tag* *current-element-id*))

                  (:end-element
                    ;(format t ":end-element ~%")
                    (setf *current-element-id* (handle-klacks-end-element s))
                    ;(format t "===: end-element ~D~%" *current-element-id*)
                    (setf *current-tag* (tag-stack-get-top-element))))
                    ;(format t "current-element-id ~A ~%" *current-element-id*)))
                    ;(if (not (null current-tag-info))
                     ;   (progn
                      ;    (format t "We just ended element-tag ~A and decreased depth to ~D ~%" (klacks:current-qname s) (get-depth))
                       ;   (format t "We just set current-tag to ~A ~%" current-tag)))))
            
            (klacks:consume s)
            (if (= x 500)
                (progn
                  (if (= 0 (mod (* x *batch-count*) 10000))
                      (format t "Wrote batch  total transactions ~D ~%" (* x *batch-count*)))
                  (incf *batch-count*)
                  (return)))))))

;;;;<Convenience "main" loader" for testing>
(defun init-counters ()
  (reset-depth)
  (reset-tag-id)
  (reset-element-id)
  (reset-attribute-id))

(defun doeload ()
  (open-store '(:BDB "/tmp/tdb"))
  ;(format t "cachesize ~D~%" *berkeley-db-cachesize*)
  (init-counters)
  (test-load-mimi-into-elephant #p"/Users/gtarcea/Documents/workspace/xmldata/mimi200k.xml")
  (close-store))


