(declaim (optimize (speed 0) (safety 3) (debug 3)))
(in-package :multi-format-stor)

(defpclass document-meta-meta ()
  ((id :initarg :id
       :accessor document-meta-id
       :index t
       :documentation "The id of the document this meta refers to")
   (current-version :initarg :current-version
		   :accessor document-meta-current-version
		   :documentation "The current version of document"))
  (:documentation "The document-meta-meta class is a meta class on the a document. It currently only
tracks version information, but may be extended in the future to track other information"))

(defun get-document-meta-meta (document-id)
  "Retrieve the document-meta-meta for a given document-id"
  (ele:get-instance-by-value 'document-meta-meta 'id document-id))

(defpclass simple-tag ()
  ((tag-name :documentation "The name of the tag")
   (tag-value :documentation "The value of the tag, may be null"))
  (:documentation "The base class for describing tags. The other types of tags all inherit from this base class"))

(defpclass document-meta-tag (simple-tag)
  ((tag-name :initarg :meta-tag
	     :accessor meta-tag
	     :type string
	     :documentation "The meta tag name")
   (tag-value :initarg :meta-value
	      :accessor meta-value
	      :type string
	      :documentation "The value of the tag"))
  (:documentation "The value associated with the meta tag. May be nil if there is no value associated with the tag"))

(defpclass document-meta-info ()
  ((meta-tags :initarg :meta-tags
	      :initform (ele:make-pset)
	      :accessor meta-tag-list
	      :documentation "A list of meta tags that describe the document to the system")
   (guid :initarg :guid
	 :accessor meta-guid
	 :initform (uuid:make-v4-uuid)
	 :index t
	 :documentation "The GUID for the document"))
  (:documentation "The system meta information that is stored on each document"))

(defpclass document-attribute (simple-tag)
  ((tag-name :initarg :attribute-tag
	     :accessor attribute-tag
	     :type string
	     :documentation "The tag associated with the attribute")
   (tag-value :initarg :attribute-value
	      :accessor attribute-value
	      :type string
	      :documentation "The value associated with the tag. May be nil if no value associated with tag"))
  (:documentation "The name and value for an attribute"))

(defpclass document-pointer (simple-tag)
  ((guid :initarg :pointer-id
	 :accessor document-guid
	 :documentation "The GUID to another document")
   (tag-name :initarg :dp-meta-name
	     :accessor dp-meta-name)
   (tag-value :initarg :dp-meta-value
	      :accessor dp-meta-value))
  (:documentation "The link to another document"))

(defpclass document (document-meta-info)
  ((version :initarg :version
	    :initform 0
	    :accessor document-version
	    :index t;)
	    :documentation "The version of the document")
   (id :initarg :id
       :accessor document-id
       :index t;)
       :documentation "The document guid")
   (data :initarg :data
	 :accessor document-data
	 :documentation "The data associated with this document")
   (children :initform (ele:make-pset)
	     :accessor document-children
	     :documentation "Children links")
   (parents :initform (ele:make-pset)
	    :accessor document-parents
	    :documentation "Parent links")
   (attributes :initform (ele:make-pset)
	       :accessor document-attributes
	       :documentation "A list of attributes associated with the document"))
  (:documentation "A document object stored in the system"))

(defgeneric xml-document-p (doc)
  (:documentation "Is this an XML document? Should be overridden in xml-document")
  (:method ((doc document)) nil))

(defgeneric json-document-p (doc)
  (:documentation "Is this an JSON document? Should be overridden in json-document")
  (:method ((doc document)) nil))

(defgeneric sexp-document-p (doc)
  (:documentation "Is this an SEXP document? Should be overridden in sexp-document")
  (:method ((doc document)) nil))

(defpclass xml-document (document) ()
  (:documentation "An XML document"))

(defmethod xml-document-p ((doc xml-document))
  "We are of type xml-document"
  t)

(defpclass json-document (document) ()
  (:documentation "An JSON document"))

(defmethod json-document-p ((doc json-document))
  "We are of type json-document"
  t)

(defpclass sexp-document (document) ()
  (:documentation "A SEXP document"))

(defmethod sexp-document-p ((doc sexp-document))
  "We are of type sexp-document"
  t)

(defgeneric verify-document (doc)
  (:documentation "Verifies that the document is the correct type"))

;;;; This is repeated code below... can we refactor to a common set of routines? (same code exists in multi-format-stor.lisp)
;;;; TODO: Look into refactoring this common use case

(defgeneric create-document-for-class (doc-type class stor document-id document-data)
  (:documentation "Create an instance of the class type"))

(defun create-document (doc-type stor document-id document-data)
  (when (null document-id)
    (error "You must specify a document-id"))
  (create-document-for-class doc-type (select-document-class doc-type) stor document-id document-data))

(defvar *document-class-selector* nil)

(defun add-document-class (doc-type class)
  (setf *document-class-selector* (acons doc-type class *document-class-selector*)))

(defun select-document-class (doc-type)
  (let* ((entry (assoc doc-type *document-class-selector*))
	 (class (cdr entry)))
    (if (null entry)
	(error "Cannot create a class of type ~A~%" doc-type)
	class)))

(defun create-document-using (class verify-fn stor document-id document-data)
  "The generic constructor function. It is called with the functions that do verification"
  (cond
    ((and document-data (not (funcall verify-fn document-data))) nil) ; Should we raise a condition instead?
    (t
     ;; Wrap in a transaction at the highest level
     (with-transaction (:store-controller (stor-handle stor))
       (let ((meta (get-document-meta-meta document-id)))
	 (if (null meta)
	     ;; This is the first time we have seen this document-id
	     (initialize-document-first-time stor class document-id document-data)
	     ;; We have seen this document before, so we are updating a current version
	     (create-new-instance-of-existing-document stor class meta document-id document-data)))))))

(defun initialize-document-first-time (stor class document-id document-data)
  "This function sets up all the structures for a first time document. This includes creating the document-meta-meta
that will describe all instances of this document."
  ;; Wrap in a transaction so that all the instance creations happen as an atomic unit.
  (ensure-transaction (:store-controller (stor-handle stor))
    (let ((meta (make-instance 'document-meta-meta :id document-id :current-version 0 :sc (stor-handle stor)))
	  (document (make-instance class :version 0 :id document-id :data document-data :sc (stor-handle stor))))
      document)))

(defun create-new-instance-of-existing-document (stor class meta document-id document-data)
  "We are creating a new instance of an existing document. The meta information contains the current version, we just
need to update the meta to the next version and assign that version to the new instance of the document."
  ;; Wrap in a transaction so that all the retrieves and updates happen as an atomic unit.
  (ensure-transaction (:store-controller (stor-handle stor))
    (let* ((current-version (document-meta-current-version meta))
	   (next-version (1+ current-version))
	   (document (make-instance class :version next-version :id document-id :data document-data :sc (stor-handle stor))))
      (setf (document-meta-current-version meta) next-version) ; This will cause an update through to Elephant
      document)))

;     (make-instance class :id document-id :data document-data :sc (stor-handle stor)))))
;     (let ((version (get-next-version stor document-id)))
;       (make-instance class :version version :id document-id :data document-data :sc (stor-handle stor))))))

(defmethod create-document-for-class ((doc-type (eql :xml)) class stor document-id document-data)
  (create-document-using class #'verify-xml-document stor document-id document-data))

(defmethod create-document-for-class ((doc-type (eql :json)) class stor document-id document-data)
  (create-document-using class #'verify-json-document stor document-id document-data))
   
(defmethod create-document-for-class ((doc-type (eql :sexp)) class stor document-id document-data)
  (create-document-using class #'verify-sexp-document stor document-id document-data))

;;;; Add different doc class types

(add-document-class :xml (find-class 'xml-document))

(add-document-class :json (find-class 'json-document))

(add-document-class :sexp (find-class 'sexp-document))

;;;; General functions for document handling

(defun verify-document-using (fn doc-string)
  "Calls function to verify the document string. Returns t if function returns a t value, otherwise returns nil"
  (cond
    ((ignore-errors (funcall fn doc-string)) t)
    (t nil)))

(defun verify-json-document (json-string)
  (verify-document-using #'json:decode-json-from-string json-string))

(defun verify-xml-document (xml-string)
  (verify-document-using #'xmls:parse xml-string))

(defun verify-sexp-document (sexp-string)
  (if (null sexp-string)
      nil
      (listp sexp-string)))

(defun verify-sexp-xml-document (sexp-list)
  "Verifies that the list passed in can be converted to an xml string"
  (verify-document-using #'(lambda (l)
			     (xmls:write-xml l t)) sexp-list))

(defun verify-sexp-json-document (sexp-list)
  "Verifies that the list passed in can be converted to an json string"
  (verify-document-using #'json:encode-json-to-string sexp-list))

;;;; Set up indexing. We have a few derived indices which we need to tell Elephant about
(defun derived-index-get-latest (doc-instance)
  "This function computes the derived index key for finding the latest version of a document"
  nil)

;;;; Define generics for accessing document information

(defgeneric get-document (stor doc-id)
  (:documentation "Finds a document in the named stor"))

(defgeneric get-all-documents (stor doc-id)
  (:documentation "Finds all the documents with doc-id"))

;(defgeneric 

;;;; Functions for accessing documents
;;(defun get-document (doc-id)
;;  t)

;;(defun get-all-documents (doc-id)
;;  t)

;;(defun purge-documents (doc-id)
;;p  t)
