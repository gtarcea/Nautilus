;;;; Created on 2008-04-09 19:54:32

(in-package :xqldb)

;;;;
;;;; Define the structure for the XML objects
;;;;
;;;; TODO: Define which slots are indexed (some will be).
;;;; TODO: Identify if any of the slots should be transient (I don't think any will need to be but... should think this one through).
;;;; TODO: Is there any other inheritance going on here? None of the classes currently inherit from other classes.
;;;;
;;;;<class definitions>

(defpclass xml-element-p ()
  ((element-tag-id-p :initarg :xml-element-tag-id-p
                     :initform -1
                     :type integer
                     :accessor xml-element-tag-id-p
                     :documentation "Each unique tag has a tag id. This allows us to store tags once in a lookuptable")
   (element-tag-string :initarg :xml-element-tag-string
                       :initform nil
                       :type string
                       :accessor xml-element-tag-string
                       :transient t
                       :documentation "The element tag string. Since this may be ready many times, we store it as a transient value. All element tags are mapped to integer values")
   (element-id-p :initarg :xml-element-id-p
                 :initform -1
                 :type integer
                 :index t
                 :accessor xml-element-id-p
                 :documentation "The unique id for this element")
   (element-tag-value-p :initarg :xml-element-tag-value-p
                        :initform nil
                        :type string
                        :accessor xml-element-tag-value-p
                        :documentation "The value association with the element tag, or nil if no value")
   (element-tag-attribute-list-p :initarg :xml-element-tag-attribute-list-p
                                 :initform (make-pset)
                                 :accessor xml-element-tag-attribute-list-p
                                 :documentation "A PSet of class xml-attributes")
   (element-depth-p :initarg :xml-element-depth-p
                    :initform -1
                    :accessor xml-element-depth-p
                    :type integer
                    :documentation "The depth in the tree of this element"))
  (:documentation "This is the base persistent xml element class. This class is used when loading data (automatically persists
    and as a shadow class when reading. For reads this allows object storage read, and then the transient class can be used
    for all manipulation."))

(defpclass xml-attribute-p ()
  ((element-id-p :initarg :xml-element-id-p
                 :initform -1
                 :accessor xml-element-id-p
                 :type integer
                 :documentation "The element this attribute is associated with")
   (attribute-id-p :initarg :xml-attribute-id-p
                   :initform -1
                   :accessor xml-attribute-id-p
                   :type integer
                   :documentation 
                   "Each attribute has a unique id. This is different than the attribute-tag-id that is used to map string attribute names to integers")
   (attribute-tag-id-p :initarg :xml-attribute-tag-id-p
                       :initform -1
                       :accessor xml-attribute-tag-id-p
                       :type integer
                       :documentation "Each attribute has a unique id. This allows us to store attribute tags once in a lookup table")
   (attribute-tag-string-t :initarg :xml-attribute-tag-string
                           :initform nil
                           :type string
                           :accessor xml-attribute-tag-string
                           :transient t
                           :documentation 
                           "The attribute string. Even though this is mapped to an integer, we keep the attribute string around. It's transient because it may be read many times.")
   (attribute-tag-value-p :initarg :xml-attribute-tag-value-p
                          :type string
                          :accessor xml-attribute-tag-value-p
                          :initform nil
                          :documentation "The value associated with this attribute or nil if no value"))
  (:documentation "The base class for xml attributes"))

(defpclass xml-element-tag ()
  ((element-tag-string :initarg :xml-element-tag-string
                       :initform nil
                       :accessor xml-element-tag-string
                       :type string
                       :documentation "The string representing the tag, eg the 'element' in <element>")
   (element-tag-id :initarg :xml-element-tag-id
                   :initform -1
                   :accessor xml-element-tag-id
                   :type integer
                   :documentation "The integer id we use as the stand in for the tag"))
  (:documentation "The individual record to map tags to integer ids"))

(defpclass xml-attribute-tag ()
  ((attribute-tag-string :initarg :xml-attribute-tag-string
                         :initform nil
                         :accessor xml-attribute-tag-string
                         :type string
                         :documentation "The string represent the attribute tag, eg the 'x' in <element x='abc'>")
   (attribute-tag-id :initarg :xml-attribute-tag-id
                     :initform -1
                     :accessor xml-attribute-tag-id
                     :type integer
                     :documentation "The integer id we use as the stand in for the attribute tag"))
  (:documentation "The individual record to map attribute tags to integer ids"))



;;;; Simple allocation functions to create the classes
;;;;<make class functions>
(defun make-xml-element-p (tag-string tag-id id depth)
  (make-instance 'xml-element-p
                 :xml-element-tag-id-p tag-id
                 :xml-element-id-p id
                 :xml-element-tag-string tag-string
                 :xml-element-depth-p depth))

(defun make-xml-attribute-p (element-id attribute-id attribute-tag-id attribute-value)
  (make-instance 'xml-attribute-p
                 :xml-element-id-p element-id
                 :xml-attribute-id-p attribute-id
                 :xml-attribute-tag-value-p attribute-value
                 :xml-attribute-tag-id-p attribute-tag-id))

(defun make-xml-element-tag (tag id)
  (make-instance 'xml-element-tag
                 :xml-element-tag-string tag
                 :xml-element-tag-id id))

(defun make-xml-attribute-tag (tag id)
  (make-instance 'xml-attribute-tag
                 :xml-attribute-tag-string tag
                 :xml-attribute-tag-id id))

;;;; Methods on classes
;;;;<generics>
(defgeneric add-attribute (element attribute)
  (:documentation "Adds attribute to element")) 

(defgeneric remove-attribute (element attribute)
  (:documentation "Removes attribute from element"))

(defgeneric map-element-attributes (fn element)
  (:documentation "Like map, takes a function and an element and applies the function to element attribute in the element"))

;;;;<methods>
(defmethod add-attribute ((element xml-element-p) (attribute xml-attribute-p))
  (insert-item attribute (xml-element-tag-attribute-list-p element))) 

(defmethod remove-attribute ((element xml-element-p) (attribute xml-attribute-p))
  (remove-item attribute (xml-element-tag-attribute-list-p element)))

(defmethod map-element-attributes (fn (element xml-element-p))
  (map-pset fn (xml-element-tag-attribute-list-p element)))