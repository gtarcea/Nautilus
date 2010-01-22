;;;; 2008-04-04 20:09:09


(in-package :common-lisp-user)


;;; Should this be moved to its own package? should we rename the query-evaluator directory?
(defpackage :xqldb
  (:nicknames :xqldb)
  (:use :cl :elephant :cxml)
  (:export
   ;; Exported symbols go here
   ))

(defpackage XML
  (:use "COMMON-LISP")
  (:export
   ;; symbols in xml-utilities.lisp:
   "*PRINT-VERBOSE*"
   "*XML-PROLOG*"
   "*XML-VERSION*"
   "*XML-FILE-VERSION*"
   "*XML-FILE-BEING-PARSED*"
   "FORMAT-IF-VERBOSE"
   "PRINT-N-SPACES"
   "MAP-TO-XML"
   "MAP-FROM-XML"
   ;; symbols in element-definition.lisp:
   "NAME"
   "ELEMENT-ATTRIBUTES"
   "CONTENT"
   "START-TAG-ACTIONS"
   "END-TAG-ACTIONS"
   "CONTAINER"
   "XML-ELEMENT"
   ;; symbols in xml-tag-definitions.lisp:
   "*XML-ELEMENT-DEFINITIONS*"
   "DEFXMLELEMENT"
   "IN-XML-DOCUMENT"
   ;; symbols in element-implementation.lisp:
   "GET-ELEMENT-ATTRIBUTE"
   "SET-ELEMENT-ATTRIBUTE"
   "GET-CHILD-ELEMENT"
   "GET-ELEMENT-TEXT-CONTENT"
   "SET-ELEMENT-TEXT-CONTENT"
   ;; symbols in name-character-check.lisp:
   "WHITE-SPACE-P"
   "NAMECHARP"
   "DIGITP"
   "LETTERP"
   "EXTENTERP"
   "IDEOGRAPHICP"
   "COMBINING-CHAR-P"
   "BASE-CHAR-P"
   ;; symbols in xml-parser.lisp:
   "PARSE-XML-FILE"
   "PARSE-XML"
   "READ-PROLOG"
   "READ-ELEMENT"
   "READ-NAME"
   "READ-WHITE-SPACE"
   "READ-EQUAL-SIGN"
   "READ-VALUE"
   "*XML-RELAXED-CASE*"
   ;; symbols in xml-writer.lisp:
   "PRINT-OBJECT"
   "WRITE-ELEMENT"
   ;; symbols in XMLisp.lisp:
   ;; classes
   "XML-SERIALIZER"
   "![CDATA["
   "!DOCTYPE"
   "!-- "
   ;; aggregation methods
   "SET-ATTRIBUTE-VALUE"
   "ADD-SUBOBJECT"
   "ADD-OBJECT-TO-SLOT"
   "CLEANUP-SUB-OBJECT-SLOTS"
   ;; print control methods
   "XML-PRINTABLE-AS-ATTRIBUTE-VALUE-P"
   "XML-PRINTABLE-AS-SUBELEMENT-P"
   "MAP-OBJECT"
   "PRINT-SLOT-WITH-NAME-P"
   "PRINT-SLOT-VALUE-AS-ATTRIBUTE"
   "PRINT-SLOT-VALUE-AS-SUBELEMENT"
   "PRINT-SLOTS"
   "PRINT-TYPED-ATTRIBUTE-VALUE"
   "PRINT-TYPED-SUBELEMENT-VALUE"
   ;; reading
   "LOAD-OBJECT"
   "SAVE-OBJECT"
   "FINISHED-READING"
   "FINISHED-READING-ATTRIBUTES"
   "READ-TYPED-ATTRIBUTE-VALUE"
   ;; variables
   "*XMLISP-OUTPUT-NON-DEFAULT-VALUE-ATTRIBUTES*"
   "DEF-ELEMENT-CLASS-NAME"
   "XML-TAG-NAME-STRING")

  ;; each MOP implementation appears to keep its symbols in a
  ;; different, application specific package: how clever!!  probably
  ;; not a good idea to just USE that entire package
  ;#+:mcl
;  (:import-from "CCL"
;                "SLOT-DEFINITION-NAME" "SLOT-DEFINITION-TYPE"
;                "SLOT-DEFINITION-INITFORM")
;                
;  #+:common-lispworks
;  (:import-from "HARLEQUIN-COMMON-LISP"
;                "CLASS-SLOTS" "SLOT-DEFINITION-NAME" "SLOT-DEFINITION-TYPE"
;                "SLOT-DEFINITION-INITFORM")
;  #+:ccl-5.1
;  (:import-from "CCL"
;                "CLASS-SLOTS")
  #+sbcl
  (:import-from "SB-MOP"
                "CLASS-SLOTS" "SLOT-DEFINITION-INITFORM" "SLOT-DEFINITION-NAME"
                "SLOT-DEFINITION-TYPE"))


