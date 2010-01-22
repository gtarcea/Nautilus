;;;; 2008-04-04 20:09:09

(defpackage #:facets-asd
  (:use :cl :asdf))

(in-package :facets-asd)

;;;; Setup C-Source file handling

;; We assume that the compiler is in the path...
(defvar *gcc* "gcc")

;;;; Groveling

(defclass grovel-file (cl-source-file) ())

(defmethod perform ((o compile-op) (c grovel-file))
  (let* ((output-file (car (output-files o c)))
	 (filename (component-pathname c))
	 (c-source (merge-pathnames "tmp.c" output-file))
	 (a-dot-out (merge-pathnames "a.out" output-file))
	 (constants (merge-pathnames "grovel.lisp-temp" output-file))
	 (*grovel*))
    (declare (special *grovel*))
    (load filename)
    (and (funcall (the function *grovel*) c-source a-dot-out constants)
	 (compile-file constants :output-file output-file))))

(defsystem facets
  :name "facet"
  :version "0.1"
  :maintainer "V. Glenn Tarcea"
  :author "V. Glenn Tarcea <gtarcea@umich.edu>"
  :description "A persistent distributed object store"
  :components ((:file "defpackage")
	       (:file "macros" :depends-on ("defpackage"))
               (:file "metaclass" :depends-on ("defpackage"))
       	       (:file "facet" :depends-on ("defpackage" "metaclass"))
       	       (:file "class-local-shared-slots" :depends-on ("defpackage" "metaclass"))
	       (:file "class-system-shared-slots" :depends-on ("defpackage" "metaclass"))
       	       (:file "class-distributed-shared-slots" :depends-on ("defpackage" "metaclass")))
  :depends-on (:bordeaux-threads))

;;;; TODO: Setup automated tests using 5am!
