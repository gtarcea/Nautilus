(defpackage #:com.facets.concurrent-asd
  (:use :cl :asdf))

(in-package :com.facets.concurrent-asd)

;;;; Setup C-source file handling

;; Assume gcc is in the path

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

(defsystem concurrent
  :name "concurrent"
  :version "0.1"
  :maintainer "V. Glenn Tarcea"
  :author "V. Glenn Tarcea <gtarcea@umich.edu>"
  :description "Concurrency modules for common lisp"
  :components ((:file "defpackage")
	       (:file "conditions" :depends-on ("defpackage"))
	       (:file "distributed" :depends-on ("defpackage"))
	       (:file "futures" :depends-on ("defpackage"))
	       (:file "promises" :depends-on ("defpackage"))
	       (:file "combinators" :depends-on ("defpackage"))
	       (:file "count-downs" :depends-on ("defpackage"))
	       (:file "exchangers" :depends-on ("defpackage"))
	       (:file "mailboxes" :depends-on ("defpackage"))
	       (:file "flags" :depends-on ("defpackage"))
	       (:file "messaging" :depends-on ("defpackage")))
  :depends-on ())