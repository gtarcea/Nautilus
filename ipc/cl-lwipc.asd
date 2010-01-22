;;;; 2008-04-04 20:09:09

(defpackage #:cl-lwipc-asd
  (:use :cl :asdf))

(in-package :cl-lwipc-asd)

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

(defsystem cl-lwipc
  :name "cl-lwipc"
  :version "0.1"
  :maintainer "V. Glenn Tarcea"
  :author "V. Glenn Tarcea <gtarcea@umich.edu>"
  :description "An interface to System V and POSIX based IPC"
  :components ((:file "defpackage")
	       (:grovel-file "grovel-constants" :depends-on ("defpackage"))
	       (:file "ipc-uffi" :depends-on ("defpackage"))
	       (:file "util" :depends-on ("defpackage" "ipc-uffi" "grovel-constants"))
	       (:file "msgq" :depends-on ("defpackage" "ipc-uffi" "grovel-constants"))
	       (:file "sem" :depends-on ("defpackage" "ipc-uffi" "grovel-constants"))
	       (:file "rlock" :depends-on ("defpackage" "ipc-uffi" "grovel-constants"))
	       (:file "shmem" :depends-on ("defpackage" "ipc-uffi")))
  :depends-on (:cffi :uffi))

;;;; TODO: Setup automated tests using 5am!
