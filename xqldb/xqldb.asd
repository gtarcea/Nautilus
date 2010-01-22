;;;; 2008-04-04 20:09:09

(defpackage #:xqldb-asd
  (:use :cl :asdf))

(in-package :xqldb-asd)

;;;; Setup C-Source file handling

;; We assume that the compiler is in the path...
(defvar *gcc* "gcc")

(defvar *gcc-options*
  '(#-darwin "-shared"
    #+darwin "-dynamiclib"
    "-fPIC"))

(defun uffi-funcall (fn &rest args)
  (unless (find-package :uffi)
    (asdf:operate 'asdf:load-op :uffi))
  (apply (find-symbol (symbol-name fn) (symbol-name :uffi)) args))

(defmethod output-files ((o compile-op) (c c-source-file))
  (list (make-pathname :name (component-name c)
		       :type (uffi-funcall :default-foreign-library-type)
		       :defaults (component-pathname c))))

(defmethod perform ((o compile-op) (c c-source-file))
  (unless (zerop (run-shell-command "~A ~A ~{~A ~}-o ~A"
				    *gcc*
				    (namestring (component-pathname c))
				    *gcc-options*
				    (namestring (car (output-files o c)))))
    (error 'operation-error :component c :operation o)))

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

(defsystem xqldb
  :name "xqldb"
  :version "0.1"
  :maintainer "V. Glenn Tarcea"
  :author "V. Glenn Tarcea <gtarcea@umich.edu>"
  :description "A Fast Object Store for XML, JSON and SEXP based documents"
  :components ((:file "defpackage")      
               (:file "xml-storage" :depends-on ("defpackage"))
               (:file "xml-element-tracker" :depends-on ("defpackage"))
               (:file "convenience-macros" :depends-on ("defpackage"))
               (:file "xml-shredder" :depends-on ("defpackage" "convenience-macros" "xml-storage"))
	       (:file "snapshot-set" :depends-on ("defpackage"))
	       (:file "utils" :depends-on ("defpackage"))
               (:file "XMLisp" :depends-on ("defpackage"))
               (:module query-evaluator
			:components
			((:file "package")
			 (:file "treematch" :depends-on ("package"))
			 (:file "match-stack" :depends-on ("package"))
			 (:file "pattern-tree" :depends-on ("package"))))
	       (:module mailbox
			:components
			((:file "defpackage")))
	       (:module mfstor
			:components
			((:file "defpackage")
			 (:file "multi-format-stor" :depends-on ("defpackage"))
			 (:file "document" :depends-on ("defpackage" "multi-format-stor")))))
  :depends-on (:sb-posix :elephant :cxml :uuid :cl-json :xmls :uffi :bordeaux-threads :sb-bsd-sockets :s-sysdeps :cffi))

;;;; TODO: Setup automated tests using 5am!