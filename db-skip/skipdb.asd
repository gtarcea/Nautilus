(in-package :cl-user)

(defpackage :skipdb-asd
  (:use :cl :asdf))

(in-package :skipdb-asd)

(defsystem :skipdb
  :name "skipdb"
  :author "V. Glenn Tarcea <gtarcea@umich.edu> and Leslie P. Polzer <leslie.polzer@gmx.net>"
  :version "0.1"
  :maintainer "V. Glenn Tarcea <gtarcea@umich.edu and Leslie P. Polzer <leslie.polzer@gmx.net>"
  :licence "BSD"
  :description "Memory based skiplist and database"
  :components ((:file "defpackage")
               (:file "skiplist" :depends-on ("defpackage"))
               (:file "dbdir" :depends-on ("defpackage"))
               (:file "slefile" :depends-on ("defpackage" "dbdir" "skiplist")))
  :depends-on (:cl-fad :cl-store))
