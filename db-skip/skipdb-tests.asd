(in-package :cl-user)

(defpackage :skipdb-tests-asd
  (:use :cl :asdf))

(in-package :skipdb-tests-asd)

(defsystem :skipdb-tests
  :name "skipdb-tests"
  :author "V. Glenn Tarcea <gtarcea@umich.edu> and Leslie P. Polzer <leslie.polzer@gmx.net>"
  :maintainer "V. Glenn Tarcea <gtarcea@umich.edu> and Leslie P. Polzer <leslie.polzer@gmx.net>"
  :version "0.1"
  :license "BSD"
  :description "Tests for skipdb"
  :components ((:module :tests
                        :components ((:file "defpackage")
                                     (:file "dbdir-tests" :depends-on ("defpackage"))
                                     (:file "skiplist-tests" :depends-on ("defpackage"))
                                     (:file "slefile-tests" :depends-on ("defpackage")))))
  :depends-on (:skipdb :fiveam))