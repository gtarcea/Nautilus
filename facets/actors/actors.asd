(defpackage :com.facets.actors-asd
  (:use :cl :asdf))

(defsystem actors
  :name "actors"
  :version "0.1"
  :maintainer "V. Glenn Tarcea"
  :author "V. Glenn Tarcea <gtarcea@umich.edu>"
  :description "Persistent actors built on facets and Elephant"
  :components ((:file "defpackage")
               (:file "process")
               (:file "conditions" :depends-on ("defpackage"))
               (:file "box" :depends-on ("defpackage"))
               (:file "model" :depends-on ("defpackage"))
               (:file "mailbox" :depends-on ("defpackage" "conditions"))
               (:file "actor" :depends-on ("defpackage" "process" "mailbox" "conditions")))
  :depends-on (:bordeaux-threads :uuid :usocket :cl-store))
