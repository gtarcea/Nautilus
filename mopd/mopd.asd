(defpackage :mopd-asd
    (:use :cl :asdf))

(defsystem mopd
    :name "MopD"
    :version "0.1"
    :maintainer "V. Glenn Tarcea"
    :author "V. Glenn Tarcea <gtarcea@umich.edu>"
    :description "A Meta Object Protocol for data. Handles data loading, pipelining,
cleaning and transformation"
    :components ((:file "package"))
    :depends-on ())
