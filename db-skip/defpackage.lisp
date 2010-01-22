(in-package :cl-user)

(defpackage :skipdb
  (:use :cl :cl-fad :cl-store)
  (:nicknames :sl)
  (:export
   #:sl-insert #:sl-remove #:make-skiplist #:sl-mapc
   #:skiplist-height #:skiplist-head #:skiplist-bottom
   #:skipnode-data #:skipnode-key #:skipnode-up #:skipnode-down
   #:skipnode-prev #:skipnode-next
   #:set-data #:set-key :sl-mapkey :sl-update :sl-find
   ))
