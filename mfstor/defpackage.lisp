
(in-package :common-lisp-user)

(defpackage :multi-format-stor
  (:nicknames :mfstor)
  (:use :cl :elephant) ; And others... such as cl-json and xmls
  (:export
   ;; multi-format-stor.lisp exports
   #:open-stor #:close-stor

   ;; document.lisp exports
   #:document #:document-meta-meta #:document-id #:document-version
   #:create-document
   ))
