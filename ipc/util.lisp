(in-package :cl-lwipc)

(defun get-errno ()
  (sb-alien:get-errno))
