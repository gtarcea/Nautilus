(in-package :skipdb)

;; Configurable - need to provide an override.
(defparameter *skipdb-dir* (db-dir-open "/tmp") "The directory to store skiplist writes to")

;;;; ====================================================================
;;;; This module writes a skiplist entry (key and data) to a .sle file
;;;; ====================================================================

(defun write-sle-file (path data)
  (cl-store:store data path))

;; Set an :after method on set-data to automatically get updates to existing skiplist nodes and
;; write the data.
(defmethod set-data :after (new-data (skipnode skipnode))
  (multiple-value-bind (path exists?) (ensure-db-dir-file-path *skipdb-dir* (skipnode-key skipnode))
    (when exists?
      (write-sle-file path new-data))))
