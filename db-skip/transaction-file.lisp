;;;; ==============================================================================
;;;; This module implements a simple transaction file for writing transactions to.
;;;; It uses cl-store to write objects to a transaction log. If the log has an
;;;; invalid entry in it cl-store will raise an exception. We catch this exception
;;;; and position forward in the transaction log to the next cl-store entry. This
;;;; entry may also be corrupted. If it is, we continue the process on until we
;;;; a valid transaction or reach the end of file.
;;;; ==============================================================================

;(in-package :skipdb)

(defstruct transaction
  start  ;; Marks start of an object transaction
  txnid  ;; The txnid being written
  obj    ;; The object being stored in the transaction log
  end)   ;; Marks the end of a transaction

(defun open-txn-log ()
  ;; replace hard coded path with *skipdb-dir*
  (open "/tmp/txn" :direction :io :element-type '(unsigned-byte 8)
        :if-exists :append :if-does-not-exist :create))

(defun find-next-transaction (stream)
  (let ((file-length (file-length stream))
        (byte nil)
        (cl-s (make-array 4))
        (length 0))
    (loop until (= (file-position stream) file-length)
         ;; Use page 127 as an example
         (setf byte (read-byte stream))
         (if (= byte xxxx) ; 'C'
             (progn
               (setf (aref cl-s 0) byte)
               (setf length 1))
             ()))))

(defun apply-transaction (transaction)
  nil)

(defun replay-transactions (stream)
  (file-position stream :start)
  (let ((file-length (file-length stream)))
    (loop until (= (file-position stream) file-length)
         (let ((txn (handler-case (cl-store:restore stream)
                      (:no-error (obj) obj)
                      (t () nil))))
           (if (null txn)
               (find-next-transaction stream)
               (apply-transaction txn))))))