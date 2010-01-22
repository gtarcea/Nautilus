;;;; ==========================================================================
;;;; The journaled-file class implements a transactional file. Each write to
;;;; a journaled file is first written to a transaction log, and then is
;;;; written to the journaled file. When a journaled file is opened the log
;;;; is consulted and any writes that were commited to the transaction log
;;;; that are not in the journaled file are then completed and the transaction
;;;; log is cleared out after that. The transaction log is also periodically
;;;; flushed to keep the log file from growing out of bounds. You can control
;;;; the frequency of the flushes.
;;;; ==========================================================================

; (in-package :skipdb)
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defclass journaled-file-stream ()
  ((jfile-dir :initarg :jfile-dir
              :accessor jfile-dir
              :documentation "The path and name of the journaled file")
   (jfile-name :initarg :jfile-name
               :accessor jfile-name
               :documentation "The file name without path of the journaled file")
   (jfile-stream :initarg :jfile-stream
                 :accessor jfile-stream
                 :documentation "The stream for the journaled file")
   (jfile-log-dir :initarg :jfile-log-dir
                  :accessor jfile-log-dir
                  :documentation "The directory to store log files in. If not specified
then it defaults to the same directory where the journaled file is")
   (jfile-log-stream :initarg :jlog-stream
                     :accessor jfile-log-stream
                     :documentation "The stream for the log file")
   (jfile-log-size :initarg :jfile-log-size
                   :accessor jfile-log-size
                   :documentation "The size to preallocate for the log file")
   (jfile-buf :initform (make-array 4096 :element-type '(unsigned-byte 8) :fill-pointer t :adjustable t)
              :accessor jfile-buf
              :documentation "A buffer to read log file entries into. We store it here to save on
allocating on every call to read/write the log file"))
  (:documentation "Class definition for journaled files"))

(defgeneric open-journal (what &rest args)
  (:documentation "Opens a journaled file"))

(defgeneric close-journal (jfile path)
  (:documentation "Closes a journaled file"))

(defgeneric delete-journal (jfile)
  (:documentation "Deletes a journaled file"))

(defmacro with-journaled-file (jfile &rest body)
  (format t "Do something..."))

(defconstant +min-log-size+ (* 64 1024 1024))
(defconstant +transaction-start+ #16rEE12)
(defconstant +transaction-end+ #16rEE13)

(defun make-journal (&key (file #P"./") (log-path nil) (log-size (* 64 1024 1024)))
  (let* ((file-dir (directory-namestring file))
         (file-name (file-namestring file))
         (log-dir (if log-path log-path file-dir)))
    (make-instance 'journaled-file-stream
                   :jfile-log-size (if (< log-size +min-log-size+) +min-log-size+ log-size)
                   :jfile-dir file-dir
                   :jfile-name file-name
                   :jfile-log-dir log-dir)))

(defun open-file (file-path &rest args)
  (handler-case (apply #'open file-path args)
    (file-error () nil)))

;;
;; Notes: if the log-stream and file-stream needed to be the same type, then the type for the file stream is:
;; (stream-element-type log-stream)
(defmethod open-journal ((jfile journaled-file-stream) &rest args)
;  (let ((file-stream (handler-case (apply #'open (merge-pathnames (jfile-name jfile) (jfile-dir jfile)) args)
;                       (file-error () nil)))
;        (log-stream (handler-case (open (merge-pathnames (concatenate 'string (jfile-name jfile) ".log") (jfile-log-dir jfile))
;                                        :direction :io :if-does-not-exist :create :if-exists :append :element-type '(unsigned-byte 8))
 ;                     (file-error () nil))))
  (let ((file-stream (apply #'open-file (merge-pathnames (jfile-name jfile) (jfile-dir jfile)) args))
        (log-stream (open-file (merge-pathnames (concatenate 'string (jfile-name jfile) ".log") (jfile-log-dir jfile))
                                                :direction :io :if-does-not-exist :create :if-exists :append
                                                ;; Use :element-type :default in SBCL to open a bivalent stream
                                                :element-type '(unsigned-byte 8))))
    (cond
     ((and file-stream log-stream)
      (setf (jfile-log-stream jfile) log-stream)
      (setf (jfile-stream jfile) file-stream)
      (apply-commited-transactions jfile)
      jfile)
     (t
      (and file-stream (close file-stream))
      (and log-stream (close log-stream))
      nil))))

(defun print-jfile (jfile)
  (format t "jfile-dir = ~A~% jfile-name = ~A~% jfile-log-dir = ~A~% jfile-log-size = ~D~%"
          (jfile-dir jfile) (jfile-name jfile) (jfile-log-dir jfile) (jfile-log-size jfile)))

(defmethod open-journal ((file string) &rest args)
  (let* ((log-path (directory-namestring file))
         (jfile (make-journal :file file :log-path log-path)))
    (apply #'open-journal jfile args)))

(defun apply-commited-transactions (jfile)
  (when nil
    (let ((file-pos 0)
          (buf-size 0)
          (start-pos 0)
          (end-pos 4)
          (txn-pos 0))
      (loop for position = (read-sequence (jfile-buf jfile) :start start-pos :end end-pos)))))
         ;; Read into confirming its a transaction start
         ;; read size of buffer
         ;; read completion
         ;; check to see if applied (assuming we read completion)
         ;; if not, apply journal file
         ;; )
        
;(defun apply-commited-transactions (jfile)
;  (let ((file-pos 0)
;        (transaction-pos 0))
;    (file-position (jfile-log-stream jfile) 0)
;    (loop for bytes = (read-sequence

;(defun last-commited-transaction-position (jfile)
;  (let ((last-good-position 0))))

;(defun truncate-to-completed-transactions (jfile)
;  (let ((last-commit-pos (last-commited-transaction-position jfile)))
;    (file-position (jfile-log-stream jfile) (file-length jfile))
;    (when (/= last-commit-pos file-position)
;      (sb-posix:ftruncate (sb-sys:fd-stream-fd (jfile-log-stream jfile) last-commit-position))
;      (file-position (jfile-log-stream jfile) (file-length s)))))

#|
(let ((buf (make-array 4096 :element-type (stream-element-type input-stream)))
  (loop for pos = (read-sequence input-stream)
        while (plusp pos)
        do (write-sequence buf output-stream :end pos))))
|#
