; (in-package :mailbox)

(require 'bordeaux-threads)
(require 'sb-bsd-sockets)
(require 's-sysdeps)

(defclass postoffice ()
  ((socket :initarg :socket
	   :accessor postoffice-socket
	   :documentation "The socket the postoffice server is listening on")
   (mailbox-lock :initarg :mailbox-lock
		 :accessor postoffice-mailbox-lock
		 :documentation "The global lock the postoffice server uses to synchronize access to the mailboxes")
   (mailboxes :initform (make-hash-table)
	      :accessor postoffice-mailboxes
	      :documentation "The mailboxes and slots per mailbox that the postoffice is tracking"))
  (:documentation "Represents a postoffice server and its resources"))

(defun make-postoffice (lock-name)
  (make-instance 'postoffice :mailbox-lock (bt:make-lock lock-name)))

(defun start-server (postoffice port func)
  (let* ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
	 (handler-fn (lambda (fd)
		       (declare (ignore fd))
		       (let ((sock-stream (sb-bsd-sockets:socket-make-stream
				      (sb-bsd-sockets:socket-accept socket)
				      :element-type 'character
				      :input t
				      :output t
				      :buffering :none)))
			 (funcall func postoffice sock-stream)))))
    (setf (postoffice-socket postoffice) socket)
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket #(0 0 0 0) port)
    (sb-bsd-sockets:socket-listen socket 15)
    (sb-sys:add-fd-handler (sb-bsd-sockets:socket-file-descriptor socket)
			   :input handler-fn)))

;(defvar *mailboxes* nil)
;(defvar *mailbox-lock* nil)
;(defvar *postoffice-socket* nil)

(defun postoffice-server-init ()
  "Initializes the postoffice server. Tasks include creating the thread locks to
synchronize access to shared variables, and initializing the mailboxes hash and
arrays"
  (format t "postoffice-server-init")
;  (setf *mailboxes* (make-hash-table))
;  (setf (gethash 1 *mailboxes*) (make-array 32 :element-type 'bit))
;  (setf *mailbox-lock* (bt:make-lock "mailbox-lock")))
  (let ((postoffice-server (make-postoffice "mailbox-lock")))
    ; Create a default mailbox
    (setf (gethash 1 (postoffice-mailboxes postoffice-server)) (make-array 32 :element-type 'bit))
    postoffice-server))


(defun postoffice-call-handler (postoffice stream)
  (format t "postoffice-call-handler~%")
  (let ((sym (intern (string-upcase (read-line stream))))
	(mlock (postoffice-mailbox-lock postoffice)))
    ;; TODO: Should probably wrap this in a unwind-protect to handle any errors
    (format t "sym is '~A'~%" sym)
    (bt:with-lock-held (mlock)
      (format t "past lock~%")
      (case sym
	(get-mailbox (do-get-mailbox postoffice stream))
	(return-mailbox (do-return-mailbox postoffice stream))
	(create-mailbox (do-create-mailbox postoffice stream))
	(shutdown-postoffice-server (do-shutdown-postoffice-server postoffice)
				    (return-from postoffice-call-handler))
	(t (format t "Unknown call ~S~%" sym)))))
  (finish-output stream)
  (close stream)
  (values))

(defun do-shutdown-postoffice-server (postoffice)
  "Shutdowns the postoffice, cleans up the sockets, stops all fd handlers."
  (values))

(defun do-get-mailbox (postoffice stream)
  "Returns a slot in the mailbox, return the slot if available, or -1 if mailbox
is full or if mailbox doesn't exist"
  (format t "do-get-mailbox~%")
  (let ((cur-mailbox-ids (gethash (parse-integer (read-line stream)) (postoffice-mailboxes postoffice))))
    (when cur-mailbox-ids
      (format t "looking for a mailbox~%")
      (dotimes (i 32 i)
	(when (= 0 (bit cur-mailbox-ids i))
	  (format t "found a mailbox ~D~%" i)
	  (setf (bit cur-mailbox-ids i) 1)
	  (format stream "~D~%" i)
	  (return-from do-get-mailbox))))
    (format t "Didn't find a mailbox")
    (format stream "-1~%")))

(defun do-return-mailbox (postoffice stream)
  "Returns a used mailbox slot back to the mailbox. Returns slot on success, -1 on failure"
  (format t "do-return-mailbox")
  (let ((mailbox-id (parse-integer (read-line stream)))
	(id-to-return (parse-integer (read-line stream))))
    (when (or (< id-to-return 0) (> id-to-return 31))
      (format stream "-1~%")
      (return-from do-return-mailbox))
    (multiple-value-bind (value found) (gethash mailbox-id (postoffice-mailboxes postoffice))
      (cond
	((not found) (format stream "-1~%"))
	(t (setf (bit value id-to-return) 0))))))

(defun do-create-mailbox (postoffice stream)
  (format t "do-create-mailbox")
  (let ((mailbox-id (parse-integer (read-line stream)))
	(rv -1))
    (multiple-value-bind (value found) (gethash mailbox-id (postoffice-mailboxes postoffice))
      (declare (ignore value))
      (when (not found)
	; Create new mailbox
	(setf (gethash mailbox-id (postoffice-mailboxes postoffice)) (make-array 32 :element-type 'bit))
	(setf rv 1)))
    (format stream "~D~%" rv)))

(defun start-postoffice-server (port)
  (let ((postoffice (postoffice-server-init)))
    (start-server postoffice port #'postoffice-call-handler)))

(defun test-call-handler (stream)
  (let ((buf (read-line stream)))
    (format t "~A~%" buf)))

(defun test-init-handler ()
  (format t "test-init-handler called ~%"))

(defun start-test-server (port)
  (start-server2 port #'test-call-handler #'test-init-handler))

(defun s-start-test-server (port)
  (s-sysdeps:start-standard-server :port port :name "test-server" :connection-handler #'test-call-handler))


			 