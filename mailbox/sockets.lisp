

(defun resolve-hostname (name)
  (cond
    ((eql name :any) #(0 0 0 0))
    ((typep name '(vector * 4)) name)
    (t (car (sb-bsd-sockets:host-ent-addresses
	     (sb-bsd-sockets:get-host-by-name name))))))

(defun open-server (&key (host :any) (port 0)
		    (reuse-address t)
		    (backlog 10)
		    (protocol :tcp))
  "Returns a socket"
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket
			     :type :stream
			     :protocol protocol)))
    (when reuse-address
      (setf (sb-bsd-sockets:sockopt-reuse-address sock) t))
    (sb-bsd-sockets:socket-bind sock (resolve-hostname host) port)
    (sb-bsd-sockets:socket-list sock backlog)
    sock))

(defmacro with-server ((name arguments) &body body)
  `(let (,name)
     (unwind-protect
	  (progn
	    (setf ,name (open-server ,@arguments))
	    ,@body)
       (when ,name (sb-bsd-sockets:socket-close ,name)))))

;; Change this to something...
(defconstant +buflen+ 256)

(defstruct server-session
  sock ; Socket returned by accept
  fd ; Raw file handle
  stream ; lisp stream
  buffer ; Pre-allocated incoming buffer
  handler ; serve-event handler
  )

(defun data-received-handler (session)
  "Reads all pending characters on a socket into the session buffer"
  (let ((buffer (sever-session-buffer session))
	(sock (server-session-sock session)))
    (do ((fin nil))
	(fin t)
      (setf (fill-pointer buffer) +buflen+)
      (multiple-value-bind (buf len raddr)
	  (sb-bsd-sockets:socket-receive sock buffer nil)
	(declare (ignore raddr))
	(if (null buf)
	    (setf fin t)
	    (setf (fill-pointer buffer) len)))
      (cond
	((= (length buffer) 0)
	 (sb-bsd-sockets:socket-close sock)
	 (sb-sys:remove-fd-handler (server-session-handler session))
	 (setf fin t))
	(t buffer)))))

(defun accept-handler (socket)
  (let* ((conn (sb-bsd-sockets:socket-accept socket))
	 (fd (sb-bsd-sockets:socket-file-descriptor conn))
	 (session (make-server-session :sock conn :fd fd
				       :stream (sb-bsd-sockets:socket-make-stream
						conn
						:input t
						:output t
						:element-type 'character
						:buffering :none)
				       :buffer (make-array +buflen+
							   :element-type 'character
							   :adjustable nil
							   :fill-pointer t)))
	 (handler (sb-sys:add-fd-handler fd
					 :input #'(lambda (fd)
						    (declare (ignore fd))
						    (data-received-handler session)))))
    (setf (sb-bsd-sockets:non-blocking-mode conn) t)
    (setf (server-session-handler session) handler)))

(defun start-server2 (port)
  (with-server (socket (:port port :reuse-address t))
    (sb-sys:with-fd-handler ((sb-bsd-sockets:socket-file-descriptor socket)
			     :input #'(lambda (fd)
					(declare (ignore fd))
					(accept-handler socket)))
      (loop
	   (sb-sys:serve-all-events)))))
  


