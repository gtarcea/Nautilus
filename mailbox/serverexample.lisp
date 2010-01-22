;; Set up local listener serverA
(defun start-server (port)
    (s-sysdeps:start-standard-server
        :port port
        :name "postoffice-server"
        :connection-handler #'(lambda (stream)
                                (let ((buf (read-line stream)))
                                    (format t "~A~%" buf)))))

(defun start-server2 (port)
    (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)
