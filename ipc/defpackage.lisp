
(in-package :common-lisp-user)

(defpackage :cl-lwipc
  (:nicknames :ipc)
  (:use :cl :uffi)
  (:documentation "A lightweight interface to system v and posix ipc")
  (:export
   #:ipc-stat #:ipc-creat #:ipc-excl #:ipc-rmid #:ipc-nowai #:ipc-set
   #:errno-eacess #:errno-enoent
   #:shm-r #:shm-w
   #:getval #:setval #:getpid #:getncnt #:getzcnt
   #:s-irusr #:s-iwusr
;   #:key-t #:size-t #:pid-t #:time-t #:uid-t #:gid-t #:mode-t
   #:ipc-perm
   #:msgq-msg #:msgq-msg-pointer #:msqid-ds
   #:msg-get #:msg-ctl #:msg-send #:msg-rcv
   #:sem #:semid-ds #:semun #:sembuf
   #:sem-get #:sem-ctl1 #:sem-ctl2 #:sem-ctl3 #:sem-op
   #:shmid-ds
   #:shm-get #:shm-ctl #:shm-attach
   #:get-errno
   #:shmem #:shmem-exists #:shmem-aref #:make-shmem
   #:msgq #:make-msgq #:msgq-exists
   #:semaphore-set #:make-semaphore-set #:semaphore-set-exists
   #:semaphore #:make-semaphore #:semaphore-exists
   ))
