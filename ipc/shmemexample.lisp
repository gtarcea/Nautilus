;;;;;;;;;;;;;;;;;;;;;
;; Shared memory

;; Create shared memory segment
(setf shmem (ipc::shm-get 54321 131072 (logior ipc::s-irusr ipc::s-iwusr ipc::ipc-creat ipc::ipc-excl)))

;; Attach to shared memory segment
(setf memory (ipc::shm-attach shmem nil 0))

;; How to access memory
(with-foreign-object (size :int)
   ;; FOO is a foreign function returning a :POINTER-VOID
   (let ((memory (foo size)))
      (when (mumble)
         ;; at this point we know for some reason that MEMORY points
         ;; to an array of unsigned bytes
         (with-cast-pointer (memory :unsigned-byte)
           (dotimes (i (deref-pointer size :int))
            (do-something-with
              (deref-array memory '(:array :unsigned-byte) i)))))))

example of accessing structure:
(uffi:def-struct tstruct2
         (a :unsigned-int)
       (b :unsigned-int))

(uffi:with-cast-pointer (s memory 'tstruct2)
       (uffi:get-slot-value s 'tstruct 'a)
       (setf (uffi:get-slot-value s 'tstruct 'b) 2000))

(uffi:def-struct tlist2
         (a :unsigned-int)
       (b (:array 'tstruct2 10)))

(uffi:with-cast-pointer (s memory 'tlist2)
       (uffi:deref-array (uffi:get-slot-value s 'tlist 'b) 'tstruct2 0))

;; Accessing an array element tlist2 a slot:

(uffi:with-cast-pointer (s memory 'tlist2)
       (uffi:get-slot-value (uffi:deref-array (uffi:get-slot-value s 'tlist 'b) 'tstruct2 0) 'tstruct2 'a))

;; Setting the a slot in the tlist2 (array item)
(uffi:with-cast-pointer (s memory 'tlist2)
       (setf (uffi:get-slot-value (uffi:deref-array (uffi:get-slot-value s 'tlist 'b) 'tstruct2 0) 'tstruct2 'a) 10))

;; shm-ctl
(setf shmid-ds-ptr (uffi:allocate-foreign-object 'ipc::shmid-ds))
(shm-ctl shmemid ipc::ipc-stat shmid-ds-ptr)

;;; SHMEM Pointers to access memory areas
;; Look at first item in shmem-header, which is an int
;; Mem is a pointer to shared memory created with shm-attach
;; Use a combination of uffi and sb-alien to access pointers
;; Here we set 4 bytes in a value, this value corresponds to
;; the num-mailboxes in shmem-header allocated in shared memory
(setf (uffi:deref-pointer (sb-alien:sap-alien (sb-alien::sap+ (sb-alien:alien-sap mem) 4) (* unsigned-int)) :unsigned-int) 5)
;; Broken down:
(sb-alien:alien-sap mem) ; returns the sap for mem
(sb-alien::sap+ (sb-alien:alien-sap mem) 4) ; increments sap by 4 bytes
(sb-alien:sap-alien (sb-alien::sap+ (sb-alien:alien-sap mem) 4) (* unsigned-int)) ; creates a sb-alien pointing to offset
;; The uffi:deref-pointer allows us to dereference the memory to see/get at value, the setf sets the value

;;; Work on poking around in the shared memory segment. Assuming a total size for shmem of 131072 and
;;; 5 mailboxes and 3 messages per mailbox. Then the sizes are:
;;; mailbox-header = 12
;;; Total individual message size:
;;; 131072
;;;  - 20 (postoffice-header)
;;;  - 12*5 (mailbox-header size * num-mailboxes)
;;; -------
;;;  130992
;;;   divided by 15 (total number of mail-messages)
;;; --------
;;;   8730  (rounded 8732.8 to 8730 for ease)

(setf mailbox-size (+ 12 (* 3 8730))) ; 12 mailbox-header, 3 num-messages-per-mailbox
 ;; = 26202
(setf shmem-header-size 20)

;; Example function that writes the string "hello world" to shared memory
;; 
;; NOTE: BOTH OF THESE ARE WRONG IN THAT THEY DO NOT OFFSET TO THE MAILBOX
;;
(defun write-hello-world (addr mailbox message)
"Writes 'hello world' into the mailbox specified and message slot specified
assumes that the variables shmem-header-size, mailbox-size message-size exist"
  (let ((offset-start (+ shmem-header-size (* mailbox-size (1- mailbox)) (* message-size (1- message))))
        (hello-world (sb-ext:string-to-octets "hello-world")))
     (dotimes (i 11)
        (setf (uffi:deref-pointer (sb-alien:sap-alien (sb-alien::sap+ (sb-alien:alien-sap addr) i) 
                                  (* unsigned-char)) :unsigned-char) (aref hello-world i)))))

;; Using CFFI and sb-sys to write hello world
(defun write-hello-world-cffi (addr mailbox message)
       (let ((offset-start (+ shmem-header-size (* mailbox-size (1- mailbox)) (* message-size (1- message))))
         (hello-world (sb-ext:string-to-octets "hello-world")))
         (dotimes (i 11)
           (setf (cffi:mem-ref (sb-sys:sap+ addr i) :char) (aref hello-world i)))))

;;;;;;;;;;;;;;;;;

;;; Access shmem-header as array of int
(uffi:with-cast-pointer (s mem :int)
   (uffi:deref-array s :int 3))

;; To increment a pointer in SBCL
(sb-sys:sap+ pointer amount-to-increment)

;; with alien
(sb-sys:sap+ (sb-alien:alien-sap mem) amount-to-increment)

;; To use cffi and access as an array of bytes the pointer
(cffi:mem-ref (sb-sys:sap+ (sb-alien:alien-sap mem) 4) :int)
;; Set a value
(setf (cffi:mem-ref (sb-sys:sap+ (sb-alien:alien-sap mem) 4) :int) 3)
;; Set a value


;;;;;;;;;;;;;;;;;;;;;;;
;; **** GET ERRNO ****
;;;;;;;;;;;;;;;;;;;;;;;
(sb-alient:get-errno)

;;;;;;;;;;;;;;;;;;;;;;;
;; Message queues

;; Create message queue
(setf msgqid (ipc::msg-get 54333 (logior ipc::s-irusr ipc::s-iwusr ipc::ipc-creat ipc::ipc-excl)))

;; Send message on queue
(setf msgq-msg-ptr (uffi:allocate-foreign-object 'ipc::msgq-msg))
(setf (uffi:get-slot-value msgq-msg-ptr 'ipc::msgq-msg 'ipc::msg-type) 200)
(setf (uffi:deref-array (uffi:get-slot-value msgq-msg-ptr 'ipc::msgq-msg 'ipc::msg) :char 0) 1)
(setf (uffi:deref-array (uffi:get-slot-value msgq-msg-ptr 'ipc::msgq-msg 'ipc::msg) :char 1) 2)
(ipc::msg-send msgqid msgq-msg-ptr 256 ipc::ipc-nowait)

;; Get message just sent
(setf rmsgq-ptr (uffi:allocate-foreign-object 'ipc::msgq-msg))
(ipc::msg-rcv msgqid rmsg-ptr 256 0 0)
(uffi:get-slot-value rmsg-ptr 'ipc::msgq-msg 'ipc::msg-type)
(uffi:deref-array (uffi:get-slot-value msgq-msg-ptr 'ipc::msgq-msg 'ipc::msg) :char 0)
(uffi:deref-array (uffi:get-slot-value msgq-msg-ptr 'ipc::msgq-msg 'ipc::msg) :char 1)

;;;;;;;;;;;;;;;;;;;;;;;
;; Semaphores
(setf semid (ipc::sem-get 54333 1 (logior ipc::s-irusr ipc::s-iwusr ipc::ipc-creat ipc::ipc-excl)))
;; Initialize
(setf semarg (uffi:allocate-foreign-object 'ipc::semun))
;(setf (uffi:get-slot-value semarg 'ipc::semun 'ipc::val) 0)
;(ipc::sem-ctl semid 0 ipc::setval (uffi:deref-pointer semarg 'ipc::semun))
(ipc::sem-ctl1 semid 0 ipc::setval 0)
; Check value
(ipc::sem-ctl1 semid 0 ipc::getval 0)

;; Set Semaphore
(setf semop (uffi:allocate-foreign-object 'ipc::sembuf))
(setf (uffi:get-slot-value semop 'ipc::sembuf 'ipc::sem-num) 0)
(setf (uffi:get-slot-value semop 'ipc::sembuf 'ipc::sem-op) 1)
(setf (uffi:get-slot-value semop 'ipc::sembuf 'ipc::sem-flg) 0)
;; V operation - make semaphore available
(ipc::sem-op semid semop 1)

;; P operation - Busy away unless the semaphore is available
(setf (uffi:get-slot-value semop 'ipc::sembuf 'ipc::sem-op) -1)
(ipc::sem-op semid semop 1)

;; Now try P, but with no wait flag set
(setf (uffi:get-slot-value 'ipc::sembuf 'ipc::sem-flg) ipc::ipc-nowait)
(ipc::sem-op semid semop 1)

; errno should equal 35 (EAGAIN)
(sb-alien:get-errno)

