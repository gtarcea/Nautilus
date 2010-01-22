(in-package :cl-lwipc)

;; TODO: Look up these types in the system
(uffi:def-foreign-type :key-t :unsigned-int)
(uffi:def-foreign-type :size-t :unsigned-int)
(uffi:def-foreign-type :pid-t :int)
(uffi:def-foreign-type :time-t :long)
(uffi:def-foreign-type :uid-t :int)
(uffi:def-foreign-type :gid-t :int)
(uffi:def-foreign-type :mode-t :short)
(uffi:def-foreign-type :off-t :long)

(uffi:def-struct ipc-perm
    (uid :uid-t)
  (gid :gid-t)
  (cuid :uid-t)
  (cgid :gid-t)
  (mode :mode-t)
  (seq :unsigned-long)
  (key :key-t))

;;;; System V message queues

(defconstant +msgq-message-length+ 256)

(uffi:def-struct msgq-msg (msg-type :long) (msg (:array :char #.+msgq-message-length+)))
(uffi:def-foreign-type msgq-msg-pointer (* msgq-msg))
(uffi:def-struct msqid-ds
    (msg-perm ipc-perm)
  (msg-first :pointer-void)
  (msg-last :pointer-void)
  (msg-cbytes :unsigned-long)
  (msg-qnum :unsigned-long)
  (msg-qbytes :unsigned-long)
  (msg-lspid :pid-t)
  (msg-lrpid :pid-t)
  (msg-stime :time-t)
  (msg-rtime :time-t)
  (msg-ctime :time-t))

(uffi:def-function ("msgget" msg-get)
    ((key :key-t)
     (flag :int))
  :module "cl-lwipc"
  :returning :int)

(uffi:def-function ("msgctl" msg-ctl)
    ((msgqid :int)
     (command :int)
     (msgqid-ds (* msqid-ds)))
  :module "cl-lwipc"
  :returning :int)

(uffi:def-function ("msgsnd" msg-send)
    ((msqid :int)
     (msg msgq-msg-pointer)
     (nbytes :size-t)
     (flag :int))
  :module "cl-lwipc"
  :returning :int)

(uffi:def-function ("msgrcv" msg-rcv)
    ((msgqid :int)
     (msg msgq-msg-pointer)
     (nbytes :size-t)
     (type :long)
     (flag :int))
  :module "cl-lwipc"
  :returning :int)

;;;; System V Semaphores

(uffi:def-array-pointer unsigned-short-array-pointer :unsigned-short)

(uffi:def-struct sem
    (semval :unsigned-short)
  (sempid :pid-t)
  (semncnt :unsigned-short)
  (semzcnt :unsigned-short))

(uffi:def-struct semid-ds
    (sem-perm ipc-perm)
  (sem-base (* sem))
  (sem-nsems :unsigned-short)
  (sem-otime :time-t)
  (sem-ctime :time-t))

;; sem-ctl take a union, but UFFI is treating these as pointers
;; and the deref is throwing things off. Instead we define 3
;; different versions of sem-ctl, one for each parameter type
(uffi:def-union semun
    (val :int)
  (semid-ds (* semid-ds))
  (arg-array unsigned-short-array-pointer))

(uffi:def-struct sembuf
    (sem-num :unsigned-short)
  (sem-op :short)
  (sem-flg :short))

(uffi:def-function ("semget" sem-get)
    ((key :key-t)
     (nsems :int)
     (flag :int))
  :module "cl-lwipc"
  :returning :int)

(uffi:def-function ("semctl" sem-ctl1)
    ((semid :int)
     (semnum :int)
     (cmd :int)
     (arg :int))
  :module "cl-lwipc"
  :returning :int)

(uffi:def-function ("semctl" sem-ctl2)
    ((semid :int)
     (semnum :int)
     (cmd :int)
     (arg (* semid-ds)))
  :module "cl-lwipc"
  :returning :int)

(uffi:def-function ("semctl" sem-ctl3)
    ((semid :int)
     (semnum :int)
     (cmd :int)
     (arg (* :unsigned-short)))
  :module "cl-lwipc"
  :returning :int)

(uffi:def-function ("semop" sem-op)
   ((semid :int)
    (semoparray (* sembuf))
    (nops :size-t))
  :module "cl-lwipc"
  :returning :int)

;;;; Posix Semaphores

(uffi:def-foreign-type :sem-t :int)

(uffi:def-function ("sem_open" sem-open1)
    ((name :cstring)
     (flags :int))
  :module "cl-lwipc"
  :returning (* :int))

(uffi:def-function ("sem_open" sem-open2)
    ((name :cstring)
     (flags :int)
     (mode :mode-t)
     (value :unsigned-int))
  :module "cl-lwipc"
  :returning (* :int))

(uffi:def-function ("sem_close" sem-close)
    ((sem (* :sem-t)))
  :module "cl-lwipc"
  :returning :int)

(uffi:def-function ("sem_destroy" sem-destroy)
    ((sem (* :sem-t)))
  :module "cl-lwipc"
  :returning :int)

(uffi:def-function ("sem_getvalue" sem-getvalue)
    ((sem (* :sem-t))
     (semval (* :int)))
  :module "cl-lwipc"
  :returning :int)

(uffi:def-function ("sem_unlink" sem-unlink)
    ((name :cstring))
  :module "cl-lwipc"
  :returning :int)

(uffi:def-function ("sem_init" sem-init)
    ((sem (* :sem-t))
     (pshared :int)
     (value :unsigned-int))
  :module "cl-lwipc"
  :returning :int)

(uffi:def-function ("sem_trywait" sem-trywait)
    ((sem (* :sem-t)))
  :module "cl-lwipc"
  :returning :int)

(uffi:def-function ("sem_wait" sem-wait)
    ((sem (* :sem-t)))
  :module "cl-lwipc"
  :returning :int)

;;;; Posix Mutexes (using glue code)


;;;; Posix Condition Variables (using glue code)

;;;; System V Shared Memory

(uffi:def-struct shmid-ds
    (shm-perm ipc-perm)
  (shm-amp :pointer-void)
  (shm-segsz :int)
  (shm-lkcnt :unsigned-short)
  (shm-lpid :pid-t)
  (shm-cpid :pid-t)
  (shm-nattach :unsigned-long)
  (shm-cnattach :unsigned-long)
  (shm-atime :time-t)
  (shm-dtime :time-t)
  (shm-ctime :time-t))

(uffi:def-function ("shmget" shm-get)
    ((key :key-t)
     (size :int)
     (flag :int))
  :module "cl-lwipc"
  :returning :int)

(uffi:def-function ("shmctl" shm-ctl)
    ((shmid :int)
     (cmd :int)
     (shmid-ds (* shmid-ds)))
  :module "cl-lwipc"
  :returning :int)

(uffi:def-function ("shmat" shm-attach)
    ((shmid :int)
     (addr :pointer-void) 
     (flag :int))
  :module "cl-lwipc"
  :returning (* :unsigned-char))

;;;; Record locking

(uffi:def-struct flock
    (l-type :short)
  (l-start :off-t)
  (l-whence :short)
  (l-len :off-t)
  (l-pid :pid-t))

(uffi:def-function ("fcntl" fcntl)
    ((filedesc :int)
     (cmd :int)
     (flock (* flock)))
  :module "cl-lwipc"
  :returning :int)
    
