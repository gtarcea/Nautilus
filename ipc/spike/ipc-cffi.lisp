;;;;
;;;; This package defines an interface to System V IPC, including:
;;;;  Shared Memory
;;;;  Message Queues
;;;;  Semaphores
;;;;

(in-package :sysv-ipc)

;;;; Define some types used by System V IPC

(defctype :key-t :unsigned-int)
(defctype :size-t :unsigned-int)
(defctype :pid-t :int)
(defctype :time-t :long)
(defctype :uid-t :int)
(defctype :gid-t :int)
(defctype :mode-t :short)

(define-foreign-type char-array (&rest dimensions)
  `(:array :char ,@dimensions))

;;;; System V Permissions used by all System V IPC

(defcstruct ipc-perm
  (uid :uid-t)
  (gid :gid-t)
  (cuid :uid-t)
  (cgid :gid-t)
  (mode :mode-t)
  (seq :unsigned-long)
  (key :key-t))

;;;; System V message queues

(defconstant +msgq-message-length+ 256)

(defcstruct msgq-msg
  (msg-type :long)
  (msg :pointer