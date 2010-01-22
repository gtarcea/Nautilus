(in-package :mailbox)

(defclass shared-memory-segment ()
  ((shmem-id :initarg :shmem-id
	     :accessor shmem-id
	     :documentation "The sys v shared memory id")
   (shmem-address :initarg :shmem-address
		  :accessor shmem-address
		  :documentation "The shared memory address"))
  (:documentation "A class describing a sys v shared memory segment"))

(defclass message-queue ()
  ((msgq-id :initarg :msgq-id
	    :accessor msgq-id
	    :documentation "The sys v msgq id"))
  (:documentation "A class describing a sys v msg queue"))

(defclass semaphore ()
  ((sem-id :initarg :sem-id
	   :accessor sem-id
	   :documentation "The sys v semaphore id"))
  (:documentation "A class describing a sys v semaphore"))

(defclass postoffice-ipc (shared-memory-segment message-queue)
  ((semaphore-list :initform (make-array 0 :fill-pointer t :adjustable t :initial-element nil)
		   :accessor semaphore-list
		   :documentation "A list of semaphores used to synchronize access to the mailboxes"))
  (:documentation "The IPC elements used to create a postoffice"))

(defclass postoffice (postoffice-ipc)
  ((num-mailboxes :initarg :num-mailboxes
		  :accessor num-mailboxes
		  :documentation "The maximum number of mailboxes in the system")
   (name :initarg :name
	 :accessor postoffice-name
	 :documentation "The name of the mailbox - must be unique")
   (id :initarg :id
       :accessor postoffice-id
       :documentation "The id used to create the ipc elements"))
  (:documentation "The postoffice class - a postoffice is made up of a number of mailboxes"))

(defclass mailbox ()
  ((mailbox-id :initarg :mailbox-id
	       :accessor mailbox-id
	       :documentation "")
   (mailbox-name :initarg :mailbox-name
		 :accessor mailbox-name
		 :documentation ""))
  (:documentation ""))

(defclass mailmessage ()
  ((message :initarg :message
	    :accessor :mail-message
	    :documentation ""))
  (:documentation ""))

(defun open-mailbox (mailbox-id mailbox-name)
  (make-instance 'mailbox :mailbox-id mailbox-id :mailbox-name mailbox-name))

;;
;; The header for the shared memory segment is made up of a bunch of ints
;; eg, the structure is:
;; (defstruct shmem-header
;;    (postoffice-id :int)
;;    (num-mailboxes :int)
;;    (active-mailboxes :int)
;;    (num-messages-per-mailbox :int)
;;    (state :int)
;;    (list-of-mailboxes :array :int))
;;

;; Size in bytes
(defconstant +shmem-header-size+ (* 4 5)) ; Doesn't include the array list-of-mailboxes

;;
;; Each mailbox is a set size in length + its header. A mailbox header looks as follows
;; (defstruct mailbox-header
;;    (mailbox-id :int)
;;    (num-mailbox-messages :int)
;;    (message-size :int)
;;    (list-of-messages :array :int))
;;

;; Size in bytes
(defconstant +mailbox-header-size+ (* 4 3)) ; Does include the array list-of-mailboxes

;; Define shared memory structures

(uffi:def-struct shmem-header
    (postoffice-id :unsigned-int)
  (num-mailboxes :unsigned-int)
  (active-mailboxes :unsigned-int)
  (num-messages-per-mailbox :unsigned-int)
  (state :unsigned-int))

(uffi:def-struct mailbox-header
    (mailbox-id :unsigned-int)
  (num-mailbox-messages :unsigned-int)
  (message-size :unsigned-int)
  (bytes :byte))


(defun compute-shmem-size (num-mailboxes message-size num-messages-per-mailbox)
  "Returns size in bytes of shared memory segment to allocate"
  (let ((mailbox-size (* +mailbox-header-size+ message-size num-messages-per-mailbox))
	(shmem-header (+ +shmem-header-size+ (* 4 num-mailboxes))))
    (+ shmem-header (* mailbox-size num-mailboxes))))

(defun create-new-postoffice (postoffice-name postoffice-id num-mailboxes num-messages-per-mailbox message-size)
  "Creates a new postoffice. This should only be called once. If a postoffice already exists then raise error."
  (if (shmem-exists postoffice-id)
      (error "Postoffice already exists")) ; Should do a define condition here and then raise it.
  (let ((postoffice (make-instance 'postoffice :id postoffice-id :name postoffice-name :num-mailboxes num-mailboxes))
	(shmem-size (compute-shmem-size num-mailboxes num-messages-per-mailbox message-size)))
    (multiple-value-bind (shmemid addr shmem-created) (open-shared-memory-segment postoffice-id shmem-size)
      (when (not (null addr))
	(if shmem-created
	    ;; We need to initialize the rest of the data structures
	    (initialize-postoffice addr postoffice-id num-mailboxes num-messages-per-mailbox message-size))
	(do-stuff-here)))))

(defun create-existing-postoffice (postoffice-name postoffice-id)
  "Attaches to an existing postoffice"
  nil)

(defun get-mailbox (mailbox-address)
  nil)

(defun initialize-postoffice (addr postoffice-id num-mailboxes num-messages-per-mailbox message-size)
  "Creates and initializes the other IPC elements that make up a postoffice and sets up all the
header and other bookkeeping items in the shared memory segment"
  (initialize-shmem addr postoffice-id num-mailboxes num-messages-per-mailbox message-size)
  (create-message-queue postoffice-id num-mailboxes)
  (create-semaphores postoffice-id num-mailboxes))

(defun initialize-shmem (addr postoffice-id num-mailboxes num-messages-per-mailbox message-size)
  "Initializes the shmem, sets up the various headers and lays out the memory.
TODO: Need to be careful to synchronize access to make sure no one else is doing anything here.
Since this is System V there is a chance for a race condition. I'll have to figure out a method to
handle this (probably a procedural method)"
  (initialize-shmem-header addr postoffice-id num-mailboxes num-messages-per-mailbox))

(defun initialize-shmem-header (addr postoffice-id num-mailboxes num-messages-per-mailbox)
  (uffi:with-cast-pointer (shmem addr 'shmem-header)
    (setf (uffi:get-slot-value shmem 'shmem-header 'postoffice-id) postoffice-id)
    (setf (uffi:get-slot-value shmem 'shmem-header 'num-mailboxes) num-mailboxes)
    (setf (uffi:get-slot-value shmem 'shmem-header 'num-messages-per-mailbox) num-messages-per-mailbox)
    (setf (uffi:get-slot-value shmem 'shmem-header 'active-mailboxes) 0)
    (setf (uffi:get-slot-value shmem 'shmem-header 'state) 0)))

(defun write-to-mailbox (addr mailbox-index data data-size)
  "Writes to the mailbox at index."
  nil)

(defun create-message-queue (postoffice-id num-mailboxes)
  nil)

(defun create-semaphores (postoffice-id num-mailboxes)
  )

;; TODO: Put this somewhere
(defun get-errno ()
  (sb-alien:get-errno))
   
   
