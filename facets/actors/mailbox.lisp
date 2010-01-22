(in-package :com.facets.actors)

(defclass mailbox ()
  ((lock :initform (bt:make-lock)
         :accessor mailbox-lock
         :documentation "The lock for synchronizing access to the mailbox. This is used mostly
to coordinate access to the mailbox-messages")
   (internal-messages :initform nil
                      :accessor mailbox-internal-messages
                      :documentation "Special internal messages go here with specific behaviors.
The special messages are:
    :abort - Causes the 'actor-abort' condition to be raised")
   (messages :initform ()
             :accessor mailbox-messages
             :documentation "The messages in the mailbox"))
  (:documentation "Defines the mailbox used by the different actors in a system"))

(defclass mail-message ()
  ((message :initarg :message
            :accessor mail-message
            :documentation "The message that was sent")
   (data :initarg :data
         :accessor mail-data
         :documentation "The data associated with the message"))
  (:documentation "All messages in a mailbox must be instances of mail-message"))

(defun make-mail-message (message data)
  (make-instance 'mail-message :message message :data data))

(defun create-mailbox ()
  "Creates a new mailbox"
  (make-instance 'mailbox))
   
(defgeneric add-message (mailbox message)
  (:documentation "Adds a message to a mailbox"))

(defgeneric get-first-message (mailbox)
  (:documentation "Gets the first message out of the mailbox"))

(defgeneric get-message-if (mailbox match)
  (:documentation "Gets the first message matching 'match' in the mailbox"))

(defgeneric mailbox-empty? (mailbox)
  (:documentation "Returns t if the mailbox contains to messages"))

(defgeneric purge-mailbox (mailbox)
  (:documentation "Discards all messages from a mailbox"))

(defmethod purge-mailbox ((mailbox mailbox))
  (setf (mailbox-messages mailbox) nil))

(defmethod add-message ((mailbox mailbox) (message mail-message))
  "Adds a new message to the specified mailbox"
  (if (equal :abort (mail-message message))
      (setf (mailbox-internal-messages mailbox) (append (list message) (mailbox-internal-messages mailbox)))
      (setf (mailbox-messages mailbox) (append (list message) (mailbox-messages mailbox)))))

(defun check-for-special-messages (mailbox)
  (dolist (msg (mailbox-internal-messages mailbox))
    (cond       ; use cond so we can easily add other special messages
      ((equal (mail-message msg) :abort)
       (error 'actor-abort)))))

(defmethod get-first-message ((mailbox mailbox))
  (check-for-special-messages mailbox)
  (if (not (null (mailbox-messages mailbox)))
      (let ((item (car (mailbox-messages mailbox))))
        (setf (mailbox-messages mailbox) (cdr (mailbox-messages mailbox)))
        item)
      nil))

(defmethod get-message-if ((mailbox mailbox) match-func)
  (check-for-special-messages mailbox)
  (let ((item (find-if match-func (mailbox-messages mailbox))))
    (if item
        (progn
          (setf (mailbox-messages mailbox) (remove-if match-func (mailbox-messages mailbox)))
          item)
        nil)))

(defmethod mailbox-empty? ((mailbox mailbox))
  (null (mailbox-messages mailbox)))
