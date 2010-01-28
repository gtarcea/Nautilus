(in-package :com.facets.actors)

(defvar *me* nil)
(defvar *actors* (make-hash-table))
(defvar *actors-lock* (bt:make-lock))

(defclass actor-location ()
  ((remote? :initarg :remote
	    :accessor remote?
	    :documentation "t if the actor is remote (ie, outside of this process space")
   (host :initarg :host
	 :accessor host
	 :documentation "If the actor is remote, this is the name of the host where it is located")
   (port :initarg :port
	 :accessor port
	 :documentation "If the actor is remote, this is the port to communicate with it on")
   (socket :initarg :socket
	   :accessor socket
	   :documentation "The socket to use for remote hosts, nil if not set up")))

(defclass actor ()
  ((parent :initarg :actor-parent
	   :accessor actor-parent
	   :documentation "The process/thread that spawned this actor") ; not sure what to use this for yet...
   (children :initarg :actor-children
	     :accessor actor-children
	     :documentation "Actors linked to this one. Linked actors will have error and exit messages propogated")
   (alive :initform t
	  :documentation "True if actor is still alive, nil if the actor has been terminated. This
is checked by the various send and receive routines to ensure the actor is still alive.")
   (handlers :initform nil
	     :documentation "An assoc list of handler functions that are called at various special times in an actors life. These are -
                                 :abort - Clean up handler called on actor abort
                                 :exit  - Clean up handler called on actor exit")
   (name :initarg :actor-name
	 :accessor actor-name
	 :documentation "Each actor has a unique name")
   (mailbox :initarg :actor-mailbox
	    :accessor actor-mailbox
	    :documentation "The mailbox for the actor")
   (event-channel :initform (bt:make-condition-variable)
		  :accessor actor-event-channel
		  :documentation "The condition to wait/signal on for messages")))

(defgeneric receive-msg (actor &optional timeout)
  (:documentation "Gets the first available message available for the actor. Optionaly waits
until a message is available"))

(defun actor-alive? (actor)
  "Use the mailbox lock to synchronize access. No point in creating a second lock just for this"
  (bt:with-lock-held ((mailbox-lock actor))
    (with-slots (alive) actor
      alive)))

(defun set-dead (actor)
  "Use the mailbox lock to synchronize access. No point in creating a second lock just for this"
  (bt:with-lock-held ((mailbox-lock actor))
    (with-slots (alive) actor
      (setf alive nil))))

(defgeneric on-exit (actor on-exit-func)
  (:documentation "Sets the function to run when the actor exits"))

(defgeneric on-abort (actor on-abort-func)
  (:documentation "Sets the function to run when the actor receives a actor-abort condition"))

(defgeneric receive-msg-if (actor filter-func &optional timeout)
  (:documentation "Gets the first message that matches the filter-func. Optionaly waits
until a message is available"))

(defgeneric send-msg (actor message)
  (:documentation "Sends a message to actor"))

(defgeneric send-msg2 (actor msg data)
  (:documentation "Sends a message containing data"))

(defmethod on-exit ((actor actor) on-exit-func)
  "Appends the exit handler on to the list of handlers"
  (with-slots (handlers) actor
    (setf (getf handlers :exit-handler) on-exit-func)))

(defmethod on-abort ((actor actor) on-abort-func)
  "Appends the abort handler on to the list of handlers"
  (with-slots (handlers) actor
    (setf (getf handlers :abort-handler) on-abort-func)))
   
;; To call on-abort-func
;; (with-slots (handlers) actor
;;     (funcall (second (assoc :abort-func handlers))))
;;

(defun link-actors (actor actor-to-link)
  "Adds actor-to-link to the links for actor"
  (setf (actor-children actor) (append (list actor-to-link) (actor-children actor))))

;; (funcall (second (assoc :exit-func l)))

(defun abort-linked-actors (actor)
  (set-dead actor)
  (labels ((traverse-child-actors (actors-list)
             (when actors-list
               (traverse-child-actors (actor-children (car actors-list)))
               (abort-actor (car actors-list))
               (traverse-child-actors (cdr actors-list)))))
    (traverse-child-actors (actor-children actor))
    (send-abort-msg actor)))

(defun abort-actor (actor)
  (set-dead actor)
  (send-abort-msg actor))

(defun send-abort-msg (actor)
  (format t "aborting actor ~A~%" (actor-name actor)))
;  (send-msg2 actor :abort nil))

(defun find-actor (name)
  (bt:with-lock-held (*actors-lock*)
    (gethash name *actors*)))

(defun spawn-link (thunk parent-actor &key (name nil))
  (when (not (null name))
    ;; User specified a name, make sure its unique
    ;; TODO: Raise an exception here rather than returning nil
    (bt:with-lock-held (*actors-lock*)
      (if (not (eql nil (gethash name *actors*)))
          (return-from spawn-link nil))))
  (let ((new-actor (make-instance 'actor
                                  :actor-parent nil
                                  :actor-children nil
                                  :actor-mailbox (create-mailbox)
                                  :actor-name (if (null name)
                                                  (uuid:make-v4-uuid)
                                                  name))))
    (if (not (null parent-actor))
        (link-actors parent-actor new-actor))
    (bt:with-lock-held (*actors-lock*)
      (setf (gethash (actor-name new-actor) *actors*) new-actor))
    (bt:make-thread #'(lambda ()
                        (let ((*me* new-actor))
                          (handler-case (funcall thunk)
                            (error () (abort-linked-actors *me*))
                            (actor-abort () nil))
                          (set-dead *me*)))
                    :name (actor-name new-actor))
    new-actor))

(defun spawn (thunk &key (name nil))
  (spawn-link thunk nil :name name))

(defmethod receive-msg ((actor actor) &optional (timeout 0))
  "Receives a message"
  (bt:with-lock-held ((mailbox-lock (actor-mailbox actor)))
    (cond
      ((= timeout 0)
       ;; Never wait
       (get-first-message (actor-mailbox actor)))
      ((= timeout -1)
       ;; Wait forever
       (bt:condition-wait (actor-event-channel actor) (mailbox-lock (actor-mailbox actor)))
       (get-first-message (actor-mailbox actor)))
      (t
       ;; Wait with timeout
       (if (mailbox-empty? actor)
           (with-condition-timeout (timeout (return-from receive-msg nil))
             (bt:condition-wait (actor-event-channel actor) (mailbox-lock (actor-mailbox actor)))))
       (get-first-message (actor-mailbox actor))))))

(defmethod receive-msg-if ((actor actor) filter-func &optional (timeout 0))
  "Receives a message matching filter."
  (bt:with-lock-held ((mailbox-lock (actor-mailbox actor)))
    (cond
      ((= timeout 0)
       ;; Never wait
       (get-message-if (actor-mailbox actor) filter-func))
      ((= timeout -1)
       ;; Wait forever
       (bt:condition-wait (actor-event-channel actor) (mailbox-lock (actor-mailbox actor)))
       (get-message-if (actor-mailbox actor) filter-func))
      (t
       ;; Wait with timeout
       (if (mailbox-empty? (actor-mailbox actor))
           (with-condition-timeout (timeout (return-from receive-msg-if nil))
             (bt:condition-wait (actor-event-channel actor) (mailbox-lock (actor-mailbox actor)))))
       (get-message-if (actor-mailbox actor) filter-func)))))
  
(defmethod send-msg ((actor actor) (message mail-message))
  "Only send a message to a living actor. There is a race condition here because an
actor could be dieing while we are sending the message but not yet have set set-dead"
  (bt:with-lock-held ((mailbox-lock (actor-mailbox actor)))
    (with-slots (alive) actor
      (if (null alive)
          (return-from send-msg nil)))
    (add-message (actor-mailbox actor) message)
    (bt:condition-notify (actor-event-channel actor))))

(defmethod send-msg2 ((actor actor) msg data)
  (let ((mmsg (make-mail-message msg data)))
    (send-msg actor mmsg)))

;;
;; Example code that demonstrates creating an actor, sending a message to it
;; and it responding to the message
;;* (setf a (actors::spawn #'(lambda ()
;;                           (let ((msg (actors::receive-msg actors::*me* -1)))
;;                              (format t "msg = ~A~%" msg)))))
;;
;;* (actors::send-msg a "hello")
;;
;; Prints:
;; msg = hello
;;
