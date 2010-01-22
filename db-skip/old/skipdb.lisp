(in-package :skipdb)

(defclass db-record ()
  ((level :accessor db-level
		  :documentation "The level in the skip list")
   (pid :accessor pid
		:documentation "Process tracking")
   (tid :accessor tid
		:documentation "Thread tracking")
   (key :accessor rec-key
		:documentation "The records key value")
   (data :accessor rec-data
		 :documentation "The records data")
   (dirty? :accessor dirty?
		   :documentation "Has the record changed?"))
  (:documentation "A record entry in the database"))

(defclass skipdb ()
  ((dbstream :accessor db-stream
			 :documentation "The I/O stream to the backing store")
   (state :db-state
		  :initarg :db-state
		  :documentation "The state of the database, eg :open, :closed, :recovering, :fsck")
   (path :initarg db-path
		 :accessor :db-path
		 :documentation "The path to the backing store")))

(defgeneric db-close (db &key abort)
  (:documentation "Closes the db"))

(defgeneric db-open (db &key recover)
  (:documentation "Opens the database, recovers the database if the database needs recovery
and a recovery isn't already in progress"))

(defgeneric db-get (db key)
  (:documentation "Retrieves the record with the given key from the database"))

(defgeneric db-put (db key data)
  (:documentation "Inserts a new record into the database"))

(defgeneric db-update (db dbrec)
  (:documentation "Updates an existing record in the database"))

(defgeneric db-open-cursor (db &optional &key key)
  (:documentation "Opens a cursor on database, optionally starting at key"))

(defgeneric map-db (db func &optional cursor)
  (:documentation "Performs a map function on the database, optionally using an existing
cursor, otherwise it opens a new one"))

(defgeneric db-close-cursor (db cursor)
  (:documentation "Closes an existing cursor"))

(defmacro with-cursor (cursor &body body)
  "Not sure...."
  (format t "fill in"))

(defgeneric db-open-txn (db)
  (:documentation "Creates a new transaction"))

(defmacro with-txn (txn &body body)
  "Not sure...."
  (format t "fill in"))