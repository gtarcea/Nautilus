(in-package :skiplist)

(defclass skipnode ()
  ((key :initarg :node-key
		:accessor node-key
		:documentation "The key for the node")
   (data :initarg :node-data
		 :accessor node-data
		 :documentation "The data for the node"))
  (:documentation "A skiplist data node"))

(defclass skipnode-internal
	;; Need to add a compare function for keys, right now we rely on keys being integers...
  ((node :initarg :node-skipnode
		 :accessor node-skipnode
		 :documentation "The data and key node")
   (next :initarg :node-next
		 :accessor node-next
		 :documentation "The next node at the current level")
   (down :initarg :node-down
		 :accessor node-down
		 :documentation "The node one level down")
   (height :initarg :node-height
		   :accessor node-height
		   :documentation "The height of the current set of nodes")))

(defclass skiplist2 ()
  ((head :initarg :skiplist-head
		 :accessor skiplist-head
		 :documentation "")
   (bottom :initarg :skiplist-bottom
		   :accessor skiplist-bottom
		   :documentation "")
   (height :initarg :skiplist-height
		   :accessor skiplist-height
		   :documentation ""))
  (:documentation "The skiplist"))

(defgeneric search (sl key)
  (:documentation "Searches a skiplist returning the skipnode with a matching key or nil if no
match is found"))

(defgeneric insert (sl key data)
  (:documentation "Inserts or updates an existing skipnode. Returns the skipnode and t if node
was found nil otherwise"))

(defgeneric update (sl key data)
  (:documentation "Updates an existing skipnode, returns nil if no such node exists, otherwises
returns the skipnode"))

(defgeneric insert-unique (sl key data)
  (:documentation "Inserts a new skipnode only if one doesn't exist. Returns nil if unable to
insert, otherwise returns the new skipnode"))

;; ==============================================================================
;; ======= Utility functions used in searches, and other list operations ========
;; ==============================================================================

(defun find2 (node key)
  (if (and (not (null (node-next node))) (> key (node-key node)))
	  (find2 (node-next node) key)
	  node))

(defun find (node key)
  (if (not (null (node-down node)))
	  (let ((node2 (find2 node)))
		(find (node-down node2) key))
	  node))

;; ==============================================================================

(defmethod search ((sl skiplist2) key)
  (let* ((node (skiplist-head sl))
		 (return-node (find node)))
	(if (not (null (node-next return-node)))
		(node-skipnode (node-next return-node))
		nil)))

(defmethod insert ((sl skiplist2) key data)
  ;; *** Don't do search and then insert, this is inefficient,
  ;; *** better to do search as we go, and if we find then we can
  ;; *** update the data, otherwise can just do an insert.
  ;;
  ;; For simplicity first we do a search, and then we can optimize this
  ;; to keep the search path in an array (at 32 levels the array would be max
  ;; 32 long, so even if we made this a height 64 skiplist (store 2^64 entries)
  ;; the path array is very small.
  ;;
  ;; This code is very ugly, and needs to be "lisp-ified". At this point I'm not
  ;; even sure the loop is correct...
  ;;
  (let ((node (search sl key)))
	(if node
		(return-from insert node)
		(let ((height (random-level))
			  (node nil)
			  (last nil))
		  (when (> height (skiplist-height sl))
			  (setf (skiplist-head sl) (make-node nil nil (skiplist-head sl)))
			  (incf (skiplist-height sl)))
		  (setf node (skiplist-head sl))
		  (loop for x downfrom (- (skiplist-height sl) 1)
			   until (null node) do
			   (setf node (find2 node key))
			   (if (<= i height)
				   (if (or (null (node-next node)) (not (= (node-key (node-next node)))))
					   (setf (node-next node) (make-node key (node-next node) nil)))
				   (if (not (null save))
					   (setf (node-down save) (node-next node)))
				   (setf save (node-next node)))
			   (setf node (node-down node)))))))
			   
		  
				
	

(defmethod insert-unique ((sl skiplist2) key data)
  )

;; ===================================

;;; TODO:
;;; index protocol for cursor
;;; range queries

;;; Pseudo-random number generator from FreeBSD

(defparameter *random-n*
  (the fixnum (get-internal-real-time))
  "Internal status of the pseudo-random number generator.")

(defun random-seed (seed)
  "Seed the pseudo-random number generator."
  (setf *random-n* seed))

(defun sl-random ()
  "Pseudo-random number generator from FreeBSD, returns NIL 3/4 of the time."
  (declare (optimize (speed 3))
		   (type integer *random-n*))
  (logtest (logand most-positive-fixnum
				   (setf *random-n*
						 (mod (+ (the number (* *random-n* 1103515245)) 12345)
							  2147483648)))
		   (ash 1 (- (integer-length most-positive-fixnum) 1))))

(defconstant +max-level+ (the fixnum 3)
  "Maximum level of skip-list, should be enough for 2^32 elements.")

(defun random-level ()
  "Returns a random level for a new skip-list node, with SL-RANDOM p for each level."
  (declare (optimize speed))
  (do ((level 1 (1+ level)))
      ((or (= level +max-level+)
		   (sl-random)) level)
    (declare (type fixnum level))))

;;; A node is a SIMPLE-VECTOR containing KEY, VALUE and the forward pointers

(defmacro node-key (node)
  `(aref (the simple-vector ,node) 0))

(defmacro node-value (node)
  `(aref ,node 1))

(defmacro node-forward (node &optional (i 0))
  `(aref (the simple-vector ,node) (the fixnum (+ ,i 2))))

(defun make-node (key value size &key initial-element)
  (let ((node (make-array (+ 2 size) :initial-element initial-element)))
    (setf (node-key node) key
		  (node-value node) value)
    node))

(defun node-level (node)
  (declare (type simple-vector node))
  (- (length node) 2))

(defun make-header (&key initial-element)
  (make-node nil nil +max-level+ :initial-element initial-element))

(defclass skiplist ()
  ((header :initform (make-header)
		   :reader skip-list-header :type simple-vector)
   #+nil
   (finger :initform (make-header)
		   :reader skip-list-finger :type simple-vector)
   (length :initform 0 :accessor skip-list-length :type fixnum)
   (level :initform 0 :accessor skip-list-level :type fixnum))
  (:documentation "Skip-list class, access to elements is done through the header node."))

(defmethod print-object ((sl skiplist) stream)
  (print-unreadable-object (sl stream :type t :identity t)
    (format stream "length = ~A" (skip-list-length sl))))

(defmethod skip-list-empty-p ((sl skiplist))
  (= (skip-list-length sl) 0))

(defun follow-node (node key level)
  "Follow a skip-list node at level LEVEL, stopping when there is no
next node or when the next node has a larger key."
  (declare (type (or null simple-vector) node)
		   (type integer key)
		   (type integer level)
		   (optimize (speed 3)))
  (format t "in follow-node ~%")
  (do ((next (node-forward node level) (node-forward node level)))
      ((not (and next (< (node-key next) key))) node)
    (declare (type (or null simple-vector) next))
	(format t "  follow-node iteration next = ~A~%" next)
    (setf node next)))

(defmethod make-update ((sl skiplist) key)
  "Search the list for the node before KEY, and construct an update
array storing the previous nodes at all the levels."
  (declare (type integer key)
		   (optimize (speed 3)))
  (format t "in make-update~%")
  (let ((node (skip-list-header sl))
		(update (make-header :initial-element (skip-list-header sl))))
;	(if (not (null update))
;		(format t "make-header update = ~A~%" update))
    (declare (type (or null simple-vector) node)
			 (type simple-vector update)
			 (optimize (speed 3)))
    (do ((level (1- (skip-list-level sl)) (1- level)))
		((< level 0) (values update (node-forward node 0)))
      (declare (type integer level))
	  (format t "make-update iteration ~D~%" level)
      (setf (node-forward update level)
			(setf node (follow-node node key level))))))

    
(defmethod skip-list-insert ((sl skiplist) key value)
  "Insert VALUE under KEY in the skip-list. Replaces the existing node
with KEY if present, or inserts a new node with random level."
  (multiple-value-bind (update node)
      (make-update sl key)
    (format t "update after first search: ~A~%" update)
    (if (and node (= (node-key node) key))
        ;; node with this key exists, just change the value. 
		(setf (node-value node) value)
        ;; a new node with random level needs to be inserted.
		(let* ((new-level (random-level))
			   (new-node (make-node key value new-level)))
		  (format t "new-level ~D~%" new-level)
		  (format t "new-node ~A~%" new-node)
		  (when (> new-level (skip-list-level sl))
			(setf (skip-list-level sl) new-level))
		  (incf (skip-list-length sl))
          ;; insert new node by changing the following nodes'
          ;; predecessor pointers 
		  (do ((level 0 (1+ level)))
			  ((= level new-level) new-node)
			(let ((next (node-forward update level)))
			  (format t "in loop level = ~D~%" level)
			  (when next
				(format t "next is true~%")
				(setf (node-forward new-node level) (node-forward next level)
					  (node-forward next level)      new-node))))))
    value))

(defmethod skip-list-reduce-header ((sl skiplist))
  "Sets the LEVEL slot of the skip-list according to the length of the header node."
  ;; adapt level slot to header structure
  (do ((level (1- (skip-list-level sl)) (1- level))
       (header (skip-list-header sl)))
      ((or (< level 0)
		   (node-forward header level))
       (setf (skip-list-level sl) (1+ level))
       sl)))

(defmethod skip-list-delete ((sl skiplist) key)
  "Delete the node with KEY in the skip-list."
  (multiple-value-bind (update node)
      (make-update sl key)
    (when (= (node-key node) key)
      (do ((level 0 (1+ level)))
		  ((= level (skip-list-level sl)))
		(let ((next (node-forward update level)))
		  (when (and next (eql (node-forward next level) node))
			(setf (node-forward next level) (node-forward node level)))))
      (decf (skip-list-length sl))
      (skip-list-reduce-header sl))
    sl))

;;; compatibility to old API

(defmethod skip-list-get (key (sl skiplist))
  (skip-list-search sl key))

(defmethod (setf skip-list-get) (new-value key (sl skiplist))
  (skip-list-insert sl key new-value))

(defmethod skip-list-remove (key (sl skiplist))
  (skip-list-delete sl key))

(defmethod skip-list-search-node ((sl skiplist) key)
  "Search for the node with KEY in the skiplist."
  (declare (type integer key)
		   (optimize (speed 3)))
  (do ((level (1- (skip-list-level sl)) (1- level))
       (node (skip-list-header sl) (follow-node node key level)))
      ((< level 0)
       (let ((result (node-forward node)))
		 (if (and result (= (node-key result) key))
			 result
			 nil)))
    (declare (type fixnum level)
			 (type simple-vector node))))

(defmethod skip-list-after-node ((sl skiplist) key)
  "Search for the node with key biffer or equal to KEY in the skip-list (for range queries)."
  (declare (type integer key)
		   (optimize (speed 3)))
  (do ((level (1- (skip-list-level sl)) (1- level))
       (node (skip-list-header sl) (follow-node node key level)))
      ((< level 0)
       (let ((result (node-forward node)))
		 (if (and result (>= (node-key result) key))
			 result
			 nil)))
    (declare (type fixnum level)
			 (type simple-vector node))))

(defmethod skip-list-search ((sl skiplist) key &optional not-found)
  (let ((result (skip-list-search-node sl key)))
    (if result
		(node-value result)
		not-found)))

(defun node-before (node key &optional (level 0))
  (let ((next (node-forward node level)))
    (and next (< (node-key next) key))))

(defmethod skip-list->list ((sl skiplist))
  (let ((node (skip-list-header sl)))
    (loop for next = (node-forward node) then (node-forward next)
	   while next
	   collect (list (node-key next) (node-value next)))))

;;;
;;; cursors
;;;

(defclass skiplist-cursor ()
  ((node :initarg :node :accessor skiplist-cursor-node)))

(defmethod sl-cursor-next ((slc skiplist-cursor) &optional eoc)
  (with-slots (node) slc
    (if node
		(let ((result (list (skiplist::node-key node)
							(skiplist::node-value node))))
		  (setf node (skiplist::node-forward node))
		  result)
		eoc)))

(defmethod sl-cursor-prev ((slc skiplist-cursor) &optional eoc)
  (declare (ignore eoc))
  (error "Can't move backward in skiplist"))

(defclass skiplist-value-cursor (skiplist-cursor)
  ())

(defmethod sl-cursor-next :around ((slc skiplist-value-cursor) &optional eoc)
  (let ((result (call-next-method)))
    (if (eql result eoc)
		eoc
		(second result))))

(defclass skiplist-key-cursor (skiplist-cursor)
  ())

(defmethod sl-cursor-next :around ((slc skiplist-key-cursor) &optional eoc)
  (let ((result (call-next-method)))
    (if (eql result eoc)
		eoc
		(first result))))

(defmethod skip-list-cursor ((sl skiplist) &key cursor (class 'skiplist-cursor))
  (if cursor
      (progn (setf (skip-list-cursor-node cursor)
				   (skiplist::node-forward (skiplist::skip-list-header sl)))
			 cursor)
      (make-instance class :node (skiplist::node-forward (skiplist::skip-list-header sl)))))

(defmethod skip-list-values-cursor ((sl skiplist))
  (skip-list-cursor sl :class 'skiplist-value-cursor))

(defmethod skip-list-keys-cursor ((sl skiplist))
  (skip-list-cursor sl :class 'skiplist-key-cursor))

(defclass skiplist-range-cursor (skiplist-cursor)
  ((end :initarg :end :reader slrc-end)))

(defmethod sl-cursor-next :around ((slc skiplist-range-cursor) &optional eoc)
  (with-slots (node end) slc
    (if (and node (< (skiplist::node-key node) end))
		(call-next-method)
		eoc)))

(defmethod skip-list-range-cursor ((sl skiplist) start end)
  (let ((node (skiplist::skip-list-after-node sl start)))
    (when node
      (make-instance 'skiplist-range-cursor :node node :end end))))

(defmethod map-skip-list (fun (sl skiplist))
  (let ((cursor (skip-list-cursor sl)))
    (do ((val (sl-cursor-next cursor) (sl-cursor-next cursor)))
		((null val))
      (apply fun val))))

(defmethod map-skip-list-values (fun (sl skiplist))
  (let ((cursor (skip-list-values-cursor sl)))
    (do ((val (sl-cursor-next cursor) (sl-cursor-next cursor)))
		((null val))
      (funcall fun val))))
