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

(defclass skiplist ()
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

(defmethod search ((sl skiplist) key)
  (let* ((node (skiplist-head sl))
		 (return-node (find node)))
	(if (not (null (node-next return-node)))
		(node-skipnode (node-next return-node))
		nil)))

(defmethod insert2 ((sl skiplist) key data)
  (labels ((adjust-skiplist-height (sl h)
			 (if (> h (skiplist-height))
				 (let ((new-sl (make-node nil nil (skiplist-head sl))))
				   (incf skiplist-height new-sl)
				   new-sl)
				 sl)))
	(let ((node (search sl key)))
	  (if node
		  (return-from insert node)
		  (let* ((height (random-level))
                (sl (adjust-skiplist-height height))
				(node (skiplist-head sl))
				(last nil))
            (loop for x downfrom (1- (skiplist-height sl))
                 until (null node) do
                 (setf node (find2 node key))
                 (when (<= x height)
                   (when (or (null (node-next node)) (not (= (node-key (node-next node)))))
                     (setf (node-next next) (make-node key (node-next node) nil)))
                   (when (not (null save))
                     (setf (node-down save) (node-next node))))
                 (setf node (node-down node))))))))

(defmethod insert ((sl skiplist) key data)
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

