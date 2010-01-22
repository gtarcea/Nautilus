(in-package :skipdb)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defgeneric set-data (new-data entry)
  (:documentation "Sets the data for a skipnode"))

(defgeneric set-key (new-key entry)
  (:documentation "Sets the key for a skipnode"))

(defstruct (skipnode
             (:print-object print-skipnode)
             (:constructor new-skipnode (&key (up nil) (down nil) (prev nil) (next nil) data key)))
  up down prev next data key)

(defmethod set-data (new-data (skipnode skipnode))
  (setf (skipnode-data skipnode) new-data))

(defmethod set-key (new-key (skipnode skipnode))
  (setf (skipnode-key skipnode) new-key))

(defstruct (skiplist
             (:print-object print-skiplist)
             (:constructor new-skiplist (&key (max-levels 32) (height 0) (head nil) (bottom nil) (key< #'<) (key= #'=))))
  max-levels height head bottom key< key=)

;;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;;; ================ Constructors ======================
;;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(defun make-skiplist (&key (levels 32) (< #'<) (= #'=))
  "A cover function for the constructor so we can add logic if needed"
  (let ((sl (new-skiplist :max-levels levels :key< < :key= =)))
    (setf (skiplist-bottom sl) (skiplist-head sl))
    sl))

(defun make-skipnode (&key key data (up nil) (down nil) (prev nil) (next nil))
  "A cover function for the constructor so we can add logic if needed"
  (new-skipnode :key key :data data :up up :down down :prev prev :next next))

;;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;;; ======= Print functions for nodes and list =========
;;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(defun print-skipnode (node stream)
  (labels ((skipnode->list (lst)
             (if (skipnode-p lst)
                 (cons (skipnode-key lst) (skipnode->list (skipnode-next lst)))
                 lst)))
    (format stream "#<SKIPNODE ~A>" (skipnode->list node))))

(defun print-skiplist (sl stream)
  (labels ((skipnode->list-down (lst)
             (if (skipnode-p lst)
                 (cons (skipnode-key lst) (skipnode->list-down (skipnode-down lst)))
                 lst)))
    (format stream "<SKIPLIST ~D ~A>" (skiplist-height sl) (skipnode->list-down (skiplist-head sl)))))

;;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;;; ======= Random number generation for height ========
;;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;;; This code was taken from the bknr skiplist implementation
;;; Note: Rewrite code to remove warnings and to make the random
;;; numbers better for the skiplist (see example skiplist code at
;;; http://eternallyconfuzzled.com/tuts/datastructures/jsw_tut_skip.aspx

#|
1 int rheight ( int max )
 2 {
 3   static int bits = 0;
 4   static int reset = 0;
 5   int h, found = 0;
 6 
 7   for ( h = 0; !found; h++ ) {
 8     if ( reset == 0 ) {
 9       bits = rand();
10       reset = sizeof ( int ) * CHAR_BIT;
11     }
12 
13     found = bits & 1;
14     bits = bits >> 1;
15     --reset;
16   }
17 
18   if ( h >= max )
19     h = max - 1;
20 
21   return h;
22 }
|#

(defparameter *bits* 0)
(defparameter *reset* 0)
(defconstant +char-bit+ 8)
(defconstant +32x8+ (* 32 8))

#|
(defun rheight (max &optional (found 0))
  (let ((found 0))
    (do (h 0 (1+ h))
        (> 0 found)
      (when (= *reset* 0)
        (setf *bits* (get-internal-real-time))
        (setf *reset* +32x8+))
      (setf found (logand *bits* 1))
      ))) ;; Not done
|#

(defparameter *random-n*
  (the integer (get-internal-real-time))
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

(defun random-height (max-height)
  "Returns a random height for a new skiplist node, with SL-RANDOM p for each level."
  (declare (optimize speed))
  (do ((h 1 (1+ h)))
      ((or (= h max-height)
           (sl-random)) h)
    (declare (type fixnum h))))

;;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;;; ======= Utility functions to help with finding =====
;;;;         and traversing a skiplist
;;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;;
;; TODO: add-skipnode-across, add-skipnode-up, add-skipnode-across-and-up are <essentially>
;; the same except for the directionality pointers. These could be refactored to remove redundant
;; and to combine functions and key logic.
;;
;; These functions assume you are adding data in order
;;
(defun add-skipnode-across (key data lst)
;  (format t "add-skipnode-across key = ~D~%" key)
;  (format t "lst key = ~D~%" (skipnode-key lst))
  (let ((elt (new-skipnode :key key :data data :prev lst)))
    (when (skipnode-p lst)
;      (when (skipnode-next lst)
;        (format t "   skipnode-next data = ~D~%" (skipnode-data (skipnode-next lst))))
;      (format t "   lst = ~A~%" lst)
      (if (skipnode-next lst)
          (setf (skipnode-prev (skipnode-next lst)) elt
                (skipnode-next elt) (skipnode-next lst)))
      (setf (skipnode-next lst) elt))
    elt))

(defun add-skipnode-across-and-down (key data lst down-node)
  (let ((new-node (add-skipnode-across key data lst)))
    (unless (null down-node)
      (setf (skipnode-up down-node) new-node))
    (setf (skipnode-down new-node) down-node)
    new-node))

(defun add-skipnode-up (key data lst)
  (let ((elt (new-skipnode :key key :data data :down lst)))
    (when (skipnode-p lst)
      (if (skipnode-up lst)
          (setf (skipnode-down (skipnode-up lst)) elt
                (skipnode-up elt) (skipnode-up lst)))
      (setf (skipnode-up lst) elt))
    elt))

(defun add-nodes-going-up (key data < node last-node lst to)
  (if (= to 0)
      nil
      (progn
        (let ((new-last-node (add-skipnode-across-and-down key data node last-node)))
          (add-nodes-going-up key data < (find-across (skipnode-up lst) key <) new-last-node (skipnode-up lst) (1- to))))))

;; ======================================================================================

(defun find-across (node key <)
  "Search across the list finding the node before the key"
;  (format t "find-across~%")
  (cond
    ((null node) nil)
    ((and (not (null (skipnode-next node))) (funcall < (skipnode-key (skipnode-next node)) key))
 ;    (format t "  node key = ~D~%" (skipnode-key node))
     (find-across (skipnode-next node) key <))
    (t
;     (format t "  from find-across returning key = ~D~%" (skipnode-key node))
     node)))

(defun find-across-and-down (node key func< func=)
  "Searches for key from node, going across and down the skiplist"
;  (format t "find-across-and-down node key = ~D, key searching for = ~D~%" (if node (skipnode-key node) nil) key)
  (when (null node)
    (return-from find-across-and-down nil))
  (if (not (null (skipnode-down node)))
      (let ((node2 (find-across node key func<)))
        (when node2 ; combine two when statements into a series of ands
;          (format t "  not null, node2 key = ~D~%" (skipnode-key node2))
          (when (and (not (null (skipnode-next node))) (funcall func= (skipnode-key (skipnode-next node)) key))
              (return-from find-across-and-down node)))
        (find-across-and-down (skipnode-down node2) key func< func=))
      (progn
;        (format t "skipnode-down node is null, key for node = ~D~%" (skipnode-key node))
        (find-across node key func<))))
; previous code:        node))) This didn't work, but saving just in case the logic above is wrong.

(defun goto-bottom (node)
  "Given a node keep following its down pointers until we are at the bottom"
  (if (skipnode-down node)
      (goto-bottom (skipnode-down node))
      node))

;;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;;; ============= skiplist API functions ===============
;;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(defun sl-find-node (sl key)
  "Find a node in the skiplist returning the node"
  (if sl
      (let* ((start-node (skiplist-head sl))
             (found-node (find-across-and-down start-node key (skiplist-key< sl) (skiplist-key= sl))))
        (cond
          ((null found-node) nil)
          ((not (null (skipnode-next found-node)))
           (skipnode-next found-node))
          (t
           (if (funcall (skiplist-key= sl) (skipnode-key found-node) key)
               found-node
               nil))))
      nil))

(defun sl-find (sl key)
  (let ((found-node (sl-find-node sl key)))
    (if found-node
        (skipnode-data found-node)
        nil)))

(defun sl-insert (sl key data)
  "Insert a new node into the skiplist"
  ;; This code could use a little refactoring - there is way too much use of loop rather than tail recurision
  ;; and the all the extra internal functions make it hard to follow. Using recursion should clean up and
  ;; reduce the size of the internal functions then the decision can be made as to what functions should
  ;; be moved outside of sl-insert.
  (labels ((add-first-node (height)
             (let* ((first-node (add-skipnode-up key data nil))
                    (node first-node))
               (setf (skiplist-bottom sl) first-node)
               (setf (skiplist-head sl) first-node)
               (setf (skiplist-height sl) height)
               (loop for x downfrom height by 1 until (= x 1) do
                    (setf node (add-skipnode-up key data node))
                    (setf (skiplist-head sl) node))))
           ;; =====================================================================
           (add-height (height)
             (let ((head (skiplist-head sl)))
               (loop for x downfrom height by 1 until (= x (skiplist-height sl)) do
                    (setf (skiplist-head sl) (add-skipnode-up (skipnode-key head) (skipnode-data head) head))
                    (setf head (skiplist-head sl)))))
           ;; =====================================================================
           (insert-new-nodes (from-node height)
             ;; First check to see if we are adding new height.
             (when (> height (skiplist-height sl))
                 (add-height height))
             ;; Now start adding nodes upwards
             (let ((node (goto-bottom from-node)) ; goto bottom of current node
                   (last-node nil))
               (add-nodes-going-up key data (skiplist-key< sl) node last-node (skiplist-bottom sl) height)))
           ;; =====================================================================
             (create-new-head (height)
               (let ((old-start (skiplist-bottom sl))
                     (old-height (skiplist-height sl)))
                 (add-first-node height)
                 (let ((old-list old-start)
                       (new-list (skiplist-bottom sl)))
                   (loop for x downfrom old-height by 1 until (= x 0) do
                        (setf (skipnode-next new-list) old-list)
                        (setf old-list (skipnode-up old-list))
                        (setf new-list (skipnode-up new-list))))))
           ;; =====================================================================
           (add-new-node (from-node height)
             ;; Cases to take care of:
             ;;   1. We are adding a new head
             ;;   2. We are adding height, but not a new head set of nodes
             (if (funcall (skiplist-key< sl) key (skipnode-key (skiplist-head sl)))
                 (create-new-head height)
                 (insert-new-nodes from-node height))
             (when (> height (skiplist-height sl))
               (setf (skiplist-height sl) height))))
    ;; ================ sl-insert starts here =====================================================
    (when (not (null sl))
      (let* ((new-height (random-height (skiplist-max-levels sl)))
             (found-node (find-across-and-down (skiplist-head sl) key (skiplist-key< sl) (skiplist-key= sl))))
        (cond ((and (null (skiplist-head sl)) (null found-node))
               ;; The skiplist is empty
               (add-first-node new-height))
              ((not (funcall (skiplist-key= sl) (skipnode-key found-node) key))
               ;; No node found - go ahead and add a new one
               (add-new-node found-node new-height))
              (t ; Node found
               found-node))))))

(defun sl-mapc (sl func)
  (labels ((iterate-across (node)
             (when (not (null node))
               (funcall func node)
               (if (not (null (skipnode-next node)))
                   (iterate-across (skipnode-next node))
                   nil))))
    (when sl
      (iterate-across (skiplist-bottom sl)))))

(defun sl-remove (sl key)
  "Removes all nodes matching key. Returns the node removed if found, otherwise returns nil"
  (labels ((remove-nodes-from (node)
             (let ((previous-node (skipnode-prev node))
                   (next-node (skipnode-next node))
                   (down-node (skipnode-down node)))
               (when previous-node
                 (setf (skipnode-next previous-node) next-node))
               (when next-node
                 (setf (skipnode-prev next-node) previous-node))
               ;; Nil out pointers contained in current node
               (setf (skipnode-up node) nil
                     (skipnode-down node) nil
                     (skipnode-next node) nil
                     (skipnode-prev node) nil)
               (when down-node
                 (remove-nodes-from down-node)))))
    (let ((found-node (sl-find-node sl key)))
      (when found-node
;;;            (format t "found-node key = ~D~%" (skipnode-key found-node))
        (remove-nodes-from found-node)
        found-node))))

(defun sl-mapkey (sl key func)
  (labels ((update-nodes-from (node)
             (let ((down-node (skipnode-down node)))
               (funcall func node down-node)
               (when down-node
                 (update-nodes-from down-node)))))
    (let ((found-node (sl-find-node sl key)))
      (and found-node
        (update-nodes-from found-node)))))

(defun sl-update-found-node (sl key data)
  (let ((found-node (sl-find-node sl key)))
    (when found-node
      ; (setf (skipnode-data found-node) data))))
      (set-data data found-node))))

(defun sl-update-all-nodes-with-key (sl key data)
  (sl-mapkey sl key #'(lambda (node down-node)
                        (when (null down-node)
                          (set-data data node)))))

(defun sl-update (sl key data &key (all-nodes t))
  "Updates the data pointed at by key. Because the system
doesn't know if the data is stored as a pointer (say data
is a defstruct) or a primitive such as an integer, the
system by default will find all nodes up and down the
skiplist with the key and update each of their data
references"
  (if all-nodes
      (sl-update-all-nodes-with-key sl key data)
      (sl-update-found-node sl key data)))


  