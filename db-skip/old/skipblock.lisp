(in-package :skipdb)

(defclass skipblock ()
  ((blocknum :accessor blocknum
			 :documentaton "Blocks are numbered sequentially starting at 0")
   (bytes-used :accessor bytes-used
			   :documentation "The size of the block - allows for storing variable size data"))
  (:documentation "A single block that can be used for storing some-of a key or data"))

(defclass skipblock-list ()
  ((block-count :accessor block-count
				:documentation "Number of blocks in the skipblock-list")
   (block-list :accessor block-list
			   :documentation "The list of blocks"))
  (:documentation "Data and keys are made up of a list of 1 or more blocks"))

(defclass skipentry ()
  ((skip-key :accessor skip-key
			 :documentation "They key for the skiplist entry - Currently this will be fixed at an integer.
Later we can come back and relax this constraint to allow any key size")
   (skip-data :accessor skip-data
			  :documentation "A skipblock-list containing the data")
   (next :accessor next
		 :documentation "An offset to the next skipentry")
   (is-free? :accessor is-free?
			 :documentation "Is this skipentry in use?")))

;;
;; Overview - initially this is really simple. The skip-list from bknr is shredded 1 entry in the
;; skip-list being turned into a skipentry (and correspondingly further decomposed into blocks).
;; The key structure is simple and essentially unused at the moment. When reading back the skip-list
;; we bring the whole thing back into memory reconstructing the entire skip-list.
;;
;; This is not efficient and would never work for a real work application. However it will work for
;; getting our feet wet and reading/writing the entire list as a set of decomposed blocks.
;;
;; The next step after this is figuring out how to store a real skiplist on disk.
;;

(defun store-skip-list (skiplist path)
  (with-open-file (stream path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
	(loop)))
   