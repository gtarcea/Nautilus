;;;; Created on 2008-04-22 19:20:22

(in-package :xql)

(defstruct xnode
  tag
  data
  (docid 0)
  (start 0)
  (end 0)
  (level 0))

(defstruct tmatch-state stacks tq->current)

(defstruct xtree nodes)

(defstruct (nodelist (:conc-name nl-))
  (nlist (make-array 0 :fill-pointer t :adjustable t)))

(defun create-xtree (height)
  (make-xtree :nodes (make-array height :fill-pointer 0)))

(setf test-xtree (create-xtree 6))

(setf root (make-xnode :tag "root" :data "root" :start 1 :end 32 :level 0))

(setf a1 (make-xnode :tag "a" :data "a1" :start 2 :end 7 :level 1))
(setf b1 (make-xnode :tag "b" :data "b1" :start 3 :end 3 :level 2))
(setf c1 (make-xnode :tag "c" :data "c1" :start 4 :end 6 :level 2))
(setf d1 (make-xnode :tag "d" :data "d1" :start 5 :end 5 :level 3))

(setf a2 (make-xnode :tag "a" :data "a2" :start 8 :end 16 :level 1))
(setf b2 (make-xnode :tag "b" :data "b2" :start 9 :end 9 :level 2))
(setf b3 (make-xnode :tag "b" :data "b3" :start 10 :end 10 :level 2))
(setf c2 (make-xnode :tag "c" :data "c2" :start 11 :end 15 :level 2))
(setf d2 (make-xnode :tag "d" :data "d2" :start 12 :end 12 :level 3))
(setf e1 (make-xnode :tag "e" :data "e1" :start 13 :end 13 :level 3))
(setf e2 (make-xnode :tag "e" :data "e2" :start 14 :end 14 :level 3))

(setf a3 (make-xnode :tag "a" :data "a3" :start 17 :end 20 :level 1))
(setf c3 (make-xnode :tag "c" :data "c3" :start 18 :end 18 :level 2))
(setf e3 (make-xnode :tag "e" :data "e3" :start 19 :end 19 :level 2))

(setf a4 (make-xnode :tag "a" :data "a4" :start 21 :end 31 :level 1))
(setf b4 (make-xnode :tag "b" :data "b4" :start 22 :end 22 :level 2))
(setf c4 (make-xnode :tag "c" :data "c4" :start 23 :end 26 :level 2))
(setf d3 (make-xnode :tag "d" :data "d3" :start 24 :end 24 :level 3))
(setf e4 (make-xnode :tag "e" :data "e4" :start 25 :end 25 :level 3))
(setf c5 (make-xnode :tag "c" :data "c5" :start 27 :end 30 :level 2))
(setf d4 (make-xnode :tag "d" :data "d4" :start 28 :end 28 :level 3))
(setf e5 (make-xnode :tag "e" :data "e5" :start 29 :end 29 :level 3))

(setf alist (make-nodelist))
(setf blist (make-nodelist))
(setf clist (make-nodelist))
(setf dlist (make-nodelist))
(setf elist (make-nodelist))
(setf rootlist (make-nodelist))

;(format t "a1 tag = ~A~%" (xnode-tag a1))
;(format t "root tag = ~A~%" (xnode-tag root))

(vector-push-extend root (nl-nlist rootlist))
(vector-push-extend a1 (nl-nlist alist))
(vector-push-extend a2 (nl-nlist alist))
(vector-push-extend a3 (nl-nlist alist))
(vector-push-extend a4 (nl-nlist alist))

(vector-push-extend b1 (nl-nlist blist))
(vector-push-extend b2 (nl-nlist blist))
(vector-push-extend b3 (nl-nlist blist))
(vector-push-extend b4 (nl-nlist blist))

(vector-push-extend c1 (nl-nlist clist))
(vector-push-extend c2 (nl-nlist clist))
(vector-push-extend c3 (nl-nlist clist))
(vector-push-extend c4 (nl-nlist clist))
(vector-push-extend c5 (nl-nlist clist))

(vector-push-extend d1 (nl-nlist dlist))
(vector-push-extend d2 (nl-nlist dlist))
(vector-push-extend d3 (nl-nlist dlist))
(vector-push-extend d4 (nl-nlist dlist))

(vector-push-extend e1 (nl-nlist elist))
(vector-push-extend e2 (nl-nlist elist))
(vector-push-extend e3 (nl-nlist elist))
(vector-push-extend e4 (nl-nlist elist))
(vector-push-extend e5 (nl-nlist elist))

(vector-push rootlist (xtree-nodes test-xtree))
(vector-push alist (xtree-nodes test-xtree))
(vector-push blist (xtree-nodes test-xtree))
(vector-push clist (xtree-nodes test-xtree))
(vector-push dlist (xtree-nodes test-xtree))
(vector-push elist (xtree-nodes test-xtree))

(defparameter *tq->current* nil)

#|
;;; Find
(defun find-partial-solution (pattern-node )
  (cond
    ((is-leaf pattern-node) t)
    (t
     (setf n (number-of-children pattern-node))
     (setf partial-solution nil)
     (do ((i 0))
	 (or (not-empty Tqi) partial-solution)
       (cond
	 ((or (is-empty Tqi) (> Tqi->current.start Tq->current.end))
	  (if partial-solution
	      (progn
		(incf i)
		(setf partial-solution nil))
	      (progn
		(do ((j 0 (1+ j)))
		    (< j i)
		  (clean-stack Qj))
		(return nil)))
	  (if (= i n)
	      (return t)))
	 (t
	  (if (< Tqi->current.start Tq->current.start)
	      (advance Tqi)
	      (if (find-partial-solution qi)
		  (progn
		    (push Sqi Tqi->current Tq->current)
		    (setf partial-solution t)))
	      (advance Tqi)))))))
    nil)
|#

(defun advance ()
  nil)

(defun number-of-children ()
  nil)

(defun  clean-stack ()
  nil)

(defun generate-solution ()
  nil)

(defun join-stack-entries ()
  nil)

(defun generate-solution2 ()
  nil)


;; We need to set Tq->current for the Find() function

;;(car (reverse (pattern-node-children (car (pattern-tree-root patterntree))))) <- This is 'a'
(defun tree-match (patterntree x-tree stack-list)
  (let ((tq (nl-nlist (aref (xtree-nodes x-tree) 1)))
	(root-of-pattern-tree (car (reverse (pattern-node-children (car (pattern-tree-root patterntree)))))))
    (do ((x 0 (1+ x)))
	(= x (array-total-size tq))
      (setf tq->current (aref tq x))
      (if (find-partial-solution root-of-pattern-tree x-tree tq x stack-list patterntree)
	  ;; sq will always be Sa from paper example
	  (push-match (aref stack-list 0) tq->current)) ; push(Sq, Tq->current, root)
      (setf tq->current (aref tq x)))
    (generate-solution root-of-pattern-tree)))
