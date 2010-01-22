;;;; Created on 2008-04-19 15:48:41
(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

(in-package :xql)

(defstruct pattern-node tag (data nil) (children nil))

(defstruct pattern-tree name (root nil))

(setf root (make-pattern-node :tag "root"))

(setf l1c1 (make-pattern-node :tag " l1c1"))
(push l1c1 (pattern-node-children root))

(setf l1c2 (make-pattern-node :tag " l1c2"))
(push l1c2 (pattern-node-children root))

(setf l1c3 (make-pattern-node :tag " l1c3"))
(push l1c3 (pattern-node-children root))

(setf l2l1c3 (make-pattern-node :tag "  l2l1c3"))
(push l2l1c3 (pattern-node-children l1c3))

(setf l2c2l1c3 (make-pattern-node :tag "  l2c2l1c3"))
(push l2c2l1c3 (pattern-node-children l1c3))

(setf l2l1c1 (make-pattern-node :tag "  l2l1c1"))
(push l2l1c1 (pattern-node-children l1c1))

(setf l3c1 (make-pattern-node :tag "   l3c1"))
(push l3c1 (pattern-node-children l2l1c1))

(setf root2 (make-pattern-node :tag "root"))
(setf r2l1c1 (make-pattern-node :tag "r2l1c1"))
(push r2l1c1 (pattern-node-children root2))

(setf root3 (make-pattern-node :tag "l1c1"))

(setf test-ntree (make-pattern-tree :name "test-tree" ))
(push root (pattern-tree-root test-ntree))

(setf match-ntree (make-pattern-tree :name "match-tree"))
(push l1c3 (pattern-tree-root match-ntree))

(defun traverse-children (fn c1)
  (when c1
    (traverse-children fn (pattern-node-children (car c1)))
    (funcall fn (car c1))
    (traverse-children fn (cdr c1))))

(defun find-sub (fn c)
  (cond
    ((null c) nil)
    ((funcall fn (car c))
     (format t "funcall returned true~%")
     (car c))
    (t
     (or (find-sub fn (pattern-node-children (car c)))
     (find-sub fn (cdr c))))))

(defun find-sub2 (fn c)
  (if (null c)
      nil
      (if (funcall fn (car c))
          (car c)
          (progn
            (format t "I am here~%")
            (or (find-sub2 fn (pattern-node-children (car c)))
            (find-sub2 fn (cdr c)))))))

(defun find-node (fn tree)
  (if (null tree)
      nil
      (let ((children (pattern-tree-root tree)))
        (find-sub fn children))))

(defun compare-trees (fn tree1 tree2)
  (cond 
    ((and (null tree1) (null tree2)) t)
    ((or (null tree1) (null tree2)) nil)
    (t  
      (let ((children1 (pattern-tree-root tree1))
            (children2 (pattern-tree-root tree2)))
        (labels ((cmp-trees (lst1 lst2)
                   (cond
                     ((and (null lst1) (null lst2)) t)
                     ((or (null lst1) (null lst2)) nil)
                     ((funcall fn (car lst1) (car lst2))
                      (cmp-trees (pattern-node-children (car lst1)) (pattern-node-children (car lst2)))
                      (cmp-trees (cdr lst1) (cdr lst2)))
                     (t nil))))
          (cmp-trees children1 children2))))))

(defvar *subtrees-list* nil)

(defun build-match (tree1 tree2)
  (format t "building a match for ~A ~A~%" (pattern-node-tag tree1) (pattern-node-tag tree2)))
