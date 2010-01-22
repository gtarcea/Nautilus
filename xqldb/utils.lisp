(in-package :xqldb)

(defun mapappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(proclaim '(inline last1 single append1 conc1 mklist))

(defun last1 (lst)
  (car (last lst)))

(defun single (lst)
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun conc1 (lst obj)
  (nconc lst (list obj)))

(defun mklist (obj)
  (if (listp obj)
      obj
      (list obj)))

(defun longer (x y)
  (labels ((compare (x y)
	     (and (consp x)
		  (or (null y)
		      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
	(compare x y)
	(> (length x) (length y)))))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
	(if val
	    (push val acc))))
    (nreverse acc)))

(defun group (source n)
  (if (zerop n)
      (error "zero length"))
  (labels ((rec (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (rec rest (cons (subseq source 0 n) acc))
		   (nreverse (cons source acc))))))
    (if source
	(rec source nil) nil)))

(defun flatten (x)
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun prune (test tree)
  (labels ((rec (tree acc)
	     (cond ((null tree) (nreverse acc))
		   ((consp (car tree))
		    (rec (cdr tree) (cons (rec (car tree) nil) acc)))
		   (t (rec (cdr tree) (if (funcall test (car tree))
					  acc
					  (cons (car tree) acc)))))))
    (rec tree nil)))

(defun find2 (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
	(if val
	    (values (car lst) val)
	    (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  (and lst
       (let ((first (car lst)))
	 (cond ((funcall test y first) nil)
	       ((funcall test x first) lst)
	       (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  (let ((rest (before x y lst :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test))
	  :test test))

(defun split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src)))
	((or (null src) (funcall fn (car src)))
	 (values (nreverse acc) src))
      (push (car src) acc))))

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
	     (max (funcall fn wins)))
	(dolist (obj (cdr lst))
	  (let ((score (funcall fn obj)))
	    (when (> score max)
	      (setq wins obj
		    max score))))
	(values wins max))))

(defun best (fn lst)
  (if (null lst)
      nil
      (let ((wins (car lst)))
	(dolist (obj (cdr lst))
	  (if (funcall fn obj wins)
	      (setq wins obj)))
	wins)))

(defun mostn (fn lst)
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
	    (max (funcall fn (car lst))))
	(dolist (obj (cdr lst))
	  (let ((score (funcall fn obj)))
	    (cond ((> score max)
		   (setq max score
			 result (list obj)))
		  ((= score max)
		   (push obj result)))))
	(values (nreverse result) max))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args)
      (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

;(defun explode (sym)
;  (map 'list #'(lambda (c)
;		 (intern (make-string :1 :initial-element c)))
;       (symbol-name sym)))

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
	    (fns (butlast fns)))
	#'(lambda (&rest args)
	    (reduce #'funcall fns
		    :from-end t
		    :initial-value (apply fn1 args))))
      #'identity))

;; Functional if
(defun fif (if then &optional else)
  #'(lambda (x)
      (if (funcall if x)
	  (funcall then x)
	  (if else (funcall else x)))))

;; Functional intersection
(defun fintersection (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fintersection fns)))
	#'(lambda (x)
	    (and (funcall fn x) (funcall chain x))))))

;; Functional union
(defun funion (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'funion fns)))
	#'(lambda (x)
	    (or (funcall fn x) (funcall chain x))))))

(defun list-recurser (rec &optional base)
  (labels ((self (lst)
	     (if (null lst)
		 (if (functionp base)
		     (funcall base)
		     base)
		 (funcall rec (car lst)
			  #'(lambda ()
			      (self (cdr lst)))))))
    #'self))

#|
(defun tree-traverse (rec &optional (base #'identity))
  (labels ((self (tree)
	     (if (atom tree)
		 (if (functionp base)
		     (funcall base tree)
		     base)
		 (funcall rec (self (car tree))
			  (if (cdr tree)
			      (self (cdr tree)))))))
    #'self))
|#
;(defun tree-recurser (rec &optional (base #'identity))
;  (labels ((self (tree)
;	     (if (atom tree)
;		 (if (functionp base)
;		     (funcall base tree)
;		     base)
;		 (funcall rec tree
;			  #'(lambda ()
;			      (self (car tree)))
;			  #'(lambda ()
;			      (if (cdr tree)
;				  (self (cdr tree))))))))
;    #'self))
	  


  



