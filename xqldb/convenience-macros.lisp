;;;; Created on 2008-04-12 20:17:40

(in-package :xqldb)

(defmacro for (listspec exp)
   (cond ((and (= (length listspec) 3)
               (symbolp (car listspec))
               (eq (cadr listspec) ':in))
          `(mapcar (lambda (,(car listspec))
                      ,exp)
                   ,(caddr listspec)))
         (t (error "Ill-formed: %s" `(for ,listspec ,exp)))))

(defmacro build-symbol (&rest l)
  (let ((p (find-if (lambda (x) (and (consp x) (eq (car x) ':package)))
                    l)))
    (cond (p
           (setq l (remove p l))))
    (let ((pkg (cond ((eq (cadr p) 'nil)
                      nil)
                 (t `(find-package ',(cadr p))))))
      (cond (p
             (cond (pkg
                    `(values (intern ,(symstuff l) ,pkg)))
               (t
                `(make-symbol ,(symstuff l)))))
        (t
         `(values (intern ,(symstuff l))))))))

(defun symstuff (l)
  `(concatenate 'string
                ,@(for (x :in l)
                       (cond ((stringp x)
                              `',x)
                         ((atom x)
                          `',(format nil "~a" x))
                         ((eq (car x) ':<)
                          `(format nil "~a" ,(cadr x)))
                         ((eq (car x) ':++)
                          `(format nil "~a" (incf ,(cadr x))))
                         (t
                          `(format nil "~a" ,x))))))



(defmacro make-counter (name)
  (let ((f-incrementer (intern (concatenate 'string "INCREMENT-" (symbol-name name))))
        (f-get (intern (concatenate 'string "GET-" (symbol-name name))))
        (f-reset (intern (concatenate 'string "RESET-" (symbol-name name))))
        (f-decrement (intern (concatenate 'string "DECREMENT-" (symbol-name name)))))
    `(let ((,name 0))
       (defun ,f-incrementer () (incf ,name))
       (defun ,f-get () ,name)
       (defun ,f-reset () (setf ,name 0))
       (defun ,f-decrement () (decf ,name)))))


(defmacro macexp (expr)
  `(pprint (macroexpand-1 ',expr)))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro when-bind* (binds &body body)
  (if (null binds)
      `(progn ,@body)
      `(let (,(car binds))
	 (if ,(caar binds)
	     (when-binds* ,(cdr binds) ,@body)))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
		     `(,s (gensym)))
		 syms)
     ,@body))

(defmacro condlet (clauses &body body)
  (let ((bodfn (gensym))
	(vars (mapcar #'(lambda (v)
			  (cons v (gensym)))
		      (remove-duplicates
		       (mapcar #'car (mappend #'cdr clauses))))))
    `(labels ((,bodfn ,(mapcar #'car vars)
		,@body))
       (cond ,@(mapcar #'(lambda (cl)
			   (condlet-clause vars cl bodfn))
		       clauses)))))

(defun condlet-clause (vars cl bodfn)
  `(,(car cl) (let ,(mapcar #'cdr vars)
		(let ,(condlet-binds vars cl)
		  (,bodfn ,@(mapcar #'cdr vars))))))

(defun condlet-binds (vars cl)
  (mapcar #'(lambda (bindform)
	      (if (consp bindform)
		  (cons (cdr (assoc (car bindform) vars))
			(cdr bindform))))
	  (cdr cl)))

(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c)
			 `(eql ,insym ,c))
		     choices)))))

(defmacro inq (obj &rest args)
  `(in ,obj ,@(mapcar #'(lambda (a)
			  `',a)
		      args)))

(defmacro in-if (fn &rest choices)
  (let ((fnsym (gensym)))
    `(let ((,fnsym ,fn))
       (or ,@(mapcar #'(lambda (c)
			 `(funcall ,fnsym ,c))
		     choices)))))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro till (test &body body)
  `(do ()
       (,test)
     ,@body))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
	  (,gstop ,stop))
	 ((> ,var ,gstop))
       ,@body)))

(defmacro setf-all-to (val &rest args)
  (with-gensyms (gval)
    `(let ((,gval ,val))
       (setf ,@(mapcan #'(lambda (a)
			   (list a gval))
		       args)))))

(defmacro setf-all-nil (&rest args)
  `(setf-all-to nil ,@args))

(defmacro setf-all-t (&rest args)
  `(setf-all-to t ,@args))

(defmacro setf-using-f (op place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    `(let* (,@(mapcar #'list vars forms)
	    (,(car var) (,op ,access ,@args)))
       ,set)))

(defmacro pull (obj place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,obj)
	      ,@(mapcar #'list vars forms)
	      (,(car var) (delete ,g ,access ,@args)))
	 ,set))))

(defmacro pull-if (test place &rest args)
    (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,test)
	      ,@(mapcar #'list vars forms)
	      (,(car var) (delete-if ,g ,access ,@args)))
	 ,set))))

(defmacro popn (n place)
    (multiple-value-bind (vars forms var set access)
	(get-setf-expansion place)
      (with-gensyms (gn glst)
	`(let* ((,gn ,n)
		,@(mapcar #'list vars forms)
		(,glst ,access)
		(,(car var) (nthcdr ,gn ,glst)))
	   (prog1 (subseq ,glst 0 ,gn)
	     ,set)))))

(defmacro sortf (op &rest places)
  (let* ((methods (mapcar #'(lambda (p)
			     (multiple-value-list (get-setf-expansion p)))
		   places))
	 (temps (apply #'append (mapcar #'third methods))))
    `(let* ,(mapcar #'list
		    (mapcan #'(lambda (m) (append (first m) (third m))) methods)
		    (mapcan #'(lambda (m) (append (second m) (list (fifth m)))) methods))
       ,@(mapcon #'(lambda (rest)
		     (mapcar #'(lambda (arg)
				 `(unless (,op ,(car rest) ,arg)
				    (rotatef ,(car rest) ,arg)))
			     (cdr rest)))
		 temps)
       ,@(mapcar #'fourth methods))))
			    