(in-package :skipdb)

(defclass skipdb-dir ()
  ((db-dir  :initarg :db-dir
            :accessor db-dir
            :documentation "The path to the database directory")
   (db-key-start :initarg :db-key-start
                 :accessor db-key-start
                 :documentation "Starting key")
   (db-key-range :initarg :db-key-range
                 :accessor db-key-range
                 :documentation "The range of keys per key directory entry")
   (db-key-lt :initarg :db-key-lt
              :accessor db-key-lt
              :documentation "Comparator function for key <")
   (db-key-gt :initarg :db-key-gt
              :accessor db-key-gt
              :documentation "Comparator function key >")
   (db-key-= :initarg :db-key-=
             :accessor db-key-=
             :documentation "Comparator for key =")))

(defgeneric db-dir-file-path (db-dir filekey)
  (:documentation "Given a key return the corresponding file path"))

(defgeneric ensure-db-dir-file-path (db-dir filekey)
  (:documentation "Given a key return the corresponding file path - ensuring that the directory path exists"))

(defun make-db-dir (path key-start key-range)
  (make-instance 'skipdb-dir
                 :db-dir path
                 :db-key-start key-start
                 :db-key-range key-range
                 :db-key-lt #'<
                 :db-key-gt #'>
                 :db-key-= #'=))

(defun db-dir-open (dir &key (key-start 1) (key-range 2500))
  (when (handler-case (fad:directory-exists-p dir)
          (t () nil))
    (make-db-dir dir key-start key-range)))

(defun get-keys (db-dir filekey)
  (multiple-value-bind (p1 p2) (floor filekey (db-key-range db-dir))
    (let* ((multiplier (if (= p2 0) (1- p1) p1))
           (start (+ (* multiplier (db-key-range db-dir)) (db-key-start db-dir)))
           (end (1- (+ start (db-key-range db-dir)))))
      (values start end))))

(defun directory-key-path (db-dir filekey)
  (multiple-value-bind (start end) (get-keys db-dir filekey)
    (merge-pathnames (fad:pathname-as-directory (db-dir db-dir)) (format nil "db~D-~D" start end))))


(defun get-file-key-path (dbdir filekey)
  (let* ((directory-path (namestring (directory-key-path dbdir filekey)))
         (file-path (merge-pathnames (fad:pathname-as-directory directory-path) (format nil "~D.sle" filekey))))
    (values file-path (not (null (fad:file-exists-p file-path))))))

(defun ensure-get-file-key-path (dbdir filekey)
  (let* ((directory-path (namestring (directory-key-path dbdir filekey)))
        (created? (handler-case (ensure-directories-exist directory-path)
                    (:no-error (ignore1 ignore2) t)
                    (t () nil))))
    (values (merge-pathnames (fad:pathname-as-directory directory-path) (format nil "~D.sle" filekey)) created?)))

(defmethod db-dir-file-path ((db-dir skipdb-dir) filekey)
  (get-file-key-path db-dir filekey))

(defmethod ensure-db-dir-file-path ((db-dir skipdb-dir) filekey)
  (ensure-get-file-key-path db-dir filekey))