;;;; Created on 2008-04-19 13:38:32

(defvar *nodes* nil)

(defun defnode (&rest args)
  (push args *nodes*)
  args)

(defun compile-net (root)
  (let ((node (assoc root *nodes*)))
    (if (null node)
        nil
        (let ((contents (second node))
              (yes (third node))
              (no (fourth node)))
          (if yes
              (let ((yes-fn (compile-net yes))
                    (no-fn (compile-net no)))
                (format t "in yes~%")
                #'(lambda ()
                    (format t "~A~%>> " contents)
                    (funcall (if (eq (read) 'yes)
                                 yes-fn
                                 no-fn))))
              (progn
                (format t "in not yes ~%")
                #'(lambda () contents)))))))

(defnode 'people "Is the person a man?" 'male 'female)
(defnode 'female 'we-are-done)
(defnode 'male "Is he living?" 'liveman 'deadman)
(defnode 'deadman "Was he American?" 'us 'them)
(defnode 'us "Is he on a coin?" 'coin 'not-coin)
(defnode 'coin "Is the coin a penny?" 'penny 'coins)
(defnode 'penny 'lincoln)
