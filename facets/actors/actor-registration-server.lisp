(in-package :com.facets.actors)

;;;;
;;;; Logic for server:
;;;;   The server is itself a set of communicating actors. The
;;;; main thread kicks off the other actors - one actor per
;;;; registered remote server.
;;;;
;;;;
;;;;
;;;;
;;;;
;;;;
;;;;


(defun main ()
  (let ((port (get-config-parameter :registration-server-port)))
    ))