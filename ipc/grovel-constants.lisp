(in-package :cl-lwipc-asd)

(defun write-groveler (file constants)
  (with-open-file (f file :direction :output :if-exists :supersede)
    (let ((*print-case* :upcase))
      (format f "
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/sem.h>
#include <sys/shm.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>

void defconstant(char *lisp_name, long unix_number)
{
   printf(\"(defconstant %s %ld); #x%lx\\n\", lisp_name, unix_number, unix_number);
}

int main()
{
   printf(\"(in-package :cl-lwipc)\\n\") ;")
      (dolist (c constants)
	(format f "~&    defconstant(\"~A\", ~A);~%" (car c) (cdr c)))
      (format f "
   return 0 ;
}"))))


(unless (boundp *grovel*)
  (error "No GROVEL hook!"))

(defvar *grovel*)

(setf *grovel*
      (lambda (c obj lisp)
	(write-groveler c
			'(;; IPC Constants
			  (ipc-stat . IPC_STAT)
			  (ipc-creat . IPC_CREAT)
			  (ipc-excl . IPC_EXCL)
			  (ipc-rmid . IPC_RMID)
			  (ipc-nowait . IPC_NOWAIT)
			  (ipc-set . IPC_SET)
			  (errno-eacess . EACCES)
			  (errno-enoent . ENOENT)
			  (errno-eagain . EAGAIN)
			  (f-getlk . F_GETLK)
			  (f-setlk . F_SETLK)
			  (f-setlkwait . F_SETLKW)
			  (f-rdlck . F_RDLCK)
			  (f-wrlck . F_WRLCK)
			  (f-unlck . F_UNLCK)
			  (seek-set . SEEK_SET)
			  (seek-cur . SEEK_CUR)
			  (seek-end . SEEK_END)
			  (shm-r . SHM_R)
			  (shm-w . SHM_W)
			  (getval . GETVAL)
			  (setval . SETVAL)
			  (getpid . GETPID)
			  (getncnt . GETNCNT)
			  (getzcnt . GETZCNT)
			  (s-irusr  . S_IRUSR)
			  (s-iwusr  . S_IWUSR)))
	(and (zerop (run-shell-command "~A ~A -o ~A"
				       *gcc*
				       (namestring c)
				       (namestring obj)))
	     (zerop (run-shell-command "~A > ~A"
				       (namestring obj)
				       (namestring lisp))))))
