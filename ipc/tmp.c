
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
   printf("(defconstant %s %ld); #x%lx\n", lisp_name, unix_number, unix_number);
}

int main()
{
   printf("(in-package :cl-lwipc)\n") ;
    defconstant("IPC-STAT", IPC_STAT);
    defconstant("IPC-CREAT", IPC_CREAT);
    defconstant("IPC-EXCL", IPC_EXCL);
    defconstant("IPC-RMID", IPC_RMID);
    defconstant("IPC-NOWAIT", IPC_NOWAIT);
    defconstant("IPC-SET", IPC_SET);
    defconstant("ERRNO-EACESS", EACCES);
    defconstant("ERRNO-ENOENT", ENOENT);
    defconstant("ERRNO-EAGAIN", EAGAIN);
    defconstant("F-GETLK", F_GETLK);
    defconstant("F-SETLK", F_SETLK);
    defconstant("F-SETLKWAIT", F_SETLKW);
    defconstant("F-RDLCK", F_RDLCK);
    defconstant("F-WRLCK", F_WRLCK);
    defconstant("F-UNLCK", F_UNLCK);
    defconstant("SEEK-SET", SEEK_SET);
    defconstant("SEEK-CUR", SEEK_CUR);
    defconstant("SEEK-END", SEEK_END);
    defconstant("SHM-R", SHM_R);
    defconstant("SHM-W", SHM_W);
    defconstant("GETVAL", GETVAL);
    defconstant("SETVAL", SETVAL);
    defconstant("GETPID", GETPID);
    defconstant("GETNCNT", GETNCNT);
    defconstant("GETZCNT", GETZCNT);
    defconstant("S-IRUSR", S_IRUSR);
    defconstant("S-IWUSR", S_IWUSR);

   return 0 ;
}