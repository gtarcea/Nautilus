
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>

/*
** Right now these are simple wrappers around the msg queue interface.
** In all likelihood these will wrap more functionality as I figure out
** the details of what I want to do. If its easier to wrap the code in
** c and then have lisp call it, then that is what I will do.
*/

/*
** This function connects to an existing msgq, or creates it if it doesn't exist.
*/
extern int msgq_msgconnect(key_t key)
{
}

extern int msgq_msgget(key_t key, int flag)
{
  return msgget(key, flag) ;
}

extern int msgq_msgrm(int msgqid)
{
  return msgctl(msgqid, IPC_RMID, NULL) ;
}

extern int msgq_msgsnd(int msgqid, void *msg, size_t nbytes, int flag)
{
  return msgsnd(msgqid, msg, nbytes, flag) ;
}

extern int msgq_msgrcv(int msgqid, void *msg, size_t nbytes, int flag)
{
  return msgrcv(msgqid, msg, nbytes, flag) ;
}
