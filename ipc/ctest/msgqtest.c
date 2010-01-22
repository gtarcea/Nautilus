
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <errno.h>
#include <fcntl.h>

int msgqid = -1 ;

struct mboxmsg
{
    long mtype ;
    char mtext[64] ;
} ;

int main()
{
    create_queue() ;
    get_msgq_stat() ;
    if (msgqid != -1)
    {
        put_msg_on_queue() ;
        get_msg_from_queue() ;
    }
}

#define MSGQKEY 54327

int get_msgq_stat()
{
    struct msqid_ds s ;
    int rc = msgctl(msgqid, IPC_STAT, &s) ;
    printf("max bytes = %d\n", s.msg_qbytes) ;
}

int create_queue()
{
    errno = 0 ;
    /*int rc = msgget(54323, S_IRUSR | S_IWUSR | IPC_CREAT | IPC_EXCL) ;*/
    int rc = msgget(MSGQKEY, S_IRUSR | S_IWUSR | IPC_CREAT | IPC_EXCL) ;
    printf("rc = %d, errno = %d\n", rc, errno) ;
    if (rc > 0)
    {
        msgqid = rc ;
    }
    else
    {
        printf(" queue already exists trying to connect to it") ;
        errno = 0 ;
        rc = msgget(MSGQKEY, 0) ;
        printf("   rc = %d, errno = %d\n", rc, errno) ;
        if (rc > 0)
        {
            msgqid = rc ;
        }
        else
        {
            printf("Could not connect to a msgqueue, exiting\n") ;
            exit(1) ;
        }
    }
}

int delete_msg_queue()
{
    printf("Trying to delete the msgq...\n") ;
    errno = 0 ;
    int rc = msgctl(65537, IPC_RMID, 0) ;
    printf("  msgctl rc = %d, errno = %d\n", rc, errno) ;
}

int put_msg_on_queue()
{
    struct mboxmsg msg ;
    int i ;
    int rc ;
    msg.mtype = 1 ;
    for (i = 0 ; i < 5 ; i++)
    {
        sprintf(msg.mtext, "Hello world %d", i) ;
        errno = 0 ;
        rc = msgsnd(msgqid, &msg, 64, IPC_NOWAIT) ;
        printf("msgsnd rc = %d, errno = %d\n", rc, errno) ;
        if (rc < 0)
        {
            printf(" msgsnd %d failed, exiting...\n", i) ;
            exit(1) ;
        }
    }
}

int get_msg_from_queue()
{
    struct mboxmsg msg ;
    int i ;
    int rc ;
    
    for (i = 0 ; i < 5 ; i++)
    {
        errno = 0 ;
        rc = msgrcv(msgqid, &msg, 64, 0, 0) ;
        printf("msgrcv rc = %d, errno = %d", rc, errno) ;
        printf(" msg %d: msg.mtype = %d, msg.mtext = '%s'\n", i, msg.mtype, msg.mtext) ;
    }
}

