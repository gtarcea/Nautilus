/*
** This file implements the low level interfaces to mailboxes. Much of the
** low level system handling is done in 'C' so that lisp can concentrate on
** the core mailbox algorithms.
*/

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/msg.h>
#include <sys/sem.h>
#include <errno.h>
#include <fcntl.h>

struct mailbox
{
    int id ;
    char name[32] ;
} ;

Struct mailmsg
{
    long mtype ;     /* Message type, from msgqueue, this is the original filter */
    char other[32] ; /* Other information we can use to filter on */
    int msglength ;  /* Length of message that will be stored in the message queue */
    char msg[1024] ; /* The message */
} ;

struct shmem_mailmsg
{
    int empty ;
    struct mailmsg msg ;
} ;

/*
** These should be configurable, but for now we'll just hard code them.
*/

#define MAX_MAILBOXES 20
#define MAX_MESSAGES 50

struct shmem_mailbox
{
    int id ;
    struct mailmsg messages[MAX_MESSAGES] ;
}

#define SHMEM_MAILBOX_SIZE (sizeof(struct shmem_mailbox))

struct msgq_msg
{
    long mtype ;
    char msg[32] ;
} ;

struct shmem_meta
{
    int active_mailboxes ;
    int configured_mailboxes ;
} ;

#define SHMEM_SIZE (1024 + (SHMEM_MAILBOX_SIZE * MAX_MAILBOXES))

extern int mailsystem_setup(int id, int max_mailboxes)
{
    /*
    ** Create system if it is not set up, otherwise just attach to various pieces.
    */
    if (max_mailboxes < 1 || max_mailboxes > MAX_MAILBOXES)
    {
	return -1 ;
    }

    if (create_semaphores(id, max_mailboxes) < 0)
    {
	return -1 ;
    }

    if (create_shmem(id, max_mailboxes) < 0)
    {
	cleanup_semaphores(id) ;
	return -1 ;
    }
}

int create_semaphores(int id, int max_mailboxes)
{
    /*
    ** The number of semaphores needed is:
    **  1 for shmem meta area.
    ** The semaphores needed for mailboxes are set up when a mailbox is created.
    */
    
    return 0 ;
}

int create_shmem(int id, int max_mailboxes)
{
    return 0 ;
}

int cleanup_semaphores(int id)
{
    return 0 ;
}

extern int mailbox_create(int id, int mailboxid)
{
    int current_mailbox_count ;
    int configured_mailbox_count ;

    lock_shmem_meta(id) ;

    int current_mailbox_count = get_mailbox_count(id) ;
    int configured_mailbox_count = get_configured_mailbox_count(id) ;

    if ((current_mailbox_count + 1) > configured_mailbox_count)
    {
	unlock_shmem_meta(id) ;
	return -1 ;
    }

    increment_mailbox_count(id) ;
    unlock_shmem_meta(id) ;

    init_shmem_mailbox(id, mailboxid) ;

    return 0 ;
}

int lock_shmem_meta(int id)
{
}

int unlock_shmem_meta(int id)
{
}

int get_mailbox_count(int id)
{
}

int get_configured_mailbox_count(int id)
{
}

int init_shmem_mailbox(int id, int mailboxid)
{
}

