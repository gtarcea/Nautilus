
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>

int semid ;

#define SEMKEY 54322

/*
union semun
{
    int val ;
    struct semid_ds *buf ;
    ushort *array ;
} argument ;
*/

semun_t argument ;

struct sembuf operations[1] ;

main()
{
    pid_t pid ;

    argument.val = 1 ;

    create_semaphore() ;

    printf("value of semaphore = %d\n", semctl(semid, 0, GETVAL, argument)) ;

    return ;
    
    if ((pid = fork()))
    {
        /* parent */
        do_p() ;
    }
    else
    {
        /* child */
        create_semaphore() ;
        do_v() ;
    }
    
}

int initialize_semaphore()
{
    semctl(semid, 0, SETVAL, argument) ;
}

int create_semaphore()
{
    int rc ;

    errno = 0 ;
    rc = semget(SEMKEY, 1, S_IRUSR | S_IWUSR | IPC_CREAT | IPC_EXCL) ;
    printf("semget rc = %d, errno = %d\n", rc, errno) ;

    if (rc > 0)
    {
        semid = rc ;
        initialize_semaphore() ;
    }
    else
    {
        printf("sem already exists, trying to connect to it\n") ;
        errno = 0 ;
        rc = semget(SEMKEY, 0, 0) ;
        printf("  2nd semget rc = %d, errno = %d\n", rc, errno) ;
        if (rc < 0)
        {
            printf("Couldn't connect to sem, exiting...\n") ;
            exit(1) ;
        }

        semid = rc ;
    }
}

int V()
{
    operations[0].sem_num = 0 ;
    operations[0].sem_op = 1 ;
    operations[0].sem_flg = 0 ;
    printf("%d doing V()\n", getpid()) ;
    semop(semid, operations, 1) ;
    printf("%d finished V()\n", getpid()) ;
}

int P()
{
    operations[0].sem_num = 0 ;
    operations[0].sem_op = -1 ;
    operations[0].sem_flg = 0 ;
    printf("%d doing P()\n", getpid()) ;
    semop(semid, operations, 1) ;
    printf("%d finished P()\n", getpid()) ;
}

int do_p()
{
    int i ;
    for (i = 0 ; i < 3 ; i++)
    {
        P() ;
        sleep(2) ;
    }
}

int do_v()
{
    int i ;
    sleep(5) ;
    for (i = 0 ; i < 3 ; i++)
    {
        V() ;
        sleep(5) ;
    }
}
