
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <errno.h>
#include <fcntl.h>

#define SHMEMKEY 54333

int main()
{
    create_shmem() ;
    show_mem() ;
    /*shmem_stat() ;*/
}

int shmemid = -1 ;

int shmem_stat()
{
    int rc ;
    struct shmid_ds s ;

    if (shmemid != -1)
    {
        errno = 0 ;
        rc = shmctl(shmemid, IPC_STAT, &s) ;
        printf("shmctl rc = %d, errno = %d\n", rc, errno) ;
        if (rc >= 0)
        {
            printf(" shm_segsz = %d\n", s.shm_segsz) ;
        }
    }
}

int show_mem()
{
    unsigned char *p = shmat(shmemid, 0, 0) ;
    
    printf(" = %d\n", *p) ;
}

int create_shmem()
{
    int rc ;
    errno = 0 ;
    rc = shmget(SHMEMKEY, 131072, S_IRUSR | S_IWUSR | IPC_CREAT | IPC_EXCL) ;
    printf(" rc = %d, errno = %d\n", rc, errno) ;

    if (rc > 0)
    {
        shmemid = rc ;
    }
    else
    {
        printf("  shmem already existings, trying to connect to it\n") ;
        errno = 0 ;
        rc = shmget(SHMEMKEY, 131072, 0) ;
        printf("    rc = %d, errno = %d\n", rc, errno) ;
        if (rc > 0)
        {
            shmemid = rc ;
        }
        else
        {
            printf("Could not connect to shmem, exiting\n") ;
            exit(1) ;
        }
    }
}
