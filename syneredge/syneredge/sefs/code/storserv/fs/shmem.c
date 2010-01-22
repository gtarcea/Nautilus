
#include "shmem.h"
#include <sys/ipc.h>
#include <sys/shm.h>
#include <errno.h>

int shmemid = -1 ;
void *addr_space = (void *) -1 ;

#define TEN_MEG 1024*1024*10

#define SHMEM_KEY 0x0001e240

static int 
shmem_exists()
{
	return (shmemid != -1) ;
}

int 
shmem_open()
{
	if (! shmem_exists() ) {
		shmemid = shmget(SHMEM_KEY,TEN_MEG,IPC_CREAT) ;
		if ( -1 == shmemid ) {
			return 0 ;
		}
	} else {
		/* shared memory already exists */
	}


	if ( (void *) -1 == addr_space ) {
		addr_space = shmat(shmemid, 0, 0) ;
		if ( (void *) -1 == addr_space ) {
			return 0 ;
		}
	}

	return 1 ;
}

int 
shmem_close(int remove)
{
	int rc ;

	rc = shmdt(addr_space) ;

	addr_space = (void *) -1 ;

	if ( -1 == rc ) {
		return 0 ;
	}

	if (remove) {
		rc = shmctl(shmemid, IPC_RMID, 0) ;
		if ( -1 == rc ) {
			return 0 ;
		}
	}

	shmemid = -1 ;

	return 1 ;
}

void *
shmem_get_memptr()
{
	if ((void *) -1 != addr_space) {
		return addr_space ;
	}

	return (void *) -1 ;
}

