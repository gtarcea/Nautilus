
#include <sys/types.h>
#include "semaphore.h"
#include <sys/ipc.h>
#include <sys/sem.h>
#include <stdlib.h>

union semun {
	int val;		/* value for SETVAL */
	struct semid_ds *buf;	/* buffer for IPC_STAT, IPC_SET */
	unsigned short *array;	/* array for GETALL, SETALL */
		/* Linux specific part: */
	struct seminfo *__buf;	/* buffer for IPC_INFO */
};


struct SEMAPHORE {
	int semid ;
} ;

SEMAPHOREP 
semaphore_create(key_t key, int value)
{
	SEMAPHOREP sem ;
	union semun arg ;
	int rc ;

	sem = (SEMAPHOREP) malloc(sizeof(SEMAPHORE)) ;

	if (! sem) {
		return NULL ;
	}

	sem->semid = semget(key, 1, IPC_CREAT | 0666) ;

	if (sem->semid < 0) {
		free(sem) ;
		return NULL ;
	}

	arg.val = value ;

	rc = semctl(sem->semid, 0, SETVAL, arg) ;

	if (rc < 0) {
		free(sem) ;
		return NULL ;
	}

	return sem ;
}

SEMAPHOREP 
semaphore_open(key_t key)
{
	SEMAPHOREP sem ;

	sem = (SEMAPHOREP) malloc(sizeof(SEMAPHORE)) ;

	if (! sem) {
		return NULL ;
	}

	sem->semid = semget(key, 1, 0666) ;

	if (sem->semid < 0) {
		free(sem) ;
		return NULL ;
	}

	return sem ;
}

static int
do_sem_op_wait(int semid, int val)
{
	struct sembuf op[1] ;
	int rc ;

	op[0].sem_num = 0 ;
	op[0].sem_op = val ;
	op[0].sem_flg = 0 ;

	rc = semop(semid, op, 1) ;

	if (rc < 0) {
		return 0 ;
	}

	return 1 ;
}

int 
semaphore_increment(SEMAPHOREP sem)
{
	if (! sem) {
		return 0 ;
	}

	return do_sem_op_wait(sem->semid, 1) ;
}

int 
semaphore_decrement(SEMAPHOREP sem)
{
	if (! sem) {
		return 0 ;
	}

	return do_sem_op_wait(sem->semid, -1) ;
}

int
semaphore_getvalue(SEMAPHOREP sem)
{
	return -1 ;
}

int
semaphore_destroy(SEMAPHOREP sem, int removesys)
{
	int rc ;

	if (! sem) {
		return 0 ;
	}

	if (removesys) {
		
		rc = semctl(sem->semid, 0, IPC_RMID, 0) ;

		if (rc < 0) {
			return 0 ;
		}

	}

	free(sem) ;

	return 1 ;
}
