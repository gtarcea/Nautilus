
#include <sys/types.h>
#include "semaphore.h"

main()
{
	SEMAPHOREP sem ;
	int rc ;
	int i ;

	sem = semaphore_open(1234) ;

	if (!sem) {
		printf("semaphore_open() failed\n") ;
		exit(1) ;
	}

	for (i = 0; i < 10; i++) {
		printf("Releasing semaphore iteration %d\n", i) ;
		rc = semaphore_increment(sem) ;
		if (! rc) {
			printf("semaphore_increment() failed\n") ;
			exit(1) ;
		}
		sleep(1) ;
	}

	rc = semaphore_destroy(sem, 0) ;
	if (! rc) {
		printf("semaphore_destroy(sem, 0) failed\n") ;
		exit(1) ;
	}

	exit(0) ;
}
