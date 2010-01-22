
#include <sys/types.h>
#include "semaphore.h"

main()
{
	SEMAPHOREP sem ;
	int rc ;
	int i ;

	sem = semaphore_create(1234, 0) ;

	if (!sem) {
		printf("semaphore_create() failed\n") ;
		exit(1) ;
	}

	for (i = 0 ; i < 10; i++) {
		printf("Acquiring semaphore iteration %d\n", i) ;
		rc = semaphore_decrement(sem) ;
		if (! rc) {
			printf("semaphore_increment() failed\n") ;
			exit(1) ;
		}
	}

	printf("Removing semaphore from system\n") ;
	rc = semaphore_destroy(sem, 1) ;
	if (! rc) {
		printf("semaphore_destroy(sem, 1) failed\n") ;
		exit(1) ;
	}

	exit(0) ;
}
