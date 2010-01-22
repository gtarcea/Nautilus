
#include "shmem.h"
#include "tester.h"


static int
test_open_get_close()
{
	int rc ;

	printf("Calling shmem_open()\n") ;
	rc = shmem_open() ;
	if (! rc) {
		printf("shmem_open() failed\n") ;
		return 0 ;
	}

	system("ipcs -a") ;

	printf("Calling shmem_get_memptr()\n") ;
	printf( "%lu\n", shmem_get_memptr()) ;

	printf("Calling shmem_close(1)\n") ;
	rc = shmem_close(1) ;
	if (!rc) {
		printf("shmem_close(1) failed\n") ;
		return 0 ;
	}

	system("ipcs -a") ;
	return 1 ;
}

static int
test_already_open_memory()
{
	int rc ;

	rc = shmem_open() ;

	if (-1 == rc) {
		printf("shmem_open() failed\n") ;
		return 0 ;
	}

	rc = shmem_open() ;

	if (-1 == rc) {
		printf("Open of already open shmem failed\n") ;
		return 0 ;
	}

	rc = shmem_close(1) ;

	if (-1 == rc) {
		printf("close of already open shmem failed\n") ;
		return 0 ;
	}

	return 1 ;
}

main()
{
	int rc ;

	P("test_open_get_close()") ;
	rc = test_open_get_close() ;
	S(rc, "test_open_get_close()") ;

	P("test_already_open_memory()") ;
	rc = test_already_open_memory() ;
	S(rc, "test_already_open_memory()") ;

	exit(0) ;
}

