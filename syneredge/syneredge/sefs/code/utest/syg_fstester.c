
#include "tester.h"

static int
Setup()
{
}

static int
restart_blockd()
{
	system("/filesafe/stopfs") ;
	system("/filesafe/blockd/synwrblockd") ;
	/*printf("please start blockd\n") ;
	sleep(10) ;*/
}

void run_rw_tests() ;
void run_fcreate_tests() ;


main()
{
	if (getuid()) {
		printf("synfstester unit test must be run as root\n") ;
		exit(1) ;
	}

	Setup() ;

	system("rm -f /tmp/syneredgeblocks1*") ;
	synfs_init("abc", "abc", 0, 0) ;

	run_fcreate_tests() ;

	run_rw_tests() ;

	synfs_shutdown(0) ;

	exit(0) ;
}
