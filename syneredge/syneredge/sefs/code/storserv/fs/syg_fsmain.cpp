
#include "synfs.h"

extern "C" { int exit(int) ; }

main(int argc, char **argv)
{
	int rc ;

	rc = synfs_init("/etc/syneredge", "/dev/whatever", 1024, 100) ;

	if (! rc) {
		printf("SynergyFS Initialization failed\n") ;
		exit(1) ;
	}
/*	system("../blockd/synwrblockd") ;*/

	synfs_run(argc, argv) ;
	synfs_shutdown(0) ;
	exit(0) ;
}
