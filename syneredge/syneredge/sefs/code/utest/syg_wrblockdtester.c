
/*
** This program tests the synwrblockd
*/

#include "tester.h"
#include "blockdevice.h"

int
test_make_daemon()
{
	return 1 ;
}

int
test_start_as_no_daemon()
{
	return 1 ;
}

static int
init_data(char *data, int length)
{
	int i ;
	char c = 'a' ;

	for (i = 0 ; i < length; i++) {
		if ( 'z' == c ) {
			c = 'a' ;
		}
		data[i] = c ;
		c++ ;
	}
}

int
test_writing_blocks()
{
	BLOCKDEVICEP bdev ;
	char data[2048] ;
	int rc ;
	int i ;
	
	bdev = blockdevice_create("/dev/whatever", 1024, 9) ;

	if (! bdev) {
		printf("blockdevice_create() failed\n") ;
		return 0 ;
	}

	init_data(data, 2048) ;

	/*
	** Cleanout any old blocks
	*/
	system("utblocksclean.sh") ;

	/*
	** printf("Please Launch synwrblockd\n") ;
	** sleep(10) ;
	*/
	printf("Launching synwrblockd\n") ;
	system("synwrblockd") ;

	for (i = 0 ; i < 20 ; i++) {
		printf("Writing to block device block %d\n", i) ;
		rc = blockdevice_writeblock(bdev, data, 0, i*50, i, 1) ;
		if ( rc != (i*50) ) {
			printf("blockdevice_write() %d failed\n", i) ;
			blockdevice_close(bdev, 1) ;
			return 0 ;
		}
	}

	blockdevice_close(bdev, 1) ;

	return 1 ;
}

main(int argc, char **argv)
{
	int rc ;

	P("test_make_daemon()") ;
	rc = test_make_daemon() ;
	S(rc, "test_make_daemon()") ;

	P("test_start_as_no_daemon()") ;
	rc = test_start_as_no_daemon() ;
	S(rc, "test_start_as_no_daemon()") ;

	P("test_writing_blocks()") ;
	rc = test_writing_blocks() ;
	S(rc, "test_writing_blocks()") ;
	
	exit(0) ;
}
