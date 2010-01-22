
#include <stdio.h>
#include "blockdevice.h"
#include "tester.h"

static int
test_create_close()
{
	BLOCKDEVICEP devp ;

	printf("Calling blockdevice_create()\n") ;
	devp = blockdevice_create("/syneredge/bd1", 1024, 10) ;
	if ( devp == NULL ) {
		printf("blockdevice_create() failed\n") ;
		return 0 ;
	}
	printf("Past blockdevice_create()\n") ;

	printf("devp = %lu\n", devp) ;
	blockdevice_close(devp, 1) ;
	return 1 ;
}

static int
test_open_close()
{
	BLOCKDEVICEP devp ;
	BLOCKDEVICEP devp2 ;

	printf("Calling blockdevice_create()\n") ;
	devp = blockdevice_create("/syneredge/bd1", 1024, 10) ;
	if ( devp == NULL ) {
		printf("blockdevice_create() failed\n") ;
		return 0 ;
	}
	printf("Past blockdevice_create()\n") ;

	printf("Calling blockdevice_open()\n") ;
	devp2 = blockdevice_open("/syneredge/bd1") ;
	printf("Past blockdevice_open()") ;

	printf("devp blocksize = %d\n", blockdevice_getblocksize(devp2)) ;

	printf("devp = %lu\n", devp) ;
	printf("devp2 = %lu\n", devp2) ;
	blockdevice_close(devp, 1) ;
	blockdevice_close(devp2, 0) ;
	return 1 ;
}

static int
test_read_write()
{
	BLOCKDEVICEP devp ;
	char outdata[] = "1234" ;
	char indata[5] ;
	int rc ;

	devp = blockdevice_create("/syneredge/bd1", 1024, 10) ;
	if ( devp == NULL ) {
		printf("blockdevice_create() failed\n") ;
		return 0 ;
	}

	rc = blockdevice_writeblock(devp, (void *) outdata, 0, strlen(outdata)+1, 1, 0) ;
	if ( -1 == rc ) {
		printf("blockdevice_write() failed\n") ;
		return 0 ;
	}

	rc = blockdevice_readblock(devp, (void *) indata, 1) ;
	if ( -1 == rc ) {
		printf("blockdevice_read() failed\n") ;
		return 0 ;
	} else {
		printf( "Num bytes read = %d\n", rc) ;
	}

	if (strcmp(indata, outdata) ) {
		printf("blocks don't compare!\n") ;
		return 0 ;
	} else {
		printf("indata = '%s'/%d, outdata = '%s'/%d\n", indata, 
			strlen(indata), outdata, strlen(outdata)) ;
	}

	blockdevice_close(devp, 1) ;

	return 1 ;
}

int
test_get_next_dirty_block()
{
	BLOCKDEVICEP devp ;
	char outdata[] = "1234" ;
	char indata[5] ;
	BLOCK block ;
	int rc ;

	devp = blockdevice_create("/syneredge/bd1", 1024, 10) ;
	if ( devp == NULL ) {
		printf("blockdevice_create() failed\n") ;
		return 0 ;
	}

	rc = blockdevice_getnextdirtyblock(devp, &block, 0) ;

	if ( rc ) {
		printf("Found a block when we shouldn't have\n") ;
		return 0 ;
	}

	rc = blockdevice_writeblock(devp, (void *) outdata, 0, strlen(outdata)+1, 1, 0) ;
	if ( -1 == rc ) {
		printf("blockdevice_write() failed\n") ;
		return 0 ;
	}

	rc = blockdevice_getnextdirtyblock(devp, &block, 0) ;

	if ( ! rc ) {
		printf("Didn't find a block when we should have\n") ;
		return 0 ;
	}

	printf("block = '%s'/%d\n", block.data, block.datalength) ;

	rc = blockdevice_getnextdirtyblock(devp, &block, 0) ;

	if ( rc ) {
		printf("Found a block when we shouldn't have\n") ;
		return 0 ;
	}

	blockdevice_close(devp, 1) ;

	return 1 ;
}

main()
{
	int rc ;

	P("test_create_close()")
	rc = test_create_close() ;
	S(rc, "test_create_close()")

	P("test_read_write()")
	rc = test_read_write() ;
	S(rc, "test_read_write()")

	P("test_open_close()") ;
	rc = test_open_close() ;
	S(rc, "test_open_close()") ;

	P("test_get_next_dirty_block()") ;
	rc = test_get_next_dirty_block() ;
	S(rc, "test_get_next_dirty_block()") ;

	/*
	** Add boundary tests
	** writes of too big a block size, things like that...
	*/

	exit(0) ;
}

