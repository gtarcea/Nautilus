
#include "fileblock.h"
#include <stdlib.h>
#include "tester.h"


static FILEBLOCKP
create_fileblock(int blocknum, int bytes)
{
	FILEBLOCKP fblock ;

	fblock = fileblock_create(blocknum, bytes) ;

	return fblock ;
}

static int
destroy_fileblock(FILEBLOCKP fblock)
{
	int rc ;

	rc = fileblock_destroy(fblock) ;

	return rc ;
}

static int
test_create_destroy()
{
	FILEBLOCKP fblock ;

	fblock = create_fileblock(1, 2) ;
	if (! fblock) {
		printf("fileblock_create() failed\n") ;
		return 0 ;
	}

	if (! destroy_fileblock(fblock)) {
		printf("fileblock_destroy() failed\n") ;
		return 0 ;
	}

	return 1 ;
}

static int
test_get_block_info_funcs(FILEBLOCKP fblock)
{
	int value ;

	value = fileblock_getblocknum(fblock) ;
	printf("blocknum = %d\n", value) ;
	if (value != 1) {
		printf("fileblock_getblocknum() != 1\n") ;
		return 0 ;
	}

	value = fileblock_getbytesused(fblock) ;
	printf("bytesused = %d\n", value) ;
	if (value != 2) {
		printf("fileblock_getbytesused() != 2\n") ;
		return 0 ;
	}

	return 1 ;
}

static int
test_setbytesused(FILEBLOCKP fblock)
{
	int rc ;

	rc = fileblock_setbytesused(fblock, -1) ;
	if (rc) {
		printf("fileblock_setbytesused(-1) allowed negative number\n") ;
		return 0 ;
	}

	rc = fileblock_setbytesused(fblock, 1025) ;
	if (rc) {
		printf("fileblock_setbytesused(1025) allowed greater than 1024\n") ;
		return 0 ;
	}

	rc = fileblock_setbytesused(fblock, 1024) ;
	if (! rc) {
		printf("fileblock_setbytesused(1024) failed\n") ;
		return 0 ;
	}

	rc = fileblock_setbytesused(fblock, 0) ;
	if (! rc) {
		printf("fileblock_setbytesused(0) failed\n") ;
		return 0 ;
	}

	rc = fileblock_setbytesused(fblock, 10) ;
	if (! rc) {
		printf("fileblock_setbytesused(10) failed\n") ;
		return 0 ;
	}

	rc = fileblock_setbytesused(NULL, 15) ;
	if (rc) {
		printf("fileblock_setbytesused(NULL, 15) didn't check for NULL\n") ;
		return 0 ;
	}

	return 1 ;
}

main()
{
	int rc ;
	FILEBLOCKP fblock ;

	P("test_create_destroy()") ;
	rc = test_create_destroy() ;
	S(rc, "test_create_destroy()") ;

	fblock = create_fileblock(1, 2) ;

	P("test_get_block_info_funcs()") ;
	rc = test_get_block_info_funcs(fblock) ;
	S(rc, "test_get_block_info_funcs()") ;

	P("test_setbytesused()") ;
	rc = test_setbytesused(fblock) ;
	S(rc, "test_setbytesused()") ;

	exit(0) ;
}
