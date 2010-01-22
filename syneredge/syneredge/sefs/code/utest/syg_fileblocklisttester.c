
#include "next.h"
#include "fileblock.h"
#include "fileblocklist.h"
#include <stdlib.h>
#include "tester.h"


static FILEBLOCKLISTP
create_fileblocklist(int numblocks)
{
	FILEBLOCKLISTP fblist ;

	fblist = fileblocklist_create(numblocks) ;

	return fblist ;
}

static int
destroy_fileblocklist(FILEBLOCKLISTP fblist)
{
	int rc ;

	rc = fileblocklist_destroy(fblist) ;

	return rc ;
}

static int
test_create_destroy()
{
	FILEBLOCKLISTP fblist ;

	fblist = create_fileblocklist(5) ;
	if (! fblist) {
		printf("fileblocklist_create() failed\n") ;
		return 0 ;
	}

	if (! destroy_fileblocklist(fblist)) {
		printf("fileblocklist_destroy() failed\n") ;
		return 0 ;
	}

	return 1 ;
}

static int
test_add_get_remove_hosts(FILEBLOCKLISTP fblist)
{
	int rc ;
	char *host ;

	rc = fileblocklist_addhost(fblist, "spelljammer") ;
	if (! rc) {
		printf("fileblocklist_addhost(spelljammer) failed\n") ;
		return 0 ;
	}

	rc = fileblocklist_addhost(fblist, "antigone") ;
	if (! rc) {
		printf("fileblocklist_addhost(antigone) failed\n") ;
		return 0 ;
	}

	rc = fileblocklist_addhost(fblist, "buford") ;
	if (! rc) {
		printf("fileblocklist_addhost(buford) failed\n") ;
		return 0 ;
	}

	/*
	** First host should be spelljammer
	*/
	host = fileblocklist_getnexthost(fblist, LSTART) ;
	if (! host) {
		printf("fileblocklist_getnexthost(LSTART) failed\n") ;
		return 0 ;
	}

	printf("host = '%s'\n", host) ;
	if ( strcmp(host, "spelljammer")) {
		printf("host doesnot equal spelljammer\n") ;
		return 0 ;
	}

	/*
	** Remove this host and then try and retrieve.
	*/
	rc = fileblocklist_removehost(fblist, "spelljammer") ;
	if (! rc) {
		printf("fileblocklist_removehost(spelljammer) failed\n") ;
		return 0 ;
	}

	host = fileblocklist_getnexthost(fblist, LSTART) ;
	if (! host) {
		printf("fileblocklist_getnexthost(LSTART) failed\n") ;
		return 0 ;
	}

	printf("host = '%s'\n", host) ;
	printf("address for host = %ul\n", host) ;
	if ( !strcmp(host, "spelljammer")) {
		printf("host equals spelljammer, but spelljammer was deleted\n") ;
		return 0 ;
	}

	return 1 ;
}

static int
test_add_get_remove_blocks(FILEBLOCKLISTP fblist)
{
	int rc ;
	char *host ;
	FILEBLOCKP fblock1, fblock2, fblock3, fblock1dup ;
	FILEBLOCKP fblock ;
	int blocknum ;

	fblock1 = fileblock_create(1,1) ;
	fblock1dup = fileblock_create(1,1) ;
	fblock2 = fileblock_create(2,2) ;
	fblock3 = fileblock_create(3,3) ;

	printf("fblock1 = %ul\n", fblock1) ;
	rc = fileblocklist_addblock(fblist, fblock1, 0) ;
	if (! rc) {
		printf("fileblocklist_addblock(block1) failed\n") ;
		return 0 ;
	}

	printf("fblock2 = %ul\n", fblock2) ;
	rc = fileblocklist_addblock(fblist, fblock2, 0) ;
	if (! rc) {
		printf("fileblocklist_addblock(fblock2) failed\n") ;
		return 0 ;
	}

	printf("fblock3 = %ul\n", fblock3) ;
	rc = fileblocklist_addblock(fblist, fblock3, 0) ;
	if (! rc) {
		printf("fileblocklist_addblock(fblock3) failed\n") ;
		return 0 ;
	}

	/*
	** Try to add dup, this should return 0
	*/
	rc = fileblocklist_addblock(fblist, fblock1dup, 1) ;
	if (rc) {
		printf("fileblocklist_addhost(fblock1dup) succeeded, should have failed (no dups allowed flag set)\n") ;
		return 0 ;
	} 

	/*
	** First block should be blocknum 1
	*/
	fblock = fileblocklist_getnextfileblock(fblist, LSTART) ;
	if (! fblock) {
		printf("fileblocklist_getnextfileblock(LSTART) failed\n") ;
		return 0 ;
	}

	printf("fblock = %ul\n", fblock) ;
	blocknum = fileblock_getblocknum(fblock) ;
	printf("blocknum = '%d'\n", blocknum) ;
	if ( 1 != blocknum ) {
		printf("blocknum doesnot equal 1\n") ;
		return 0 ;
	}

	/*
	** Remove this block and then try and retrieve.
	*/
	rc = fileblocklist_removeblock(fblist, 1) ;
	if (! rc) {
		printf("fileblocklist_removeblock(1) failed\n") ;
		return 0 ;
	}

	fblock = fileblocklist_getnextfileblock(fblist, LSTART) ;
	if (! fblock) {
		printf("fileblocklist_getnexthost(LSTART) failed\n") ;
		return 0 ;
	}

	blocknum = fileblock_getblocknum(fblock) ;
	printf("blocknum = '%d'\n", blocknum) ;
	if ( 1 == blocknum) {
		printf("block equals 1, but block 1 was deleted\n") ;
		return 0 ;
	}

	return 1 ;
}
static int
test_get_block_info_funcs(FILEBLOCKLISTP fblist)
{
	int value ;

	/*
	** If you modify test_add_get_remove_hosts(fblist) 
	** then you will need to modify the values this function
	** checks for.
	*/

	return 1 ;
}

static int
test_add_duplicate_hosts(FILEBLOCKLISTP fblist)
{
	int rc ;
	int count = 0 ;
	char *host ;

	rc = fileblocklist_addhost(fblist, "duphost") ;

	if (! rc) {
		printf("fileblocklist_addhost(duphost) failed\n") ;
		return 0 ;
	}
	
	/*
	** Add it again.
	*/
	rc = fileblocklist_addhost(fblist, "duphost") ;
	if (! rc) {
		printf("fileblocklist_addhost(duphost) failed on second add\n") ;
		return 0 ;
	}

	host = fileblocklist_getnexthost(fblist, LSTART) ;
	while (1) {
		if (! host) {
			break ;
		}

		if (strcmp(host, "duphost") == 0) {
			count++ ;
		}
		host = fileblocklist_getnexthost(fblist, LNEXT) ;
	}

	if (1 == count) {
		return 1 ; /* Only saw dup once */
	}

	printf("Saw dup host more than once (%d) - not allowed\n", count) ;

	return 0 ;
}

main()
{
	int rc ;
	FILEBLOCKLISTP fblist ;

	P("test_create_destroy()") ;
	rc = test_create_destroy() ;
	S(rc, "test_create_destroy()") ;

	fblist = create_fileblocklist(5) ;

	P("test_add_get_remove_hosts()") ;
	rc = test_add_get_remove_hosts(fblist) ;
	S(rc, "test_add_get_remove_hosts()") ;

	P("test_add_get_remove_blocks()") ;
	rc = test_add_get_remove_blocks(fblist) ;
	S(rc, "test_add_get_remove_blocks()") ;

	P("test_add_duplicate_hosts()") ;
	rc = test_add_duplicate_hosts(fblist) ;
	S(rc, "test_add_duplicate_hosts()") ;

	exit(0) ;
}
