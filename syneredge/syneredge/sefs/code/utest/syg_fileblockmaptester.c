
#include "fileblockmap.h"
#include "tester.h"
#include "next.h"

static char *
add_dir(char *name)
{
	static char fullpath[1024] ;

	sprintf(fullpath, "/etc/syneredge/fbmap/%s", name) ;

	return fullpath;
}

static FILEBLOCKMAPP
create_new_fbmap(char *name)
{
	FILEBLOCKMAPP fbmap ;

	fbmap = fileblockmap_open(add_dir(name), 1) ;

	if (! fbmap) {
		printf("fileblockmap_open() failed, Fatal!\n") ;
		exit(1) ;
	}

	return fbmap ;
}

static int
test_create_new()
{
	FILEBLOCKMAPP fbmap ;

	fbmap = fileblockmap_open(add_dir("testfile"), 1) ;

	if (! fbmap) {
		return 0 ;
	}

	fileblockmap_close(fbmap, 1) ;

	return 1 ;
}

static int
test_create_existing()
{
	FILEBLOCKMAPP fbmap ;

	fbmap = fileblockmap_open(add_dir("testfile"), 0) ;

	if (! fbmap) {
		return 0 ;
	}

	fileblockmap_close(fbmap, 1) ;
	return 1 ;
}

static int
test_add_remove_hosts(FILEBLOCKMAPP fbmap)
{
	int rc ;
	char *host ;

	rc = fileblockmap_addhost(fbmap, "spelljammer") ;

	if (! rc) {
		return 0 ;
	}

	host = fileblockmap_getnexthost(fbmap, LSTART) ;
	if (strcmp(host, "spelljammer") != 0) {
		printf("host != spelljammer (it should)\n") ;
		return 0 ;
	}

	rc = fileblockmap_removehost(fbmap, "spelljammer") ;

	if (! rc) {
		printf("fileblockmap_removehost(spelljammer) failed\n") ;
		return 0 ;
	}

	host = fileblockmap_getnexthost(fbmap, LSTART) ;
	if (host) {
		printf("A host was found '%s' (there should be no hosts)\n", host) ;
		return 0 ;
	}
	return 1 ;
}

static int
test_add_remove_blocks(FILEBLOCKMAPP fbmap)
{
	FILEBLOCKP fblock ;
	int rc ;
	int blocknum ;

	rc = fileblockmap_addblock(fbmap, 1, 1) ;

	if (! rc) {
		printf("fileblockmap_addblock() failed\n") ;
		return 0 ;
	}

	fblock = fileblockmap_getnextblock(fbmap, LSTART) ;

	if (! fblock) {
		printf("fileblocklist_getnextblock(LSTART) failed\n") ;
		return 0 ;
	}

	blocknum = fileblock_getblocknum(fblock) ;

	if (1 != blocknum) {
		printf("blocknum != 1 (it should) %d\n", blocknum) ;
		return 0 ;
	}

	rc = fileblockmap_removeblock(fbmap, 1) ;

	if (! rc) {
		printf("fileblockmap_removeblock(1) failed\n") ;
		return 0 ;
	}

	fblock = fileblockmap_getnextblock(fbmap, LSTART) ;

	if (fblock) {
		printf("fileblocklist_getnextblock(LSTART) found block (it was removed\n") ;
		return 0 ;
	}

	return 1 ;
}

static int
test_save_new(FILEBLOCKMAPP fbmap)
{
	int rc ;

	rc = fileblockmap_addblock(fbmap, 1, 1) ;

	if (! rc) {
		printf("fileblockmap_addblock() failed\n") ;
		return 0 ;
	}

	rc = fileblockmap_addhost(fbmap, "spelljammer") ;

	if (! rc) {
		return 0 ;
	}

	rc = fileblockmap_close(fbmap, 1) ;

	if (! rc) {
		return 0 ;
	}

	return 1 ;
}

static int
test_delete_nonexisting()
{
	int rc ;

	rc = fileblockmap_delete(add_dir("nosuchfile")) ;

	if (rc) {
		printf("fileblockmap_delete(nosuchfile) succeeded and shouldn't have\n") ;
		return 0 ;
	}

	return 1 ;
}

static int
test_delete_existing()
{
	FILEBLOCKMAPP fbmap ;
	int rc ;
	char *filename = "testdeleteexisting" ;

	fbmap = create_new_fbmap(filename) ;
	if (! fbmap) {
		printf("Unable to create blockmap %s\n", filename) ;
		return 0 ;
	}

	rc = fileblockmap_close(fbmap, 1) ;
	if (! rc) {
		printf("fileblockmap_close() failed\n") ;
		return 0 ;
	}


	rc = fileblockmap_delete(add_dir(filename)) ;
	if (!rc) {
		printf("fileblockmap_delete(%s) failed\n", filename) ;
		return 0 ;
	}

	rc = fileblockmap_exists(add_dir(filename)) ;
	if (rc) {
		printf("fileblockmap_exists(%s) found file, it shouldn't have!\n", filename) ;
		return 0 ;
	}
}

static int
test_blockcount()
{
	FILEBLOCKMAPP fbmap ;
	int rc ;
	int count ;

	fbmap = create_new_fbmap("blockcounttest") ;
	rc = fileblockmap_addblock(fbmap, 1, 1) ;

	count = fileblockmap_getblockcount(fbmap) ;
	if (count != 1) {
		printf("1 fileblockmap_getblockcount() != 1 (%d)\n", count) ;
		return 0 ;
	}

	rc = fileblockmap_addblock(fbmap, 2, 1) ;
	count = fileblockmap_getblockcount(fbmap) ;
	if (count != 2) {
		printf("2 fileblockmap_getblockcount() != 2 (%d)\n", count) ;
		return 0 ;
	}

	rc = fileblockmap_removeblock(fbmap, 1) ;
	count = fileblockmap_getblockcount(fbmap) ;
	if (count != 1) {
		printf("3 fileblockmap_getblockcount() != 1 (%d)\n", count) ;
		return 0 ;
	}

	fileblockmap_close(fbmap, 0) ;

	return 1 ;
}

main()
{
	int rc ;
	FILEBLOCKMAPP fbmap ;

	system("rm -f /etc/syneredge/fbmap/testfile") ;
	P("test_create_new()") ;
	rc = test_create_new() ;
	S(rc, "test_create_new()") ;

	P("test_create_existing()") ;
	rc = test_create_existing() ;
	S(rc, "test_create_existing()") ;

	system("rm -f /etc/syneredge/fbmap/mytestmap") ;
	fbmap = create_new_fbmap("mytestmap") ;

	P("test_add_remove_host()") ;
	rc = test_add_remove_hosts(fbmap) ;
	S(rc, "test_add_remove_host()") ;

	P("test_add_remove_blocks()") ;
	rc = test_add_remove_blocks(fbmap) ;
	S(rc, "test_add_remove_blocks()") ;

	P("test_save_new()") ;
	rc = test_save_new(fbmap) ;
	S(rc, "test_save_new()") ;

	P("test_blockcount()") ;
	rc = test_blockcount() ;
	S(rc, "test_blockcount()") ;

	system("rm -f /etc/syneredge/fbmap/*") ;

	P("test_delete_nonexisting()") ;
	rc = test_delete_nonexisting() ;
	S(rc, "test_delete_nonexisting()") ;

	P("test_delete_existing()") ;
	rc = test_delete_existing() ;
	S(rc, "test_delete_existing()") ;

	exit(0) ;
}

