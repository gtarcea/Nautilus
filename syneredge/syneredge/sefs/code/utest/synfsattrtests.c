
#include "synfs.h"
#include "tester.h"
#include "fileblockmap.h"
#include "fileblock.h"
#include "next.h"
#include "errno.h"

static int
test_getattr_on_nonexisting_file()
{
	return 0 ;
}

test_getattr_on_existing_file()
{
	return 0 ;
}

static int
run_getattr_tests()
{
	int rc ;

	P("test_getattr_on_nonexisting_file()") ;
	rc = test_getattr_on_nonexisting_file() ;
	S(rc, "test_getattr_on_nonexisting_file()") ;

	P("test_getattr_on_existing_file()") ;
	rc = test_getattr_on_existing_file() ;
	S(rc, "test_getattr_on_existing_file()") ;
}

static int
run_statfs_tests()
{
	exit(0) ;
}

static int
Setup_Attr_Tests()
{
	/*
	system("rm -f /etc/syneredge/fbmap/*") ;
	system("rm -f /tmp/syneredgeblocks/*") ;
	system("rm -f /tmp/syneredgeblocks2/*") ;
	*/
	return 1 ;
}

void
run_attr_tests()
{
	Setup_Attr_Tests() ;
	run_getattr_tests() ;
	run_statfs_tests() ;
	/*
	** When we are ready to implement these.
	**
	** run_xattr_tests() ;
	*/
}
