
#include "fileblockmapdb.h"
#include "tester.h"
#include <stdio.h>

FILEBLOCKMAPDBP
open_mapdb()
{
	FILEBLOCKMAPDBP mapdb ;

	mapdb = fileblockmapdb_open("/etc/syneredge/fbmap", 1) ;

	if (! mapdb) {
		printf("fileblockmapdb_open(/etc/syneredge/fbmap) failed - FATAL\n") ;
		exit(1) ;
	}

	return mapdb ;
}

static int
test_open_close_db()
{
	int rc ;
	FILEBLOCKMAPDBP mapdb ;

	mapdb = fileblockmapdb_open("/etc/syneredge/fbmap", 1) ;
	if (! mapdb) {
		printf("fileblockmapdb_open(/etc/syneredge/fbmap) failed\n") ;
		return 0 ;
	}

	rc = fileblockmapdb_close(mapdb) ;

	if (! rc) {
		printf("fileblockmapdb_close() failed\n") ;
		return 0 ;
	}

	return 1 ;
}

FILEBLOCKMAPP
create_new_fileblockmap()
{
	FILEBLOCKMAPP map ;
	int rc ;

	map = fileblockmap_open("/etc/syneredge/fbmap/mapdbtestnewmap", 1) ;

	if (! map) {
		printf("fileblockmap_open(/etc/syneredge/fbmap/mapdbtestnewmap) failed\n") ;
		return NULL ;
	}

	rc = fileblockmap_addblock(map, 1, 2) ;
	rc = fileblockmap_addblock(map, 2, 2) ;

	rc = fileblockmap_addhost(map, "spelljammer") ;
	rc = fileblockmap_addhost(map, "buford") ;

	rc = fileblockmap_close(map, 1) ;

	return map ;
}

static int
test_delete_nonexisting_entry(FILEBLOCKMAPDBP mapdb)
{
	int rc ;
	char *filename = "nonexist" ;

	rc = fileblockmapdb_removefileblockmap(mapdb, filename) ;
	if (rc) {
		printf("fileblockmapdb_removefileblockmap(%s) succeeded and shouldn't have\n", filename) ;
		return 0 ;
	}

	return 1 ;
}

static int
test_delete_existing_entry(FILEBLOCKMAPDBP mapdb)
{
	/*
	** We know that mapdbtestnewmap exists so lets delete.
	*/
	char *filename = "mapdbtestnewmap" ;
	int rc ;
	FILEBLOCKMAPP fbmap ;

	rc = fileblockmapdb_removefileblockmap(mapdb, filename) ;
	if (! rc) {
		printf("fileblockmapdb_removefileblockmap(%s) failed\n", filename) ;
		return 0 ;
	}

	fbmap = fileblockmapdb_findfileblockmap(mapdb, filename) ;

	if (fbmap) {
		printf("fileblockmapdb_findfileblockmap(%s) succeeded and shouldn't have\n", filename) ;
		return 0 ;
	}

	return 1 ;
}

main()
{
	int rc ;
	FILEBLOCKMAPDBP mapdb ;

	system("rm -f /etc/syneredge/fbmap/mapdbtestnewmap") ;

	P("test_open_close_db()") ;
	rc = test_open_close_db() ;
	S(rc, "test_open_close_db()") ;

	create_new_fileblockmap() ;
	mapdb = open_mapdb() ;

	P("test_delete_nonexisting_entry()") ;
	rc = test_delete_nonexisting_entry(mapdb) ;
	S(rc, "test_delete_nonexisting_entry()") ;

	P("test_delete_existing_entry()") ;
	rc = test_delete_existing_entry(mapdb) ;
	S(rc, "test_delete_existing_entry()") ;

	fileblockmapdb_close(mapdb) ;

	exit(0) ;
}
