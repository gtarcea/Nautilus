
#include "fileblockmapdb.h"
#include <stdio.h>
#include <sys/types.h>
#include <dirent.h>
#include <unistd.h>

struct FILEBLOCKMAPDB {
	char *db ;
} ;

static void
set_path(char *path, char *dbpath, char *filename)
{
	sprintf(path, "%s/%s", dbpath, filename) ;
}

FILEBLOCKMAPDBP 
fileblockmapdb_open(char *name, int create)
{
	FILEBLOCKMAPDBP mapdb ;
	char command[1024] ;

	if (access(name, F_OK) != 0) {
		/*
		** DB Path doesn't exist.
		*/
		if (create) {
			sprintf(command, "mkdir -p %s > /dev/null 2>&1", name) ;
			system(command) ;
			if (access(name, F_OK) != 0) {
				/*
				** mkdir failed.
				*/
				return NULL ;
			}
		} else {
			return NULL ;
		}
	}

	mapdb = malloc(sizeof(FILEBLOCKMAPDB)) ;

	if (! mapdb) {
		return NULL ;
	}

	mapdb->db = strdup(name) ;

	return mapdb ;
}

int 
fileblockmapdb_removefileblockmap(FILEBLOCKMAPDBP mapdb, char *name)
{
	int rc ;
	char path[1024] ;

	if (! mapdb) {
		return 0 ;
	}

	set_path(path, mapdb->db, name) ;

	rc = fileblockmap_delete(path) ;

	return rc ;
}

int 
fileblockmapdb_close(FILEBLOCKMAPDBP mapdb)
{
	if (! mapdb) {
		return 0 ;
	}

	free(mapdb->db) ;
	free(mapdb) ;

	return 1 ;
}

FILEBLOCKMAPP
fileblockmapdb_findfileblockmap(FILEBLOCKMAPDBP mapdb, char *name)
{
	FILEBLOCKMAPP map ;
	char path[1024] ;

	if (! mapdb) {
		return NULL ;
	}

	set_path(path, mapdb->db, name) ;

	map = fileblockmap_open(path, 0) ;

	if (! map) {
		return NULL ;
	}

	return map ;
}

char *
fileblockmapdb_getdbpath(FILEBLOCKMAPDBP mapdb)
{
	if (! mapdb) {
		return NULL ;
	}

	return mapdb->db ;
}


FILEBLOCKMAPP
fileblockmapdb_openfileblockmap(FILEBLOCKMAPDBP mapdb, char *filename, int create)
{
	char path[1024] ;

	if (!mapdb) {
		return NULL ;
	}

	set_path(path, mapdb->db, filename) ;

	return fileblockmap_open(path, create) ;
}

int fileblockmapdb_fileblockmapexists(FILEBLOCKMAPDBP mapdb, char *name)
{
	char path[1024] ;

	if (!mapdb) {
		return 0 ;
	}

	set_path(path, mapdb->db, name) ;

	return fileblockmap_exists(path) ;
}
