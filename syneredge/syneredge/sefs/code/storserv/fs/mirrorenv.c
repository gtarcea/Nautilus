
#include "mirrorenv.h"
#include "arraylist.h"
#include "next.h"
#include <stdlib.h>
#include <stdio.h>

struct MIRRORENV {
	char *configfile ;
	ARRAYLISTP mlist ;
} ;

static int
mfreefunc(void *item)
{
	MIRRORENTRYP m ;

	m = (MIRRORENTRYP) item ;

	if (item) {
		free(m->file) ;
		free(m->host) ;
		free(m) ;
	}

	return 1 ;
}

static int
mcomparefunc(void *a, void *b)
{
	return 0 ;
}

static int
mfindfunc(void *mirror, void *mirror2)
{
	return 0 ;
}

static int
read_mconfig_file(ARRAYLISTP mlist, char *configfile)
{
	FILE *fp ;
	char buf[1024] ;
	char *bcount ;
	char *bsize ;
	char f[1024] ; /* file */
	char h[1024] ; /* host */
	int fieldcount ;
	MIRRORENTRYP mentry ;

	fp = fopen(configfile, "r") ;

	if (!fp) {
		return 0 ;
	}

	while ( fgets(buf, 1024, fp) != NULL) {
		/*
		** File format:
		** Host file blocksize blockcount
		** Example:
		** myhost /usr/space1/mirror
		*/
		fieldcount = sscanf(buf, "%s %s %d %d", h, f, &bsize, &bcount) ;
		if (fieldcount == 4) {
			mentry = malloc(sizeof(MIRRORENTRY)) ;
			mentry->host = strdup(h) ;
			mentry->file = strdup(f) ;
			mentry->blocksize = bsize ;
			mentry->blockcount = bcount ;
			arraylist_additem(mlist, (void *) mentry) ;
		}
	}

	return 1 ;
}

MIRRORENVP 
mirrorenv_open(char *basedir)
{
	MIRRORENVP mdb ;
	char configfile[1024] ;
	int rc ;

	mdb = malloc(sizeof(MIRRORENV)) ;
	if (! mdb) {
		return NULL ;
	}

	mdb->mlist = arraylist_create(5) ;
	if (! mdb->mlist) {
		free(mdb) ;
		return NULL ;
	}

	sprintf(configfile, "%s/mirrors", basedir) ;
	mdb->configfile = strdup(configfile) ;

	rc = arraylist_setcomparefunc(mdb->mlist, mcomparefunc) ;
	rc = arraylist_setitemfreefunc(mdb->mlist, mfreefunc) ;
	rc = arraylist_setitemfindfunc(mdb->mlist, mfindfunc) ;

	rc = read_mconfig_file(mdb->mlist, mdb->configfile) ;

	if (! rc) {
		arraylist_destroy(mdb->mlist) ;
		free(mdb->configfile) ;
		free(mdb) ;
		return NULL ;
	}

	return mdb ;
}

MIRRORENTRYP 
mirrorenv_getnextmirror(MIRRORENVP mdb, int next)
{
	if (mdb) {
		return (MIRRORENTRYP) arraylist_getnextitem(mdb->mlist, next) ;
	}

	return NULL ;
}

int 
mirrorenv_destroy(MIRRORENVP mdb)
{
	int rc ;

	if (mdb) {
		rc = arraylist_destroy(mdb->mlist) ;
		free(mdb->configfile) ;
		free(mdb) ;
		return 1 ;
	}

	return 0 ;
}
