
#include "fileblocklist.h"
#include "arraylist.h"
#include "fileblock.h"
#include "next.h"
#include <stdlib.h>

struct FILEBLOCKLIST {
	int blockcnt ;
	ARRAYLISTP blocklist ;
	ARRAYLISTP hostlist ;
} ;

static int
hostfreefunc(void *item)
{
	if (item) {
		free((char *) item) ;
	}

	return 1 ;
}

static int
hostcomparefunc(void *a, void *b)
{
	if (a && b) {
		return (strcmp((char *) a, (char *) b) == 0) ;
	}

	return 0 ;
}

static int
hostfindfunc(void *hosta, void *hostb)
{
	if (hosta && hostb) {
		return (strcmp((char *) hosta, (char *) hostb) == 0);
	}

	return 0 ;
}

static int
fileblockcompare(void *a, void *b)
{
	FILEBLOCKP fb1 = (FILEBLOCKP) a ;
	FILEBLOCKP fb2 = (FILEBLOCKP) b ;
	int fb1blocknum, fb2blocknum ;

	if (fb1 && fb2) {
		fb1blocknum = fileblock_getblocknum(fb1) ;
		fb2blocknum = fileblock_getblocknum(fb2) ;
		return (fb1blocknum == fb2blocknum) ;
	}

	return 0 ;
}

static int
fileblockfindfunc(void *fblock, void *blocknum)
{
	FILEBLOCKP fb = (FILEBLOCKP) fblock ;
	int b ;
	int fbblocknum ;

	if (fb && blocknum) {
		b = *((int *) blocknum) ;
		fbblocknum = fileblock_getblocknum(fb) ;
		return (fbblocknum == b) ;
	}

	return 0 ;
}

static int
fileblockfree(void *block)
{
	FILEBLOCKP fb = (FILEBLOCKP) block ;

	return fileblock_destroy(fb) ;
}

FILEBLOCKLISTP 
fileblocklist_create(int numblocks)
{
	FILEBLOCKLISTP fblist ;
	int blockcount ;
	int rc ;

	fblist = malloc(sizeof(FILEBLOCKLIST)) ;
	if (! fblist) {
		return NULL ;
	}

	blockcount = (numblocks < 0 ? 10 : numblocks) ;
	fblist->blocklist = arraylist_create(blockcount) ;

	if (! fblist->blocklist) {
		free(fblist) ;
		return NULL ;
	}

	rc = arraylist_setcomparefunc(fblist->blocklist, fileblockcompare) ;
	rc = arraylist_setitemfreefunc(fblist->blocklist, fileblockfree) ;
	rc = arraylist_setitemfindfunc(fblist->blocklist, fileblockfindfunc) ;

	fblist->hostlist = arraylist_create(5) ;

	if (! fblist->hostlist) {
		arraylist_destroy(fblist->blocklist) ;
		free(fblist) ;
		return NULL ;
	}

	arraylist_setcomparefunc(fblist->hostlist, hostcomparefunc) ;
	arraylist_setitemfreefunc(fblist->hostlist, hostfreefunc) ;
	arraylist_setitemfindfunc(fblist->hostlist, hostfindfunc) ;

	return fblist ;
}

int 
fileblocklist_destroy(FILEBLOCKLISTP fblist)
{
	int rc ;

	if (! fblist) {
		return 0 ;
	}

	rc = arraylist_destroy(fblist->blocklist) ;

	if (! rc) {
		/* Hmmmm what to do? */
	}

	rc = arraylist_destroy(fblist->hostlist) ;

	if (! rc) {
		/* Hmmmm what to do? */
	}

	free(fblist) ;

	return 1 ;
}

int
fileblocklist_hostexists(FILEBLOCKLISTP fblist, char *host)
{
	char *hostfound ;

	if (fblist && host) {
		hostfound = arraylist_finditem(fblist->hostlist, (void *) host) ;
		if (hostfound) {
			return 1 ;
		}
	}

	return 0 ;
}

int
fileblocklist_addhost(FILEBLOCKLISTP fblist, char *host)
{
	char *hostcopy ;
	int rc ;
	char *hostfound ;

	if (fblist && host) {

		hostfound = arraylist_finditem(fblist->hostlist, (void *) host) ;

		if (hostfound) {
			/*
			** host already in list.
			*/
			return 1 ;
		} else {

			hostcopy = strdup(host) ;
			rc = arraylist_additem(fblist->hostlist, (void *) hostcopy) ;

			if (rc) {
				return 1 ;
			}
		}
	}

	return 0 ;
}

int
fileblocklist_removehost(FILEBLOCKLISTP fblist, char *host)
{
	int rc ;

	if (fblist && host) {

		rc = arraylist_deleteitem(fblist->hostlist, host) ;

		if (rc) {
			return 1 ;
		}
	}

	return 0 ;
}

char *
fileblocklist_getnexthost(FILEBLOCKLISTP fblist, int next)
{
	if (fblist) {
		return (char *) arraylist_getnextitem(fblist->hostlist, next) ;
	}

	return NULL ;
}

int
fileblocklist_gethostcount(FILEBLOCKLISTP fblist)
{
	if (fblist) {
		return arraylist_getitemcount(fblist->hostlist) ;
	}

	return -1 ;
}


int 
fileblocklist_addblock(FILEBLOCKLISTP fblist, FILEBLOCKP fblock, int nodups)
{
	int rc ;
	FILEBLOCKP fb ;
	int blocknum ;

	if (! fblist || ! fblock) {
		return 0 ;
	}

	/*
	** Should we check for duplicate blocks?
	*/
	if (nodups) {
		blocknum = fileblock_getblocknum(fblock) ;
		fb = arraylist_finditem(fblist->blocklist, (void *) &blocknum) ;

		if (fb) {
			return 0 ;
		} 
	}

	rc = arraylist_additem(fblist->blocklist, (void *) fblock) ;

	return rc ;
}

int 
fileblocklist_removeblock(FILEBLOCKLISTP fblist, int blocknum)
{
	int rc ;
	FILEBLOCKP fb ;

	if (fblist) {

		fb = (FILEBLOCKP) arraylist_finditem(fblist->blocklist, (void *) &blocknum) ;

		if (! fb) {
			return 0 ;
		}

		rc = arraylist_deletecurrentitem(fblist->blocklist) ;

		if (! rc) {
			return 0 ;
		}

		return 1 ;
	}

	return 0 ;
}

FILEBLOCKP 
fileblocklist_getfileblock(FILEBLOCKLISTP fblist, int blocknum)
{
	FILEBLOCKP fb ;

	if (fblist) {
		fb = (FILEBLOCKP) arraylist_finditem(fblist->blocklist, (void *) &blocknum) ;
		return fb ;
	}

	return NULL ;
}

FILEBLOCKP 
fileblocklist_getnextfileblock(FILEBLOCKLISTP fblist, int next)
{
	FILEBLOCKP fb ;
	if (fblist) {
		fb = (FILEBLOCKP) arraylist_getnextitem(fblist->blocklist, next) ;
		return fb ;
	}

	return NULL ;
}

int 
fileblocklist_getblocklistcount(FILEBLOCKLISTP fblist)
{
	if (fblist) {
		return arraylist_getitemcount(fblist->blocklist) ;
	}

	return -1 ;
}
