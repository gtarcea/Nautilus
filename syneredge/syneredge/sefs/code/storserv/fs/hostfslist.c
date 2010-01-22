
#include "hostfslist.h"
#include "arraylist.h"
#include "stdlib.h"

struct HOSTFSLIST {
	ARRAYLISTP hostfslist ;
} ;

static int
free_hostfsentry(HOSTFSENTRY *entry)
{
	if (entry) {
		free(entry->host) ;
		free(entry->fs) ;
		free(entry) ;
	}

	return 1 ;
}

static int
hostfscompare(void *a, void *b)
{
	HOSTFSENTRY *entrya, *entryb ;

	if (a && b) {
		entrya = (HOSTFSENTRY *) a ;
		entryb = (HOSTFSENTRY *) b ;

		if (strcmp(entrya->host, entryb->host) != 0) {
			return 0 ;
		}

		if (strcmp(entrya->fs, entryb->fs) != 0) {
			return 0 ;
		}

		return 1 ;
	}

	return 0 ;
}

static int
hostfsfree(void *item)
{
	HOSTFSENTRY *entry ;

	if (item) {
		entry = (HOSTFSENTRY *) item ;
		free_hostfsentry(entry) ;
	}

	return 1 ;
}

static int
hostfsfindfunc(void *item, void *what)
{
	return hostfscompare(item, what) ;
}

static HOSTFSENTRY *
create_hostfsentry(char *host, char *fs)
{
	HOSTFSENTRY *entry ;

	if (host && fs) {
		entry = malloc(sizeof(HOSTFSENTRY)) ;

		if (! entry) {
			return NULL ;
		}

		entry->host = strdup(host) ;
		entry->fs = strdup(fs) ;

		return entry ;
	}

	return NULL ;
}

HOSTFSLISTP 
hostfslist_create(char *host, char *fs)
{
	int rc ;
	HOSTFSLISTP list ;
	HOSTFSENTRY *entry ;

	if (!host) {
		return NULL ;
	}

	if (! fs) {
		return NULL ;
	}

	list = malloc(sizeof(HOSTFSLIST)) ;

	if (! list) {
		return NULL ;
	}

	list->hostfslist = arraylist_create(5) ;

	if (! list->hostfslist) {
		free(list) ;
		return NULL ;
	}

	rc = arraylist_setcomparefunc(list->hostfslist, hostfscompare) ;
	rc = arraylist_setitemfreefunc(list->hostfslist, hostfsfree) ;
	rc = arraylist_setitemfindfunc(list->hostfslist, hostfsfindfunc) ;
	
	entry = create_hostfsentry(host, fs) ;

	rc = arraylist_additem(list->hostfslist, (void *) entry) ;

	return list ;
}

int 
hostfslist_destroy(HOSTFSLISTP list)
{
	int rc ;

	if (! list) {
		return 0 ;
	}

	rc = arraylist_destroy(list->hostfslist) ;

	if (! rc) {
		/* What to do? */
	}

	free(list) ;

	return 1 ;
}

int 
hostfslist_addhostfs(HOSTFSLISTP list, char *host, char *fs)
{
	int rc ;
	HOSTFSENTRY *entry ;

	if (! list) {
		return 0 ;
	}

	if (hostfslist_findhostfsentry(list, host, fs)) {
		/*
		** Already in list, no need to add again.
		*/
		return 1 ;
	}

	/*
	** Otherwise, not in list, so add.
	*/

	entry = create_hostfsentry(host, fs) ;

	if (! entry) {
		return 0 ;
	}

	rc = arraylist_additem(list->hostfslist, entry) ;

	if (! rc) {
		free_hostfsentry(entry) ;
		return 0 ;
	}

	return 1 ;
}

int 
hostfslist_removehostfs(HOSTFSLISTP list, char *host, char *fs)
{
	HOSTFSENTRY *entry ;
	int rc ;

	if (! list) {
		return 0 ;
	}

	entry = create_hostfsentry(host, fs) ;

	if (! entry) {
		return 0 ;
	}

	rc = arraylist_deleteitem(list->hostfslist, (void *) entry) ;

	free_hostfsentry(entry) ;

	return rc ;
}

int 
hostfslist_findhostfsentry(HOSTFSLISTP list, char *host, char *fs)
{
	HOSTFSENTRY *entry ;
	HOSTFSENTRY *foundentry ;
	int rc = 0 ;

	if (! list) {
		return 0 ;
	}

	entry = create_hostfsentry(host, fs) ;

	if (! entry) {
		return 0 ;
	}

	foundentry = arraylist_finditem(list->hostfslist, (void *) entry) ;

	if (foundentry) {
		rc = 1 ;
	}

	free_hostfsentry(entry) ;

	return rc ;
}

HOSTFSENTRY *
hostfslist_getnextentry(HOSTFSLISTP list, int next)
{
	HOSTFSENTRY *entry ;

	if (list) {
		entry = arraylist_getnextitem(list->hostfslist, next) ;
		return entry ;
	}

	return NULL ;
}
