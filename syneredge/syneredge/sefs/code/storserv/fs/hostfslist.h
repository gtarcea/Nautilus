
#ifndef __HOSTFSLIST_INCLUDE_
#define __HOSTFSLIST_INCLUDE_

#ifdef __cplusplus
extern "C" {
#endif

#include "next.h"

struct HOSTFSLIST ;
typedef struct HOSTFSLIST HOSTFSLIST ;
typedef HOSTFSLIST *HOSTFSLISTP ;

struct HOSTFSENTRY {
	char *host ;
	char *fs ;
} ;

typedef struct HOSTFSENTRY HOSTFSENTRY ;

/*
	long f_blocks;   * total data blocks in file system *
	long f_bfree;    * free blocks in fs *
	long f_bavail;   * free blocks avail to non-superuser *
*/

HOSTFSLISTP hostfslist_create(char *host, char *fs) ;
int hostfslist_destroy(HOSTFSLISTP list) ;
int hostfslist_addhostfs(HOSTFSLISTP list, char *host, char *fs) ;
int hostfslist_removehostfs(HOSTFSLISTP list, char *host, char *fs) ;
int hostfslist_findhostfsentry(HOSTFSLISTP list, char *host, char *fs) ;
HOSTFSENTRY *hostfslist_getnextentry(HOSTFSLISTP list, int next) ;

#ifdef __cplusplus
}
#endif

#endif /* __HOSTFSLIST_INCLUDE_ */
