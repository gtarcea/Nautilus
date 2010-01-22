
#ifndef __FILEBLOCKMAP_INCLUDE_
#define __FILEBLOCKMAP_INCLUDE_

#include "fileblock.h"
#include "next.h"

#ifdef __cplusplus
extern "C" {
#endif

struct FILEBLOCKMAP ;
typedef struct FILEBLOCKMAP FILEBLOCKMAP ;
typedef FILEBLOCKMAP *FILEBLOCKMAPP ;

FILEBLOCKMAPP fileblockmap_open(char *name, int create) ;
int fileblockmap_exists(char *name) ;
int fileblockmap_close(FILEBLOCKMAPP map, int save) ;
int fileblockmap_save(FILEBLOCKMAPP map) ;
int fileblockmap_delete(char *name) ;
FILEBLOCKP fileblockmap_addblock(FILEBLOCKMAPP map, int blocknum, int bytes) ;
int fileblockmap_removeblock(FILEBLOCKMAPP map, int blocknum) ;
int fileblockmap_updateblock(FILEBLOCKMAPP map, int blocknum, int newbytes) ;
int fileblockmap_getblockcount(FILEBLOCKMAPP map) ;
FILEBLOCKP fileblockmap_findblock(FILEBLOCKMAPP map, int blocknum) ;
FILEBLOCKP fileblockmap_getnextblock(FILEBLOCKMAPP, int next) ;
int fileblockmap_addhost(FILEBLOCKMAPP map, char *host) ;
int fileblockmap_removehost(FILEBLOCKMAPP map, char *host) ;
char *fileblockmap_getnexthost(FILEBLOCKMAPP map, int next) ;
int fileblockmap_findhost(FILEBLOCKMAPP map, char *host) ;
char *fileblockmap_getname(FILEBLOCKMAPP map) ;
int fileblockmap_getbytecount(FILEBLOCKMAPP map) ;

#ifdef __cplusplus
}
#endif

#endif /* __FILEBLOCKMAP_INCLUDE_ */

