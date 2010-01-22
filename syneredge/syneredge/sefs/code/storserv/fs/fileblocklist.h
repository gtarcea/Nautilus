
#ifndef __FILEBLOCKLIST_INCLUDE_
#define __FILEBLOCKLIST_INCLUDE_

#include "fileblock.h"

#ifdef __cplusplus
extern "C" {
#endif

struct FILEBLOCKLIST ;
typedef struct FILEBLOCKLIST FILEBLOCKLIST ;
typedef FILEBLOCKLIST *FILEBLOCKLISTP ;

FILEBLOCKLISTP fileblocklist_create(int numblocks) ;
int fileblocklist_destroy(FILEBLOCKLISTP fblist) ;
int fileblocklist_addblock(FILEBLOCKLISTP fblist, FILEBLOCKP fblock, int nodups) ;
int fileblocklist_removeblock(FILEBLOCKLISTP fblist, int blocknum) ;
FILEBLOCKP fileblocklist_getfileblock(FILEBLOCKLISTP fblist, int blocknum) ;
FILEBLOCKP fileblocklist_getnextfileblock(FILEBLOCKLISTP fblist, int next) ;
int fileblocklist_getblocklistcount(FILEBLOCKLISTP fblist) ;
int fileblocklist_addhost(FILEBLOCKLISTP fblist, char *host) ;
int fileblocklist_removehost(FILEBLOCKLISTP fblist, char *host) ;
int fileblocklist_hostexists(FILEBLOCKLISTP fblist, char *host) ;
int fileblocklist_gethostcount(FILEBLOCKLISTP fblist) ;
char *fileblocklist_getnexthost(FILEBLOCKLISTP fblist, int next) ;

#ifdef __cplusplus
}
#endif

#endif /* __FILEBLOCKLIST_INCLUDE_ */
