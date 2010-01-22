
#ifndef __FILEBLOCK_INCLUDE_
#define __FILEBLOCK_INCLUDE_

#include "next.h"

#ifdef __cplusplus
extern "C" {
#endif

struct FILEBLOCK ;

typedef struct FILEBLOCK FILEBLOCK ;
typedef FILEBLOCK *FILEBLOCKP ;

FILEBLOCKP fileblock_create(int blocknum, int bytecount) ;
int fileblock_destroy(FILEBLOCKP fblock) ;
int fileblock_getblocknum(FILEBLOCKP fblock) ;
int fileblock_getbytesused(FILEBLOCKP fblock) ;
int fileblock_setbytesused(FILEBLOCKP fblock, int bytesused) ;


#ifdef __cplusplus
}
#endif

#endif /* __FILEBLOCK_INCLUDE_ */
