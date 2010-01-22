
#include "fileblock.h"
#include <stdlib.h>

struct FILEBLOCK {
	int blocknum ;
	int bytesused ;
} ;

FILEBLOCKP 
fileblock_create(int blocknum, int bytecount)
{
	FILEBLOCKP fb ;
	int rc ;
	char *hostcopy ;

	fb = malloc(sizeof(FILEBLOCK)) ;

	if (! fb) {
		return NULL ;
	}

	fb->blocknum = blocknum ;
	fb->bytesused = bytecount ;

	return fb ;
}

int 
fileblock_destroy(FILEBLOCKP fblock)
{
	if (fblock) {
		free(fblock) ;
		return 1 ;
	}

	return 0 ;
}

int 
fileblock_getbytesused(FILEBLOCKP fblock)
{
	if (fblock) {
		return fblock->bytesused ;
	}

	return -1 ;
}

int
fileblock_getblocknum(FILEBLOCKP fblock)
{
	if (fblock) {
		return fblock->blocknum ;
	}

	return -1 ;
}

int
fileblock_setbytesused(FILEBLOCKP fblock, int bytesused)
{
	if (fblock) {
		if ((bytesused >= 0) && (bytesused <= 1024)) {
			fblock->bytesused = bytesused ;
			return 1 ;
		}
	}

	return 0 ;
}
