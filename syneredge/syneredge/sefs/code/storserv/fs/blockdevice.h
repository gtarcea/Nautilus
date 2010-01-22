
#ifndef __BLOCKDEVICE_INCLUDE_
#define __BLOCKDEVICE_INCLUDE_

#ifdef __cplusplus
extern "C" {
#endif

struct BLOCKDEVICE ;
typedef struct BLOCKDEVICE BLOCKDEVICE;
typedef BLOCKDEVICE *BLOCKDEVICEP ;

#define BLOCK_DEV_MAX_NAME_LEN 64

#define REAL_BLOCK_LENGTH 1024

struct BLOCK {
	int datalength ;
	int blocknum ;
	int offset ;
	int status ;
	char data[REAL_BLOCK_LENGTH] ;
} ;

typedef struct BLOCK BLOCK ;

BLOCKDEVICEP blockdevice_create(char *name, int blocksize, int numblocks) ;
BLOCKDEVICEP blockdevice_open(char *name) ;
int blockdevice_close(BLOCKDEVICEP dev, int cleanup) ;
int blockdevice_writeblock(BLOCKDEVICEP dev, void *block, int offset, int size, 
			int blocknum, int waitforcomplete) ;
int blockdevice_readblock(BLOCKDEVICEP dev, void *block, int blocknum) ;
int blockdevice_getnextdirtyblock(BLOCKDEVICEP dev, BLOCK *block, int wait) ;
int blockdevice_getblocksize(BLOCKDEVICEP dev) ;
int blockdevice_signalblockdone(BLOCKDEVICEP dev, int blocknum) ;
int blockdevice_waitonstatus(BLOCKDEVICEP dev, int blocknum) ;
int blockdevice_lock(BLOCKDEVICEP dev) ;
int blockdevice_unlock(BLOCKDEVICEP dev) ;

int blockdevice_dump(BLOCKDEVICEP dev) ;

#ifdef __cplusplus
}
#endif

#endif /* __SHMEMBLOCK_INCLUDE_ */

