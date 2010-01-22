
#include "blockdevice.h"
#include "shmem.h"
#include <stdlib.h>
#include <sys/types.h>
#include "semaphore.h"

/*
**
** These routines create a shared memory structure that is treated as a block device.
** The shared memory is setup as follows:
** The first 1024 bytes are allocated to the meta data on the block device.
** The remainder is an array of blocks, each block is blocksize + header in size.
** The header contains the following information:
**    int datalength (if datalength = -1, then block is free)
**
** To add:
**   * Synchronization for multiple processes/threads
**   * Count on each block, so if multiple need to read/write it they can.
**
*/

#define REAL_BLOCK_LENGTH 1024

struct BLOCKDEVICE_HEADER {
	char name[BLOCK_DEV_MAX_NAME_LEN] ;
	int blocksize ;
	int numblocks ;
} ;

typedef struct BLOCKDEVICE_HEADER BLOCKDEVICE_HEADER ;

struct SHMEM_BLOCK_OVERLAY {
	BLOCKDEVICE_HEADER header ;
	BLOCK datablocks[] ;
} ;

typedef struct SHMEM_BLOCK_OVERLAY SHMEM_BLOCK_OVERLAY ;

struct BLOCKDEVICE {
	char *name ;
	SHMEM_BLOCK_OVERLAY *bd ; /* bd = block device */
	SEMAPHOREP sem ;	/* Currently just one global semaphore */
	SEMAPHOREP dirtysem ;   /* One global semaphore to mark blocks ready to write */
	int blocksize ;
	int numblocks ;
	int lastblockchecked ;
} ;

static int
initialize_block_dev_shmem(SHMEM_BLOCK_OVERLAY *bd, int blocksize, 
		int numblocks, char *name)
{
	int i ;

	strcpy(bd->header.name, name) ;
	bd->header.blocksize = blocksize ;
	bd->header.numblocks = numblocks ;

	/*
	** If we change blocksizes to variable or settable, we'll
	** need to fix this up.
	*/
	for (i = 0 ; i < numblocks; i++) {
		bd->datablocks[i].datalength = -1 ;
		bd->datablocks[i].blocknum = -1 ;
	}

	return 1 ;
}

BLOCKDEVICEP 
blockdevice_create(char *name, int blocksize, int numblocks)
{
	BLOCKDEVICEP devp ;
	int rc ;

	if (blocksize != REAL_BLOCK_LENGTH) {
		/*
		** For now we simplify things and just force
		** all blockdevices to have 1k block sizes.
		** We can fix this assumption up later.
		*/
		return NULL ;
	}

	if (strlen(name) >= BLOCK_DEV_MAX_NAME_LEN) {
		return NULL ;
	}

	devp = malloc(sizeof(BLOCKDEVICE)) ;

	if (NULL == devp) {
		return NULL ;
	}

	devp->name = malloc(strlen(name)+1) ;

	if (NULL == devp->name) {
		free(devp) ;
		return NULL ;
	}

	devp->sem = semaphore_create(1234, 0) ;
	devp->dirtysem = semaphore_create(1235, 0) ;

	/*if (! devp->sem) {
		free(devp) ;
		return NULL ;
	}*/

	strcpy(devp->name, name) ;
	devp->blocksize = blocksize ;
	devp->numblocks = numblocks ;
	devp->lastblockchecked = 0 ;

	rc = shmem_open() ;

	if (0 == rc) {
		semaphore_destroy(devp->sem, 1) ;
		free(devp->name) ;
		free(devp) ;
		return NULL ;
	}

	devp->bd = (struct SHMEM_BLOCK_OVERLAY *) shmem_get_memptr() ;

	initialize_block_dev_shmem(devp->bd, blocksize, numblocks, name) ;

	return devp ;
}

BLOCKDEVICEP 
blockdevice_open(char *name)
{
	BLOCKDEVICEP devp ;
	int rc ;

	/*
	** Shared memory has only been setup for 1 block device (currently).
	** This makes open really easy. We just attach to the shared memory
	** segment, suck the header information out, fill up our structure,
	** and away we go. Of course, there is absolutely no error checking
	** here (yet), we could run into the case where the shared memory
	** has not been created yet. We'll worry about this later. Iterate,
	** iterate, iterate.
	*/

	devp = malloc(sizeof(BLOCKDEVICE)) ;

	if (NULL == devp) {
		return NULL ;
	}

	devp->name = malloc(strlen(name)+1) ;

	if (NULL == devp->name) {
		free(devp) ;
		return NULL ;
	}

	devp->sem = semaphore_open(1234) ;

	if (! devp->sem) {
		free(devp) ;
		return NULL ;
	}

	devp->dirtysem = semaphore_open(1235) ;

	if (! devp->dirtysem) {
		free(devp) ;
		return NULL ;
	}

	rc = shmem_open() ;

	if (0 == rc) {
		semaphore_destroy(devp->sem, 0) ;
		semaphore_destroy(devp->dirtysem, 0) ;
		free(devp->name) ;
		free(devp) ;
		return NULL ;
	}

	devp->bd = (struct SHMEM_BLOCK_OVERLAY *) shmem_get_memptr() ;

	strcpy(devp->name, name) ;
	devp->blocksize = devp->bd->header.blocksize ;
	devp->numblocks = devp->bd->header.numblocks ;

	return devp ;
}

int 
blockdevice_close(BLOCKDEVICEP dev, int cleanup)
{
	int rc ;

	if (NULL == dev) {
		return 0 ;
	}

	rc = shmem_close(cleanup) ;
	rc = semaphore_destroy(dev->sem, cleanup) ;
	rc = semaphore_destroy(dev->dirtysem, cleanup) ;

	free(dev->name) ;
	free(dev) ;

	return rc ;
}

static int
find_free_block(BLOCK blocks[], int numblocks)
{
	int i ;

	/*
	** Stupidly simple search:
	** We look through the entire array of blocks looking for
	** a free one. If we don't find one, we sleep for 1 second
	** and we do it again. Assumption right now is that there
	** will eventually be a free block. BAD ASSUMPTION, but it
	** works for now.
	*/

	while ( 1 ) {
		for (i = 0 ; i < numblocks ; i++) {
			if (-1 == blocks[i].datalength) {
				return i ;
			}
		}
		/*
		** No free blocks found. Sleep and do it again
		*/
		sleep(1) ;
	}
}

int
blockdevice_waitonstatus(BLOCKDEVICEP dev, int blocknum)
{
	return 0 ;
}

int
blockdevice_signalblockdone(BLOCKDEVICEP dev, int blocknum)
{
	int rc ;

	if (! dev) {
		return 0 ;
	}

	rc = semaphore_increment(dev->sem) ;

	return 1 ;
}

int 
blockdevice_writeblock(BLOCKDEVICEP dev, void *block, int offset, int size, int blocknum, int waitforcomplete)
{
	int i ;
	int rc ;

	if (! dev) {
		/*printf("!dev\n") ;*/
		return -1 ;
	}

	if ((size < 0) || (offset < 0)) {
		/*printf("size < 1 %d, offset < 0 %d\n", size,offset) ;*/
		return -1 ;
	}

	if ( size > dev->blocksize ) {
		/*printf("size %d > dev->blocksize %d\n", size, dev->blocksize) ;*/
		return -1 ;
	}

	if (offset > size) {
		/*printf("offset %d > size %d\n", size, dev->blocksize) ;*/
		return -1 ;
	}

	i = find_free_block(dev->bd->datablocks, dev->numblocks) ;

	/*
	** Yay! We found a block, stuff data into it!
	*/
	dev->bd->datablocks[i].datalength = size ;
	dev->bd->datablocks[i].blocknum = blocknum ;
	dev->bd->datablocks[i].offset = offset ;
	memcpy(dev->bd->datablocks[i].data, block, size) ;

	/*
	** Signal dirty block.
	*/
	rc = semaphore_increment(dev->dirtysem) ;

	if (waitforcomplete) {
		rc = semaphore_decrement(dev->sem) ;
	}

	return size ;
}

static int
find_blocknum(BLOCK blocks[], int numblocks, int blocknum)
{
	int i ;

	for (i = 0 ; i < numblocks ; i++) {
		if (blocknum == blocks[i].blocknum) {
			return i ;
		}
	}
	return -1 ;
}

int 
blockdevice_readblock(BLOCKDEVICEP dev, void *block, int blocknum)
{
	int i ;
	int blocklength ;

	if ( ! dev ) {
		return -1 ;
	}

	i = find_blocknum(dev->bd->datablocks, dev->numblocks, blocknum) ;

	if ( -1 != i ) {
		memcpy(block, dev->bd->datablocks[i].data, dev->bd->datablocks[i].datalength) ;
		blocklength = dev->bd->datablocks[i].datalength ;
		dev->bd->datablocks[i].datalength = -1 ;
		dev->bd->datablocks[i].blocknum = -1 ;
	}

	return (i == -1 ? -1 : blocklength) ;
}

static int
find_next_dirty_block(BLOCKDEVICEP dev)
{
	int i ;
	int numblocks ;
	int lastblockchecked ;

	numblocks = dev->numblocks ;
	lastblockchecked = dev->lastblockchecked ;

	while ( 1 ) {
		for (i = lastblockchecked ; i < numblocks ; i++) {
			if (dev->bd->datablocks[i].blocknum != -1) {
				dev->lastblockchecked = i ;
				return i ;
			}
		}

		if (lastblockchecked) {
			/*
			** If we are here then that means we started
			** somewhere in the middle of the block list.
			** What we want to do is check from the top
			** back to this point. We do this by setting
			** the local variable numblocks to lastblockchecked 
			** (thus limiting the blocks to check), and set
			** lastblockchecked to 0 (to start from top)
			** 
			*/
			numblocks = lastblockchecked ;
			lastblockchecked = 0 ;
		} else {
			/*
			** We've searched the whole array, set
			** dev->lastblockchecked to 0, and return -1
			** saying we didn't find anything.
			*/
			dev->lastblockchecked = 0 ;
			return -1 ;
		}
	}
}

int
blockdevice_getnextdirtyblock(BLOCKDEVICEP dev, BLOCK *block, int wait)
{
	int i ;
	int rc ;

	
	if (wait) {
		rc = semaphore_decrement(dev->dirtysem) ;
	}


	i = find_next_dirty_block(dev) ;

	if ( -1 != i ) {
		memcpy(block->data, dev->bd->datablocks[i].data, dev->bd->datablocks[i].datalength) ;
		block->datalength = dev->bd->datablocks[i].datalength ;
		block->blocknum = dev->bd->datablocks[i].blocknum ;
		block->offset = dev->bd->datablocks[i].offset ;
		dev->bd->datablocks[i].datalength = -1 ;
		dev->bd->datablocks[i].blocknum = -1 ;
		return 1 ;
	}

	return 0 ;
}

int 
blockdevice_getblocksize(BLOCKDEVICEP dev)
{
	if (dev) {
		return dev->blocksize ;
	}

	return 0 ;
}

int
blockdevice_lock(BLOCKDEVICEP dev)
{
	/*
	** Not implemented yet.
	*/
	return 0 ;
}

int
blockdevice_unlock(BLOCKDEVICEP dev)
{
	/*
	** Not implemented yet.
	*/
	return 0 ;
}

int 
blockdevice_dump(BLOCKDEVICEP dev)
{
	return 0 ;
}
