
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <signal.h>
#include <stdio.h>

#include "blockdevice.h"
#include "mirrorenv.h"
#include "next.h"
#include "daemon.h"

static BLOCKDEVICEP bdev = NULL ;
static int exitloop = 0 ;
static MIRRORENVP mirrors ;

static char zeroblock[REAL_BLOCK_LENGTH] ;

static int
usage()
{
	printf("synblkwrd [-n]\n") ;
	exit(1) ;
}

static int
init_block_device()
{
	bdev = blockdevice_open("/syneredge/doesnotmatter") ;
	if ( NULL == bdev ) {
		return 0 ;
	}

	memset(zeroblock, 0, REAL_BLOCK_LENGTH) ;

	return 1 ;
}

static int
write_block_to_file(BLOCK *block, char *filename)
{
	int fd ;
	int rc ;
	int amount_to_fill ;
	int startingat ;
	int newblock = 0 ;

	/*printf("open '%s'\n", filename) ;*/
	fd = open(filename, O_WRONLY) ;
	if (fd < 0) {
		fd = open(filename, O_WRONLY | O_CREAT, 0666) ;
		newblock = 1 ;
	}
	if ( fd == -1 ) {
		/*printf("open of %s failed\n", filename) ;*/
		return 0 ;
	}

	/*printf("length = %d, offset = %d\n", block->datalength, block->offset) ;*/
	rc = pwrite(fd, block->data, block->datalength, block->offset) ;
	if ( rc != block->datalength) {
		/*
		** Error!
		*/
		
		/*printf("rc = %d\n", rc) ;*/
		close(fd) ;
		return 0 ;
	}

	if (newblock) {
		amount_to_fill = REAL_BLOCK_LENGTH - block->datalength - (block->offset) ;
		startingat = REAL_BLOCK_LENGTH - amount_to_fill + (block->offset);

	/*printf("amount_to_fill = %d, startingat = %d\n",
		amount_to_fill, startingat) ;*/

		if ((amount_to_fill) > 0) {
			/*
			** fill in the rest of the block with zeros
			*/
			pwrite(fd, zeroblock, amount_to_fill, startingat) ;
		}
	}

	close(fd) ;

	return 1 ;
}

static int
write_block(BLOCK *block)
{
	char filename[1024] ;
	MIRRORENTRYP mentry ;
	int wroteblock = 0 ;

	mentry = mirrorenv_getnextmirror(mirrors, LSTART) ;

	while (1) {
		if (! mentry) {
			break ;
		}

		sprintf(filename, "%s/%d", mentry->file, block->blocknum) ;

		if (write_block_to_file(block, filename)) {
			/*
			** Log an error.
			*/
			wroteblock = 1 ;
		} else {
			/*
			** Log an error.
			*/
		}

		mentry = mirrorenv_getnextmirror(mirrors, LNEXT) ;
	}

	return wroteblock ;
}

static int
write_dirty_blocks()
{
	BLOCK block ;
	int rc ;

	while ( blockdevice_getnextdirtyblock(bdev, &block, 1) ) {
		rc = write_block(&block) ;
		if ( ! rc ) {
			/*
			** Write block failed. What to do? What to do?
			*/
		}

		rc = blockdevice_signalblockdone(bdev, block.blocknum) ;
		if (! rc) {
			/*
			** Signal block done failed.
			** What to do? What to do?
			*/
		}
	}
}

static int
block_write_loop()
{
	BLOCK block ;
	int rc ;

	while ( 1 ) {
		if (exitloop) {
			/*
			** Flush remaining
			*/
			write_dirty_blocks() ;
			break ;
		}
		write_dirty_blocks() ;
	}
}

static void
sig_handler(int signo)
{
	exitloop = 1 ;
	return ;
}

static int
init_mirrors()
{
	MIRRORENTRYP mentry ;
	char command[1024] ;

	mirrors = mirrorenv_open("/etc/syneredge") ;

	if (! mirrors) {
		return 0 ;
	}

	/*
	** Create directories if they don't exist
	*/
	mentry = mirrorenv_getnextmirror(mirrors, LSTART) ;
	while(1) {
		if (! mentry) {
			break ;
		}

		sprintf(command, "mkdir %s > /dev/null 2>&1",
			mentry->file) ;
		system(command) ;

		mentry = mirrorenv_getnextmirror(mirrors, LNEXT) ;
	}

	return 1 ;
}

main(int argc, char **argv)
{
	int c ;
	int become_daemon = 1 ;

	while ( (c = getopt(argc, argv, "n")) != EOF) {
		switch (c) {
			case 'n':
				become_daemon = 0 ;
				break ;
			case '?':
			default:
				usage() ;
		}
	}

	if ( ! daemon_setup_sighandlers(sig_handler) ) {
		exit(1) ;
	}

	if ( ! daemon_start(become_daemon) ) {
		exit(1) ;
	}

	if ( ! init_block_device() ) {
		printf("Block device Initialization failed\n") ;
		exit(1) ;
	}

	if ( ! init_mirrors() ) {
		printf("Mirror Initialization failed\n") ;
		exit(1) ;
	}

	block_write_loop() ;

	exit(0) ;
}

