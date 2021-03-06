
#include "synfs.h"
#include "tester.h"
#include "fileblockmap.h"
#include "fileblock.h"
#include "next.h"
#include "errno.h"

static int
read_a_block(char *dir, char *buf, int blocknum)
{
	FILE *fp ;
	char filename[1024] ;

	sprintf(filename, "%s/%d", dir, blocknum) ;

	fp = fopen(filename, "r") ;

	if (!fp) {
		perror("The error") ;
		printf("fopen(%s) failed errno = %d\n", filename, errno) ;
		return 0 ;
	}

	fread(buf, sizeof(char), 1024, fp) ;

	printf("buf = %s\n", buf) ;
	fclose(fp) ;

	return 1 ;
}

static int
read_block(char *buf, int blocknum)
{
	return read_a_block("/tmp/syneredgeblocks", buf, blocknum) ;
}

static int
read_block2(char *buf, int blocknum)
{
	return read_a_block("/tmp/syneredgeblocks2", buf, blocknum) ;
}

int
compare_blocks(char *filename, char *buf, int length, int offset)
{
	FILEBLOCKMAPP fbmap ;
	int clen ;
	int cmpresult ;
	int savelen ;
	int rc ;
	char buf1[1024] ;
	char buf2[1024] ;
	FILEBLOCKP fblock ;

	fbmap = synfs_getfbmap(filename) ;

	if (!fbmap) {
		printf("synfs_getfbmap(%s) failed\n", filename) ;
		return 0 ;
	}

	/*
	** Ignore offset for now.
	*/

	if (length > 1024) {
		clen = 1024 ;
	} else {
		clen = length ;
	}

	savelen = length ;

	fblock = fileblockmap_getnextblock(fbmap, LSTART) ;
	while (1) {
		if (!fblock) {
			break ;
		}

		rc = read_block(buf1, fileblock_getblocknum(fblock)) ;
		rc = read_block2(buf2, fileblock_getblocknum(fblock)) ;

		cmpresult = memcmp(buf, buf1, clen) ;
		if (cmpresult) {
			printf("Block compare failed\n") ;
			return 0 ;
		}

		cmpresult = memcmp(buf, buf2, clen) ;
		if (cmpresult) {
			printf("Block compare failed\n") ;
			return 0 ;
		}

		savelen -= clen ;

		if (! savelen) {
			break ;
		}
		buf+=clen ;

		if (savelen > 1024) {
			clen = 1024 ;
		} else {
			clen = savelen ;
		}

		fblock = fileblockmap_getnextblock(fbmap, LNEXT) ;
	}
}

int
open_write_to_file(char *filename, char *buf, int buflen, int release, int compareblocks, int openthefile)
{
	int rc ;

	if (openthefile) {
		rc = synfs_open(filename, -43) ;
		if (rc < 0) {
			printf("synfs_open() failed\n") ;
			return 0 ;
		}
	}

	buflen = strlen(buf) ;
	rc = synfs_write(filename, buf, buflen, 0) ;
	if (rc != buflen) {
		printf("synfs_write() failed rc = %d, buflen = %d\n", rc, buflen) ;
		return 0 ;
	}

	if (compareblocks) {
		rc = compare_blocks(filename, buf, buflen, 0) ;
		if (! rc) {
			printf("Buf written and blocks don't match\n") ;
			return 0 ;
		}
	}

	if (release) {
		rc = synfs_release(filename, 0) ;
		if (rc < 0) {
			printf("synfs_release() failed\n") ;
			return 0 ;
		}
	}

	return 1 ;
}

int
open_read_file(char *filename, char *buf, int buflen, int *amountread, int release, int openthefile)
{
	int rc ;

	if (openthefile) {
		rc = synfs_open(filename, -43) ;
		if (rc < 0) {
			printf("synfs_open() failed\n") ;
			return 0 ;
		}
	}

	rc = synfs_read(filename, buf, buflen, 0) ;

	if (rc < 0) {
		printf("synfs_read() returned unexpected value (%d) expected %d\n", rc, buflen) ;
		return 0 ;
	}

	*amountread = rc ;

	if (release) {
		rc = synfs_release(filename, 0) ;
		if (rc < 0) {
			printf("synfs_release() failed\n") ;
			return 0 ;
		}
	}

	return 1 ;

}
