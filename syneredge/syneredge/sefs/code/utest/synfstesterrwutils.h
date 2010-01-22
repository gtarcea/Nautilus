
#ifndef __SYNFSTESTERRWUTILS_INCLUDE_
#define __SYNFSTESTERRWUTILS_INCLUDE_

#ifdef __cplusplus
extern "C" {
#endif

int
open_write_to_file(char *filename, char *buf, int buflen, int release, 
		int compareblocks, int openthefile) ;

int
open_read_file(char *filename, char *buf, int buflen, int *amountread, 
		int release, int openthefile) ;

int
compare_blocks(char *filename, char *buf, int length, int offset) ;

#ifdef __cplusplus
}
#endif

#endif /* __SYNFSTESTERRWUTILS_INCLUDE_ */
