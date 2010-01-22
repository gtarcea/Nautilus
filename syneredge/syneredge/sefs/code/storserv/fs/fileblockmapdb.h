
#ifndef __FILEBLOCKMAPDB_INCLUDE_
#define __FILEBLOCKMAPDB_INCLUDE_

#include "fileblockmap.h"

#ifdef __cplusplus
extern "C" {
#endif

struct FILEBLOCKMAPDB ;
typedef struct FILEBLOCKMAPDB FILEBLOCKMAPDB ;
typedef FILEBLOCKMAPDB *FILEBLOCKMAPDBP ;

FILEBLOCKMAPDBP fileblockmapdb_open(char *name, int create) ;
char *fileblockmapdb_getdbpath(FILEBLOCKMAPDBP mapdb) ;
int fileblockmapdb_removefileblockmap(FILEBLOCKMAPDBP mapdb, char *name) ;
int fileblockmapdb_close(FILEBLOCKMAPDBP mapdb) ;
FILEBLOCKMAPP fileblockmapdb_findfileblockmap(FILEBLOCKMAPDBP mapdb, char *name) ;
FILEBLOCKMAPP fileblockmapdb_openfileblockmap(FILEBLOCKMAPDBP mapdb, char *filename, 
			int create) ;
int fileblockmapdb_fileblockmapexists(FILEBLOCKMAPDBP mapdb, char *name) ;


#ifdef __cplusplus
}
#endif

#endif /* __FILEBLOCKMAPDB_INCLUDE_ */
