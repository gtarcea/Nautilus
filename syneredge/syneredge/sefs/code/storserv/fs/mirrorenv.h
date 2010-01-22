
#ifndef __MIRRORENV_INCLUDE_
#define __MIRRORENV_INCLUDE_

#ifdef __cplusplus
extern "C" {
#endif

struct MIRRORENV ;
typedef struct MIRRORENV MIRRORENV ;
typedef MIRRORENV *MIRRORENVP ;

typedef struct MIRRORENTRY {
	char *file ;
	int blocksize ;
	int blockcount ;
	char *host ;
} MIRRORENTRY, *MIRRORENTRYP ;

MIRRORENVP mirrorenv_open(char *basedir) ;
MIRRORENTRYP mirrorenv_getnextmirror(MIRRORENVP mdb, int next) ;
int mirrorenv_destroy(MIRRORENVP mdb) ;

#ifdef __cplusplus
}
#endif

#endif /* __MIRRORENV_INCLUDE_ */

