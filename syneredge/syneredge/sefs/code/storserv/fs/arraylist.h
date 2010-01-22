
#ifndef __ARRAYLIST_INCLUDE_
#define __ARRAYLIST_INCLUDE_

#include "next.h"

#ifdef __cplusplus
extern "C" {
#endif

struct ARRAYLIST ;
typedef struct ARRAYLIST ARRAYLIST ;
typedef ARRAYLIST *ARRAYLISTP ;

typedef int (*ITEMCOMPAREFUNC)(void *a, void *b) ;
typedef int (*ITEMFREEFUNC)(void *item) ;
typedef int (*ITEMFINDFUNC)(void *item, void *what) ;

ARRAYLISTP arraylist_create(int startingsize) ;
int arraylist_destroy(ARRAYLISTP alist) ;
int arraylist_additem(ARRAYLISTP alist, void *item) ;
int arraylist_setcomparefunc(ARRAYLISTP alist, ITEMCOMPAREFUNC comparefunc) ;
int arraylist_setitemfreefunc(ARRAYLISTP alist, ITEMFREEFUNC itemfreefunc) ;
int arraylist_setitemfindfunc(ARRAYLISTP alist, ITEMFINDFUNC itemfindfunc) ;
int arraylist_deleteitem(ARRAYLISTP alist, void *item) ;
int arraylist_deletecurrentitem(ARRAYLISTP alist) ;
void *arraylist_finditem(ARRAYLISTP alist, void *what) ;
void *arraylist_getnextitem(ARRAYLISTP alist, int next) ;
int arraylist_getitemcount(ARRAYLISTP alist) ;

#ifdef __cplusplus
}
#endif

#endif /* __ARRAYLIST_INCLUDE_ */

