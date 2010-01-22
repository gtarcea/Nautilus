
#include "arraylist.h"
#include <stdlib.h>

struct ARRAYLIST {
	ITEMCOMPAREFUNC comparefunc ;
	ITEMFREEFUNC itemfreefunc ;
	ITEMFINDFUNC itemfindfunc ;
	int currentindex ;
	int count ;
	int numunused ;
	void **itemlist ;
} ;

#define MAX_ARRAY_LENGTH 128000

ARRAYLISTP 
arraylist_create(int startingsize)
{
	ARRAYLISTP list ;
	int i ;

	if (startingsize < 1) {
		return NULL ;
	}

	if (startingsize > MAX_ARRAY_LENGTH) {
		return NULL ;
	}

	list = malloc(sizeof(ARRAYLIST)) ;
	list->itemlist = malloc(sizeof(void *) *startingsize) ;

	for (i = 0 ; i < startingsize; i++) {
		list->itemlist[i] = NULL ;
	}

	list->count = list->numunused = startingsize ;
	list->currentindex = 0 ;
	list->comparefunc = NULL ;
	list->itemfreefunc = NULL ;
	list->itemfindfunc = NULL ;

	return list ;
}

static int
compare_item(ARRAYLISTP alist, int index, void *item)
{
	if (! alist->comparefunc) {
		return -1 ;
	}
	if (! alist->itemlist[index]) {
		return 0 ;
	}
	return alist->comparefunc(alist->itemlist[index], item) ;
}

static int
free_item(ARRAYLISTP alist, int index)
{
	if ( alist->itemfreefunc) {
		alist->itemfreefunc(alist->itemlist[index]) ;
		alist->itemlist[index] = 0 ;
		alist->numunused++ ;
		return 1 ;
	}

	return 0 ;
}

static int
free_each_item(ARRAYLISTP alist)
{
	int i ;

	for (i = 0; i < alist->count; i++) {
		if (alist->itemlist[i]) {
			if (!free_item(alist, i)) {
				return 0 ;
			}
		}
	}
	return 1 ;
}

int 
arraylist_destroy(ARRAYLISTP alist)
{
	if (! alist) {
		return 0 ;
	}

	free_each_item(alist) ;
	free(alist->itemlist) ;
	free(alist) ;
	return 1 ;
}

static int
add_item(ARRAYLISTP alist, void *item)
{
	int i ;

	for( i = 0 ; i < alist->count; i++) {
		if (alist->itemlist[i] == NULL) {
			alist->itemlist[i] = item ;
			return 1 ;
		}
	}
	return 0 ;
}

static int 
add_space_to_arraylist(ARRAYLISTP alist)
{
	int i ;

	alist->itemlist = realloc(alist->itemlist, sizeof(void *) * (alist->count+100)) ;
	if (alist->itemlist == NULL) {
		return 0 ;
	}

	for (i = alist->count; i < alist->count+100; i++) {
		alist->itemlist[i] = NULL ;
	}

	alist->count += 100 ;
	alist->numunused += 100 ;
	return 1 ;
}

int 
arraylist_additem(ARRAYLISTP alist, void *item)
{

	if ( ! alist ) {
		return 0 ;
	}

	if (alist->numunused) {
		if ( ! add_item(alist, item) ) {
			return 0 ;
		}
		alist->numunused-- ;
		return 1 ;
	} else {
		if ( ! add_space_to_arraylist(alist) ) {
			return 0 ;
		}

		if ( ! add_item(alist, item) ) {
			return 0 ;
		}
		alist->numunused-- ;
		return 1 ;
	}
}

int 
arraylist_setcomparefunc(ARRAYLISTP alist, ITEMCOMPAREFUNC comparefunc)
{
	if (! alist) {
		return 0 ;
	}

	alist->comparefunc = comparefunc ;

	return 1 ;
}

int 
arraylist_setitemfreefunc(ARRAYLISTP alist, ITEMFREEFUNC itemfreefunc)
{
	if (! alist) {
		return 0 ;
	}

	alist->itemfreefunc = itemfreefunc ;

	return 1 ;
}

int 
arraylist_setitemfindfunc(ARRAYLISTP alist, ITEMFINDFUNC itemfindfunc)
{
	if (! alist) {
		return 0 ;
	}

	alist->itemfindfunc = itemfindfunc ;

	return 1 ;
}

int
arraylist_deleteitem(ARRAYLISTP alist, void *item)
{
	int i ;
	int compareresult ;

	if (alist && item) {
		for (i = 0 ; i < alist->count; i++) {
			compareresult = compare_item(alist, i, item) ;
			if (compareresult == 0) {
				continue ;
			} else if (compareresult == 1) {
				if (! free_item(alist, i)) {
					return 0 ;
				}
				alist->itemlist[i] = 0 ;
				return 1 ;
			} else { /* compareresult == -1 */
				return 0 ;
			}
		}
	}
	return 0 ;
}

int
arraylist_deletecurrentitem(ARRAYLISTP alist)
{
	int i ;

	if (alist) {
		if (alist->itemlist[alist->currentindex]) {
			i = alist->currentindex ;
			alist->itemfreefunc(alist->itemlist[i]) ;
			alist->itemlist[i] = 0 ;
			alist->numunused++ ;
			return 1 ;
		}
	}

	return 0 ;
}

void *
arraylist_finditem(ARRAYLISTP alist, void *what)
{
	int i ;

	if (! alist->itemfindfunc) {
		return NULL ;
	}

	for (i = 0 ; i < alist->count; i++) {
		if (alist->itemfindfunc(alist->itemlist[i], what)) {
			alist->currentindex = i ;
			return alist->itemlist[i] ;
		}
	}

	return NULL ;
}

void *
arraylist_getnextitem(ARRAYLISTP alist, int next)
{
	int i ;

	if (next == LSTART) {
		alist->currentindex = 0 ;
	} else if (next != LNEXT) {
		return NULL ;
	}

	if (alist->currentindex == alist->count) {
		return NULL ;
	}

	while(1) {
		if (alist->itemlist[alist->currentindex]) {
			i = alist->currentindex ;
			alist->currentindex++ ;
			return alist->itemlist[i] ;
		} else {
			alist->currentindex++ ;
			if (alist->currentindex == alist->count) {
				return NULL ;
			}
		}
	}

	return NULL ;
}

int 
arraylist_getitemcount(ARRAYLISTP alist)
{
	if (! alist) {
		return -1 ;
	}

	return (alist->count - alist->numunused) ;
}
