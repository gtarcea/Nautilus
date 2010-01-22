
#include "hostfslist.h"
#include "tester.h"
#include <stdio.h>

static int
test_create_destroy_list()
{
	HOSTFSLISTP list ;
	int rc ;

	list = hostfslist_create("spelljammer", "/usr") ;

	if (! list) {
		printf("hostfslist_create(spelljammer, /usr) failed\n") ;
		return 0 ;
	}

	rc = hostfslist_destroy(list) ;

	if (! rc) {
		printf("hostfslist_destroy() failed\n") ;
		return 0 ;
	}

	return 1 ;
}

static HOSTFSLISTP
create_hostfslist()
{
	HOSTFSLISTP list ;

	list = hostfslist_create("spelljammer", "/usr") ;

	return list ;
}

#define ADDHOSTFSENTRY(a,b) rc = hostfslist_addhostfs(list,a,b) ; if (! rc) { printf("hostfslist_addhostfs(%s, %s) failed\n", a, b) ; return 0 ; }

struct anentry {
	char *host ;
	char *fs ;
	int found ; /* not used right now (maybe in the future? */
} ;

static int
test_add_many_entries(HOSTFSLISTP list)
{
	int rc ;
	int i, j ;
	struct anentry entryarray[10] = {
		{"spelljammer", "/usr1", 0},
		{"spelljammer", "/usr2", 0},
		{"spelljammer", "/usr3", 0},
		{"spelljammer", "/usr4", 0},
		{"spelljammer", "/usr5", 0},
		{"spelljammer", "/usr6", 0},
		{"buford", "/usr1", 0},
		{"buford", "/usr2", 0},
		{"buford", "/usr3", 0},
		{"buford", "/usr4", 0},
	} ;

	for (i = 0 ; i < 10; i++) {
		ADDHOSTFSENTRY(entryarray[i].host, entryarray[i].fs)
	}

	for (i = 0 ; i < 10; i++) {
		rc = hostfslist_findhostfsentry(list, entryarray[i].host,
				entryarray[i].fs) ;
		if (!rc) {
			printf("hostfslist_findhostfsentry(%s, %s) failed\n",
				entryarray[i].host, entryarray[i].fs) ;
			return 0 ;
		}
	}

	return 1 ;
}

static int
look_for_dups(HOSTFSLISTP list, char *host, char *fs)
{
	HOSTFSENTRY *entry ;
	int count ;

	count = 0 ;

	entry = hostfslist_getnextentry(list, LSTART) ;
	while (1) {
		if (! entry) {
			break ;
		}

		if (strcmp(entry->host, host) == 0) {
			if (strcmp(entry->fs, fs) == 0) {
				count++ ;
			}
		}

		entry = hostfslist_getnextentry(list, LNEXT) ;
	}

	return count ;
}

static int
test_add_duplicate_entries(HOSTFSLISTP list)
{
	int count ;
	int rc ;
	HOSTFSENTRY *entry ;

	ADDHOSTFSENTRY("duphost", "dupfs") ;
	ADDHOSTFSENTRY("duphost", "dupfs") ;

	count = look_for_dups(list, "duphost", "dupfs") ;

	if (count != 1) {
		printf("Duplicate support failed, found dup %d times\n", count) ;
		return 0 ;
	}

	/*
	** Check that different hosts, same fs works.
	*/
	ADDHOSTFSENTRY("host1", "dupfs") ;
	ADDHOSTFSENTRY("host2", "dupfs") ;

	count = look_for_dups(list, "host1", "dupfs") ;
	if (count != 1) {
		printf("Duplicate fs, different hosts failed, dup count %d\n", count) ;
		return 0 ;
	}

	count = look_for_dups(list, "host2", "dupfs") ;
	if (count != 1) {
		printf("Duplicate fs, different hosts failed, dup count %d\n", count) ;
		return 0 ;
	}

	/*
	** Check that different fs, same host works.
	*/
	ADDHOSTFSENTRY("duphost1", "fs1") ;
	ADDHOSTFSENTRY("duphost1", "fs2") ;

	count = look_for_dups(list, "host1", "dupfs") ;
	if (count != 1) {
		printf("Duplicate host, different fs failed, dup count %d\n", count) ;
		return 0 ;
	}

	count = look_for_dups(list, "host2", "dupfs") ;
	if (count != 1) {
		printf("Duplicate host, different fs failed, dup count %d\n", count) ;
		return 0 ;
	}

	return 1 ;
}

static int
test_remove_entries(HOSTFSLISTP list)
{
	int rc ;
	int count ;

	ADDHOSTFSENTRY("hosttoremove", "fs1") ;

	rc = hostfslist_removehostfs(list, "hosttoremove", "fs1") ;
	if (! rc) {
		printf("hostfslist_removehostfs(hosttoremove, fs1) failed\n") ;
		return 0 ;
	}

	count = look_for_dups(list, "hosttoremove", "fs1") ;
	if (count != 0) {
		printf("Found removed entry 'hosttoremove' 'fs1'\n") ;
		return 0 ;
	}

	return 1 ;
}

static int
test_find_entry(HOSTFSLISTP list)
{
	int rc ;

	ADDHOSTFSENTRY("hosttofind", "fstofind") ;

	rc = hostfslist_findhostfsentry(list, "hosttofind", "fstofind") ;
	if (! rc) {
		return 0 ;
	}

	return 1 ; 
}

#define TF(a, what, func) if (a != what) {printf("%s succeeded on NULL\n", func) ; return 0 ; }
static int
test_null_list_checking(HOSTFSLISTP list)
{
	HOSTFSLISTP list2 ;
	int rc ;
	HOSTFSENTRY *entry ;

	list2 = hostfslist_create(NULL, "fs1") ;
	TF(list2, NULL, "hostfslist_create(NULL, fs1)")

	list2 = hostfslist_create("spelljammer", NULL) ;
	TF(list2, NULL, "hostfslist_create(spelljammer, NULL)")

	list2 = hostfslist_create(NULL, NULL) ;
	TF(list2, NULL, "hostfslist_create(NULL, NULL)")

	rc = hostfslist_destroy(NULL) ;
	TF(rc, 0, "hostfslist_destroy(NULL)")

	rc = hostfslist_addhostfs(NULL, "host", "fs") ;
	TF(rc, 0, "hostfslist_addhostfs(NULL, host, fs)")

	rc = hostfslist_addhostfs(list, NULL, "fs") ;
	TF(rc, 0, "hostfslist_addhostfs(list, NULL, fs)")

	rc = hostfslist_addhostfs(list, "host", NULL) ;
	TF(rc, 0, "hostfslist_addhostfs(list, host, NULL)")

	rc = hostfslist_addhostfs(NULL, NULL, NULL) ;
	TF(rc, 0, "hostfslist_addhostfs(NULL, NULL, NULL)")

	rc = hostfslist_addhostfs(list, NULL, NULL) ;
	TF(rc, 0, "hostfslist_addhostfs(list, NULL, NULL)")

	rc = hostfslist_removehostfs(NULL, "host", "fs") ;
	TF(rc, 0, "hostfslist_removehostfs(NULL, host, fs)")

	rc = hostfslist_removehostfs(list, NULL, "fs") ;
	TF(rc, 0, "hostfslist_removehostfs(list, NULL, fs)")

	rc = hostfslist_removehostfs(list, "host", NULL) ;
	TF(rc, 0, "hostfslist_removehostfs(list, host, NULL)")

	rc = hostfslist_removehostfs(NULL, NULL, NULL) ;
	TF(rc, 0, "hostfslist_removehostfs(NULL, NULL, NULL)")

	rc = hostfslist_removehostfs(list, NULL, NULL) ;
	TF(rc, 0, "hostfslist_removehostfs(list, NULL, NULL)")

	rc = hostfslist_findhostfsentry(NULL, "host", "fs") ;
	TF(rc, 0, "hostfslist_findhostfsentry(NULL, host, fs)")

	rc = hostfslist_findhostfsentry(list, NULL, "fs") ;
	TF(rc, 0, "hostfslist_findhostfsentry(list, NULL, fs)")

	rc = hostfslist_findhostfsentry(list, "host", NULL) ;
	TF(rc, 0, "hostfslist_findhostfsentry(list, host, NULL)")

	rc = hostfslist_findhostfsentry(NULL, NULL, NULL) ;
	TF(rc, 0, "hostfslist_findhostfsentry(NULL, NULL, NULL)")

	rc = hostfslist_findhostfsentry(list, NULL, NULL) ;
	TF(rc, 0, "hostfslist_findhostfsentry(list, NULL, NULL)")

	entry = hostfslist_getnextentry(NULL, LSTART) ;
	TF(entry, NULL, "hostfslist_getnextentry(NULL, LSTART)")

	return 1 ;
}

main()
{
	HOSTFSLISTP list ;
	int rc ;

	P("test_create_destroy_list()") ;
	rc = test_create_destroy_list() ;
	S(rc, "test_create_destroy_list()") ;

	list = create_hostfslist() ;

	P("test_add_many_entries()") ;
	rc = test_add_many_entries(list) ;
	S(rc, "test_add_many_entries()") ;

	P("test_add_duplicate_entries()") ;
	rc = test_add_duplicate_entries(list) ;
	S(rc, "test_add_duplicate_entries()") ;

	P("test_remove_entries()") ;
	rc = test_remove_entries(list) ;
	S(rc, "test_remove_entries()") ;

	P("test_find_entry()") ;
	rc = test_find_entry(list) ;
	S(rc, "test_find_entry()") ;

	P("test_null_list_checking()") ;
	rc = test_null_list_checking(list) ;
	S(rc, "test_null_list_checking()") ;

	exit(0) ;
}
