
#ifndef __SYNFS_INCLUDE_
#define __SYNFS_INCLUDE_

#include <fuse.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <dirent.h>
#include <errno.h>
#include <sys/statfs.h>
#include "fileblockmap.h"

#ifdef __cplusplus
extern "C" {
#endif

int synfs_getattr(const char *path, struct stat *stbuf) ;

int synfs_readlink(const char *path, char *buf, size_t size) ;

int synfs_getdir(const char *path, fuse_dirh_t h, fuse_dirfil_t filler) ;

int synfs_mknod(const char *path, mode_t mode, dev_t rdev) ;

int synfs_mkdir(const char *path, mode_t mode) ;

int synfs_unlink(const char *path) ;

int synfs_rmdir(const char *path) ;

int synfs_symlink(const char *from, const char *to) ;

int synfs_rename(const char *from, const char *to) ;

int synfs_link(const char *from, const char *to) ;

int synfs_chmod(const char *path, mode_t mode) ;

int synfs_chown(const char *path, uid_t uid, gid_t gid) ;

int synfs_truncate(const char *path, off_t size) ;

int synfs_utime(const char *path, struct utimbuf *buf) ;

int synfs_open(const char *path, int flags)  ;

int synfs_read(const char *path, char *buf, size_t size, off_t offset) ;

int synfs_write(const char *path, const char *buf, size_t size, off_t offset) ;

int synfs_statfs(const char *path, struct statfs *stbuf) ;

int synfs_release(const char *path, int flags) ;

int synfs_fsync(const char *path, int isdatasync) ;

int synfs_flush(const char *path) ;

int synfs_setxattr(const char *path, const char *name, const char *value,
			size_t size, int flags) ;

int synfs_getxattr(const char *path, const char *name, char *value,
			size_t size) ;

int synfs_listxattr(const char *path, char *list, size_t size) ;

int synfs_removexattr(const char *path, const char *name) ;

int synfs_init(char *dbpath, char *bdevpath, int bsize, int numblocks) ;
int synfs_shutdown(int freememory) ;
int synfs_run(int argc, char **argv) ;

FILEBLOCKMAPP synfs_getfbmap(char *filename) ;

#ifdef __cplusplus
}
#endif 

#endif /* __SYNFS_INCLUDE_ */
