/*
    FUSE: Filesystem in Userspace
    Copyright (C) 2001-2004  Miklos Szeredi <miklos@szeredi.hu>

    This program can be distributed under the terms of the GNU GPL.
    See the file COPYING.
*/

/*
** Integrate BlockFileIO into filesystem.
** Steps:
**   1. One BlockFileIO, reopen file on each read/write. Keep current locking in place.
**   2. Extend BlockFileIO to shared a BlockFile, keep a queue of interfaces. Support
**      only 1 BlockFileIO and BlockFile interface.
*/

#define USE_NETWORK_BLOCKFILE

#include "config.h"
#include <sys/types.h>
#include <linux/unistd.h>
#include <sys/time.h>
#include <time.h>
#include <pthread.h>
#include <stdlib.h>
#include "mirrorenv.h"
#include "next.h"
#include <cstring>
#include <cctype>
#include <queue>
#include <list>
#include "PT.hpp"

//#define PT() { struct timeval t ; gettimeofday(&t, NULL); printf("Time = %s (%ul) threadid = %ul\n", ctime(&t.tv_sec), t.tv_usec, pthread_self()) ; }

//#define PT() { struct timeval t ; gettimeofday(&t, NULL); printf("Time = %ul:(%ul) threadid = %ul\n", t.tv_sec, t.tv_usec, pthread_self()) ; }

static pthread_mutex_t ctxmutex = PTHREAD_MUTEX_INITIALIZER ;

/*
** For snapshots, if we aren't looking at /etc/syneredge/fbmap, then
** we assume snapshot, and treat FS as readonly. This is another
** hack that needs to be cleaned up.
*/
static int fsreadonly = 0 ;

/*
** Mirrors to read from.
*/
static MIRRORENVP mirrors ;

#define MLOCK pthread_mutex_lock(&ctxmutex)
#define MUNLOCK pthread_mutex_unlock(&ctxmutex)

#ifdef linux
/* For pread()/pwrite() */
#define _XOPEN_SOURCE 500
#endif

#include "synfs.h"

/*
** SynerEdge Includes
*/
#include "fileblockmap.h"
#include "fileblockmapdb.h"
#include "blockdevice.h"
#include "Block.hpp"
#include "BlockFile.hpp" // For now we go directly to the file.
#include "BlockFileIO.hpp"

using namespace std ;
using namespace SynerEdge ;

static FILEBLOCKMAPP truncfbmap = NULL ;

#define HAVE_SETXATTR

#ifdef HAVE_SETXATTR
#include <sys/xattr.h>
#endif


/*
**static const char *gpath = "/etc/syneredge/fbmap" ;
*/

static char *gpath = NULL ;


/*
** Globals
*/
static FILEBLOCKMAPDBP synfsmapdb = NULL ;
static BLOCKDEVICEP wrbdev = NULL ;
static int nextblocknum = -1 ;

static struct fuse_operations synfs_oper ;

//static queue<BlockFile *, list<BlockFile *> > bfqueue ;

static BlockFileIO *bfio ;

#ifdef USE_NETWORK_BLOCKFILE
#include "NetworkBlockFile.hpp"
static TCPServerSocket *socket ;
static ClientSocket *cli ;
static NetworkBlockFile *bf ;
#else
static BlockFile *bf ;
#endif

/*
static struct fuse_operations synfs_oper = {
    .getattr	= synfs_getattr,
    .readlink	= synfs_readlink,
    .getdir	= synfs_getdir,
    .mknod	= synfs_mknod,
    .mkdir	= synfs_mkdir,
    .symlink	= synfs_symlink,
    .unlink	= synfs_unlink,
    .rmdir	= synfs_rmdir,
    .rename	= synfs_rename,
    .link	= synfs_link,
    .chmod	= synfs_chmod,
    .chown	= synfs_chown,
    .truncate	= synfs_truncate,
    .utime	= synfs_utime,
    .open	= synfs_open,
    .read	= synfs_read,
    .write	= synfs_write,
    .statfs	= synfs_statfs,
    .flush	= synfs_flush,
    .release	= synfs_release,
    .fsync	= synfs_fsync,
#ifdef HAVE_SETXATTR
    .setxattr	= synfs_setxattr,
    .getxattr	= synfs_getxattr,
    .listxattr	= synfs_listxattr,
    .removexattr= synfs_removexattr,
#endif
};
*/

static char *make_path(const char *path)
{
	static char mypath[255] ;

	sprintf(mypath, "%s%s", gpath, path) ;
	return mypath ;
}

static const char *
getfilename(const char *path)
{
	char *slash ;
	/*
	** Update this to add /etc/syneredge/fbmap?
	*/

	slash = strrchr(path, '/') ;

	if (slash) {
		/*
		** Found slash, strip of and return filename
		*/
		slash++ ;
		return slash ;
	} else {
		/*
		** No slash! Ok, well then just return filename.
		*/
		return path ;
	}
}

int synfs_getattr(const char *path, struct stat *stbuf)
{
	int res;
	FILEBLOCKMAPP fbmap ;
	char *filename ;
	int myerrno ;

	MLOCK ;

	printf("pid = %d synfs_getattr() %s\n", getpid(), path ) ;
	PT("synfs_gettr") ;

	filename = const_cast<char *>( getfilename(path) );

	memset(stbuf, 0, sizeof(struct stat)) ;
	res = lstat(make_path(path), stbuf) ;
	myerrno = errno ;
	printf("Doing lstat on '%s'\n", make_path(path)) ;
	if (res < 0) {
		printf("lstat failed\n") ;
		MUNLOCK ;
		return -errno ;
	}

	if (S_ISREG(stbuf->st_mode)) {
		fbmap = fileblockmapdb_findfileblockmap(synfsmapdb, const_cast<char *>(path)) ;
		if (! fbmap) {
			// This should never happen....
			printf("S_ISREG ! fbmap\n") ;
			MUNLOCK ;
			return -ENOENT ;
		}
		stbuf->st_mode = S_IFREG | 0755;
		stbuf->st_nlink = 2;
		stbuf->st_size = fileblockmap_getbytecount(fbmap) ;
		fileblockmap_close(fbmap, 0) ; /* free blockmap list */
	}

	MUNLOCK ;
	return 0;
}

int synfs_readlink(const char *path, char *buf, size_t size)
{
    int res;

	printf("synfs_readlink() %s\n", path ) ;
    res = readlink(gpath, buf, size - 1);
    if(res == -1)
        return -errno;

    buf[res] = '\0';
    return 0;
}


int 
synfs_getdir(const char *path, fuse_dirh_t h, fuse_dirfil_t filler)
{
	int res = 0;
    	DIR *dp;
    	struct dirent *de;
	char pathtouse[1024] ;

	MLOCK ;
	printf("synfs_getdir() %s\n", path) ;

	sprintf(pathtouse, "%s/%s", fileblockmapdb_getdbpath(synfsmapdb), path) ;
	dp = opendir(pathtouse);
	if(dp == NULL) {
		return -errno;
	}

	while((de = readdir(dp)) != NULL) {
		res = filler(h, de->d_name, de->d_type);
		if(res != 0) {
			break;
		}
	}

	closedir(dp);

	MUNLOCK ;
	return res ;
}

int synfs_mknod(const char *path, mode_t mode, dev_t rdev)
{
	char *filename ;
	FILEBLOCKMAPP fbmap ;

	MLOCK ;

	printf("synfs_mknod() %s\n", path ) ;
	PT("synfs_mknod") ;

	if (fsreadonly) {
		MUNLOCK ;
		return -EACCES ;
	}

	//filename = const_cast<char *>(getfilename(path)) ;

	if (fileblockmap_exists(make_path(path))) {
		MUNLOCK ;
		return -EEXIST ;
	}
	
	/*
	** If we are here then we are creating a new file.
	*/
	fbmap = fileblockmapdb_openfileblockmap(synfsmapdb, const_cast<char *>(path), 1) ;

	if (! fbmap) {
		/*
		** If we couldn't create the map, then some part of the
		** path didn't exist.
		*/
		MUNLOCK ;
		return -ENOENT ;
	}

	fileblockmap_close(fbmap, 1) ; /* Save new entry */

	MUNLOCK ;
	return 0 ;
}

int synfs_mkdir(const char *path, mode_t mode)
{
	int res;
	char pathtomake[1024] ;

	printf("synfs_mkdir() %s\n", path ) ;
	sprintf(pathtomake, "%s/%s", gpath, path) ;
	res = mkdir(pathtomake, mode);
	if(res == -1)
		return -errno;

	return 0;
}

int synfs_unlink(const char *path)
{
	int rc ;
	char *filename ;

	MLOCK ;
	printf("synfs_unlink() %s\n", path ) ;
	
	/*
	** Need to clean up the blocks as well.
	*/
	filename = const_cast<char *>(getfilename(path)) ;
	rc = fileblockmapdb_removefileblockmap(synfsmapdb, filename) ;

	if (! rc) {
		MUNLOCK ;
		return -ENOENT ;
	}

	MUNLOCK ;
	return 0;
}

int synfs_rmdir(const char *path)
{
    int res;

	printf("synfs_rmdir() %s\n", path) ;
    res = rmdir(gpath);
    if(res == -1)
        return -errno;

    return 0;
}

int synfs_symlink(const char *from, const char *to)
{
    int res;

	printf("synfs_symlink() %s %s\n", from, to ) ;
    res = symlink(from, to);
    if(res == -1)
        return -errno;

    return 0;
}

int synfs_rename(const char *from, const char *to)
{
    int res;

	printf("synfs_rename() %s %s\n", from, to ) ;
    res = rename(from, to);
    if(res == -1)
        return -errno;

    return 0;
}

int synfs_link(const char *from, const char *to)
{
    int res;

	printf("synfs_link() %s %s\n", from, to ) ;
    res = link(from, to);
    if(res == -1)
        return -errno;

    return 0;
}

int synfs_chmod(const char *path, mode_t mode)
{
    int res;

	printf("synfs_chmod() %s\n", path ) ;
    res = chmod(gpath, mode);
    if(res == -1)
        return -errno;
    
    return 0;
}

int synfs_chown(const char *path, uid_t uid, gid_t gid)
{
    int res;

	printf("synfs_chown() %s\n", path ) ;
    res = lchown(gpath, uid, gid);
    if(res == -1)
        return -errno;

    return 0;
}


int synfs_truncate(const char *path, off_t size)
{
	char *filename ;
	bool rc ;
    
	MLOCK ;
	printf("pid = %d synfs_truncate() %s\n", getpid(), path ) ;
	PT("synfs_truncate") ;

	if (fsreadonly) {
		MUNLOCK ;
		return -EACCES ;
	}

	/*
	** Is all this fileblockmapdb stuff needed?
	*/
	filename = const_cast<char *>(getfilename(path)) ;
	truncfbmap = fileblockmapdb_findfileblockmap(synfsmapdb, filename) ;

	if (! truncfbmap) {
		/*
		** File map doesn't exist, so lets create
		*/
		truncfbmap = fileblockmapdb_openfileblockmap(synfsmapdb, filename, 1) ;

		if (!truncfbmap) {
			/*
			** If we are here, then some portion of the path doesn't exist
			*/
			MUNLOCK ;
			return -ENOTDIR ;
		}
	}

	string f = "/etc/syneredge/fbmap/" ;
	f.append(path) ;
	rc = bfio->open(f, 0) ;

	if (rc) {
//		cout << "Calling bfio->truncate" << endl ;
		rc = bfio->truncate(size) ;
	}

	if (! rc) {
		/*
		** Figure out a proper error code.
		*/
		MUNLOCK ;
		return -EIO ;
	}

	bfio->release() ;

	MUNLOCK ;
	return 0;
}

int synfs_utime(const char *path, struct utimbuf *buf)
{
    int res;
    
	printf("synfs_utime() %s\n", path ) ;
    res = utime(gpath, buf);
    if(res == -1)
        return -errno;

    return 0;
}


int synfs_open(const char *path, int flags) 
	/* struct fuse_file_info *finfo */
{
	FILEBLOCKMAPP fbmap ;
	char *filename ;
	int exists ;
	
	MLOCK ;

	printf("synfs_open() %s\n", path ) ;
	PT("synfs_open") ;

	if (fsreadonly) {
		if (flags & O_WRONLY || flags & O_RDWR) {
			MUNLOCK ;
			return -EACCES ;
		}
	}

	filename = const_cast<char *>(getfilename(path)) ;

	printf("filename = %s\n", path) ;
	/*
	** Really simple for now. We will ignore all flags, and
	** just treat all opens as rw, with create set if the file
	** doesn't exist. This will be fine for the demo.
	*/
	
	exists = fileblockmapdb_fileblockmapexists(synfsmapdb, filename) ;

	if (! exists) {
		if (flags != -43) {
			/*
			** Hack for unit tests.
			*/
			MUNLOCK ;
			return -ENOENT ;
		}
		/*
		** File map doesn't exist, so lets create
		*/
		fbmap = fileblockmapdb_openfileblockmap(synfsmapdb, filename, 1) ;
		fileblockmap_close(fbmap, 1) ;
	}

	string f = "/etc/syneredge/fbmap/" ;
	f.append(path) ;
	bool openrc = bfio->open(f, 0) ;

	MUNLOCK ;
	return 0;
}

static int
determinereadsize(int bytesinblock, int offset, int sizelefttoread)
{
	int sizetoread ;

	printf("bytesinblock = %d, offset = %d, sizelefttoread = %d\n",
		bytesinblock, offset, sizelefttoread) ;
	if (offset > bytesinblock) {
		/*
		** We are reading past last of data.
		*/
		sizetoread = 0 ;
		printf("1st if sizetoread = %d\n", sizetoread) ;
	} else if (offset + sizelefttoread > bytesinblock) {
		sizetoread = (bytesinblock - offset) ;
		printf("2nd if sizetoread = %d\n", sizetoread) ;
	} else if (sizelefttoread < bytesinblock) {
		sizetoread = sizelefttoread ;
		printf("3rd if sizetoread = %d\n", sizetoread) ;
	} else if (bytesinblock <= sizelefttoread) {
		sizetoread = bytesinblock ;
		printf("4th if sizetoread = %d\n", sizetoread) ;
	} else {
		printf("CASE WE DIDN'T THINK OF!!!\n") ;
		printf("bytesinblock = %d, offset = %d, sizelefttoread = %d\n",
			bytesinblock, offset, sizelefttoread) ;
		exit(1) ;
		sizetoread = 0 ;
	}

	if (sizetoread < 0) {
		printf("negative sizetoread!\n") ;
		exit(1) ;
	}
	return sizetoread ;
}

int synfs_read(const char *path, char *buf, size_t size, off_t offset)
	/* struct fuse_file_info *finfo */
{
	char *filename ;
	FILEBLOCKMAPP fbmap ;
	int rc = -EINVAL ;
	bool openrc ;

	MLOCK ;

	printf("synfs_read() %s\n", path ) ;

	PT("synfs_read") ;

	filename = const_cast<char *>(getfilename(path)) ;

	fbmap = fileblockmapdb_findfileblockmap(synfsmapdb, filename) ;
	if (! fbmap) {
		/*printf("EINVAL\n") ;*/
		MUNLOCK ;
		return -EINVAL ;
	}

//	string f = "/etc/syneredge/fbmap/" ;
//	f.append(path) ;
//	openrc = bfio->open(f, 0) ;
	openrc = true ;
	if (openrc) {
//		cout << "Calling bfio->read()" << endl ;
		rc = bfio->read(offset, buf, size) ;
//		bfio->release() ;
	}

	MUNLOCK ;
	return rc ;
}

int synfs_write(const char *path, const char *buf, size_t size,
                     off_t offset)
	/* struct fuse_file_info *finfo */
{
	int rc ;
	FILEBLOCKMAPP fbmap ;
	char *filename ;
	bool openrc ;
	int isize = size ;

	MLOCK ;

	printf("\n\npid = %d synfs_write() %s size = %d\n", getpid(), path, size) ;
	PT("synfs_write()") ;

	//filename = const_cast<char *>(getfilename(path)) ;
	//fbmap = fileblockmapdb_findfileblockmap(synfsmapdb, filename) ;

//	string f = "/etc/syneredge/fbmap/" ;
//	f.append(path) ;
//	openrc = bfio->open(f, 0) ;
	openrc = true ;
	if (openrc) {
//		cout << "Calling bfio->write()" << endl ;
		rc = bfio->write(offset, buf, size) ;
		//bfio->release() ;
	}

	PT("Leaving synfs_write()") ;
	printf("\n\n") ;
	MUNLOCK ;
	return rc ;
}

int synfs_statfs(const char *path, struct statfs *stbuf)
{
	int res;

	printf("synfs_statfs() %s\n", path) ;

	/*
	** Call statfs on a file system to fill in the structure.
	*/
	res = statfs(gpath, stbuf);
	if (res == -1)
		return -errno;

	/*
	** Now change some of the information around.
	*/

	cout << "stbuf->f_blocks = " << bfio->getBlockFileNumBlocks() << endl ;
	cout << "stbuf->f_bfree = " << bfio->getBlockFileNumFreeBlocks() << endl ;
	cout << "stbuf->f_bavail = " << bfio->getBlockFileNumFreeBlocks() << endl ;

	stbuf->f_blocks = bfio->getBlockFileNumBlocks() ;
	stbuf->f_bfree = bfio->getBlockFileNumFreeBlocks() ;
	stbuf->f_bavail = bfio->getBlockFileNumFreeBlocks() ;

	return 0;
}

int synfs_release(const char *path, int flags)
	/* struct fuse_file_info *finfo */
{
	/* Just a stub.  This method is optional and can safely be left
	unimplemented */

	printf("synfs_release() %s\n", path) ;
	bfio->release() ;

    	char duppath[255] ;
	char command[255] ;
	static int diddup = 0 ;

	return 0;
}

int synfs_fsync(const char *path, int isdatasync)
	/* struct fuse_file_info *finfo */
{
    /* Just a stub.  This method is optional and can safely be left
       unimplemented */

    (void) path;
    (void) isdatasync;
    return 0;
}

int synfs_flush(const char *path)
	/* struct fuse_file_info *finfo */
{
	bfio->flush() ;
	return 0 ;
}

#ifdef HAVE_SETXATTR
/* xattr operations are optional and can safely be left unimplemented */
int synfs_setxattr(const char *path, const char *name, const char *value,
                        size_t size, int flags)
{
	printf("synfs_setxattr %s\n", path ) ;
    /*int res = lsetxattr(gpath, name, value, size, flags);
    if(res == -1)
        return -errno;*/
    return 0;
}

int synfs_getxattr(const char *path, const char *name, char *value,
                    size_t size)
{

	printf("synfs_getxattr() %s\n", path ) ;
/*    int res = lgetxattr(gpath, name, value, size);
    if(res == -1)
        return -errno;
    return res;*/
	return 0 ;
}

int synfs_listxattr(const char *path, char *list, size_t size)
{
/*
	printf("synfs_listxattr() %s\n", path ) ;
    int res = llistxattr(gpath, list, size);
    if(res == -1)
        return -errno;
    return res;*/
	return 0 ;
}

int synfs_removexattr(const char *path, const char *name)
{
	/*printf("synfs_removexattr() %s\n", path ) ;
    int res = lremovexattr(gpath, name);
    if(res == -1)
        return -errno;*/
    return 0;
}
#endif /* HAVE_SETXATTR */

static char *
syg_dir()
{
	static char *sygdir ;
	static int sygdirset = 0 ;
	char *envvar ;

	if (sygdirset) {
		return sygdir ;
	}

	envvar = getenv("SYG_DIR") ;
	if (envvar) {
		sygdir = strdup(envvar) ;
	} else {
		sygdir = strdup("/etc/syneredge") ;
	}

	sygdirset = 1 ;

	return sygdir ;
}

static char *
getfbmapdbdir()
{
	/*
	** This hack needs to be cleaned up. This was put in to
	** get snapshots quickly working.
	*/
	static char mapdir[1024] ;

	sprintf(mapdir, "%s/fbmap", syg_dir()) ;
	gpath = strdup(mapdir) ;

	return mapdir ;
}

static int
check_for_mirrors()
{
	MIRRORENTRYP mentry ;
	int count = 0 ;

	mentry = mirrorenv_getnextmirror(mirrors, LSTART) ;
	while(1) {
		if (!mentry) {
			break ;
		}
		count++ ;
		mentry = mirrorenv_getnextmirror(mirrors, LNEXT) ;
	}

	if (! count) {
		printf("No mirrors\n") ;
		exit(1) ;
	}
}

static int
init_fuse_ops()
{
	synfs_oper.getattr	= synfs_getattr ;
	synfs_oper.readlink	= synfs_readlink ;
	synfs_oper.getdir	= synfs_getdir ;
	synfs_oper.mknod	= synfs_mknod ;
	synfs_oper.mkdir	= synfs_mkdir ;
	synfs_oper.symlink	= synfs_symlink ;
	synfs_oper.unlink	= synfs_unlink ;
	synfs_oper.rmdir	= synfs_rmdir ;
	synfs_oper.rename	= synfs_rename ;
	synfs_oper.link		= synfs_link ;
	synfs_oper.chmod	= synfs_chmod ;
	synfs_oper.chown	= synfs_chown ;
	synfs_oper.truncate	= synfs_truncate ;
	synfs_oper.utime	= synfs_utime ;
	synfs_oper.open		= synfs_open ;
	synfs_oper.read		= synfs_read ;
	synfs_oper.write	= synfs_write ;
	synfs_oper.statfs	= synfs_statfs ;
	synfs_oper.flush	= synfs_flush ;
	synfs_oper.release	= synfs_release ;
	synfs_oper.fsync	= synfs_fsync ;
#ifdef HAVE_SETXATTR
	synfs_oper.setxattr	= synfs_setxattr ;
	synfs_oper.getxattr	= synfs_getxattr ;
	synfs_oper.listxattr	= synfs_listxattr ;
	synfs_oper.removexattr	= synfs_removexattr ;
#endif
}

static bool
open_block_files()
{
/*
	MIRRORENTRYP mentry ;
	int count = 0 ;

	mentry = mirrorenv_getnextmirror(mirrors, LSTART) ;
	while(1) {
		if (!mentry) {
			break ;
		}
		BlockFile bf = new BlockFile(mentry->file) ;
		count++ ;
		mentry = mirrorenv_getnextmirror(mirrors, LNEXT) ;
	}

	if (! count) {
		printf("No mirrors\n") ;
		exit(1) ;
	}
*/

	/*
	** This is a hack. This needs to be put into a parameter file.
	*/
#ifdef USE_NETWORK_BLOCKFILE
	Protocol tcp(L"tcp");
	Host hst(L"localhost", false);
	Service serv(L"sygsrv", tcp);

	socket = new TCPServerSocket(serv, false, 10);
	socket->setReuseAddress(true);
	socket->listenSocket();
	cli = socket->acceptSocket();
	cli->setTimeout(10000);
	bf = new NetworkBlockFile((*cli)) ;
#else
	bf = new BlockFile("/tmp/syneredgeblocks1") ;
	bool rc = bf->open() ;
	if (! rc) {
		// Doesn't exist so create
		rc = bf->create(1, 10000, 1024) ;
	}

	if (! rc) {
		return false ;
	}
#endif

	bf->close() ;

	bfio = new BlockFileIO((*bf)) ;

	return true ;
}

int
synfs_init(char *dbpath, char *bdevpath, int bsize, int numblocks)
{
	synfsmapdb = fileblockmapdb_open(getfbmapdbdir(), 1) ;

	if (! synfsmapdb) {
		printf("Failed to open fileblockmap db.\n") ;
		return 0 ;
	}

	//wrbdev = blockdevice_create("/dev/whatever", 1024, 100) ;

	//if (!wrbdev) {
	//	printf("Failed to create block device.\n") ;
	//	return 0 ;
	//}

//	mirrors = mirrorenv_open(syg_dir()) ;
//	if (!mirrors) {
//		printf("mirrorenv_open() failed\n") ;
//		return 0 ;
//	}

	if (! open_block_files() ) {
		printf("Failed to open block file(s).\n") ;
		return 0 ;
	}

	init_fuse_ops() ;

	return 1 ;
}

int
synfs_run(int argc, char **argv)
{
	fuse_main(argc, argv, &synfs_oper);
}

int synfs_shutdown(int freememory)
{
	cout << "synfs_shutdown" << endl ;
	delete bfio ;
	return 1 ;
}

FILEBLOCKMAPP
synfs_getfbmap(char *filename)
{
	FILEBLOCKMAPP fbmap ;

	string f = "/etc/syneredge/fbmap/" ;
	f.append(filename) ;
	cout << "looking for " << f << endl ;
	fbmap = fileblockmapdb_findfileblockmap(synfsmapdb, filename) ;
	return fbmap ;
}
