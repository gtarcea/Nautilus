
#include <unistd.h>
#include <stdio.h>

static int
lock_block_device()
{
	return 1 ;
}

static int
unlock_block_device()
{
	return 1 ;
}

static int
make_snapshot(char *fsdb, char *snapname)
{
	char command[1024] ;
	FILE *fp ;
	char *smbconf = "/etc/samba/smb.conf" ;

	fp = fopen("/tmp/snap.sh", "w+") ;

	fputs("#!/bin/sh\n", fp) ;

	/*
	** Step 1: Make snapshot db, and copy over fbmapdir.
	*/
	sprintf(command, "mkdir -p /etc/syneredge/%s/fbmap > /dev/null 2>&1\n", snapname) ;
	fputs(command, fp) ;
	sprintf(command, "cp %s/fbmap/* /etc/syneredge/%s/fbmap > /dev/null 2>&1\n", fsdb, snapname) ;
	fputs(command, fp) ;

	/*
	** Step 2: Create snapshot blockdir and copy over blocks.
	*/
	sprintf(command, "mkdir /tmp/%sblocks > /dev/null 2>&1\n", snapname) ;
	fputs(command, fp) ;
	sprintf(command, "echo spelljammer /tmp/%sblocks > /etc/syneredge/%s/mirrors\n",
		snapname, snapname) ;
	fputs(command, fp) ;
	sprintf(command, "BLOCKDIR=`head -1 %s/mirrors | cut -d' ' -f2`\n", fsdb) ;
	fputs(command, fp) ;
	sprintf(command, "cp $BLOCKDIR/* /tmp/%sblocks\n", snapname) ;
	fputs(command, fp) ;
	
	/*
	** Step 3: Update /etc/samba/smb.conf
	*/
	sprintf(command, "echo '[%s]' >> %s\n", snapname, smbconf) ;
	fputs(command, fp) ;
	sprintf(command, "echo comment = iSAN FS Snapshot %s >> %s\n", snapname, smbconf) ;
	fputs(command, fp) ;
	sprintf(command, "echo path = /tmp/%s >> %s\n", snapname, smbconf) ;
	fputs(command, fp) ;
	sprintf(command, "echo public = yes >> %s\n", smbconf) ;
	fputs(command, fp) ;
	sprintf(command, "echo writable = no >> %s\n", smbconf) ;
	fputs(command, fp) ;
	sprintf(command, "echo force user = root >> %s\n", smbconf) ;
	fputs(command, fp) ;

	/*
	** Tell smbd to re-read the config files.
	*/
	fputs("echo \"`ps -ef | grep smbd$ | grep -v grep | awk '{print $2}'`\" | while read line\n", fp) ;
	fputs("do\n", fp) ;
	fputs("kill -HUP $line\n", fp) ;
	fputs("done\n", fp) ;

	/*
	** Start filesystem of snapshot (need to set path).
	*/
	fputs("PATH=$PATH:/filesafe/fs\n", fp) ;
	fputs("export PATH\n", fp) ;
	sprintf(command, "SYG_DIR=/etc/syneredge/%s\n", snapname) ;
	fputs(command, fp) ;
	fputs("export SYG_DIR\n", fp) ;
	sprintf(command, "mkdir /tmp/%s > /dev/null 2>&1\n", snapname) ;
	fputs(command, fp) ;
	sprintf(command, "synfs /tmp/%s\n", snapname) ;
	fputs(command, fp) ;

	fclose(fp) ;
	system("chmod +x /tmp/snap.sh ; /tmp/snap.sh") ;
}

static int
usage()
{
	printf("sygsnap -f <fsdb> -s <snapshot name>\n") ;
	exit(1) ;
}

static int
start_new_fs(char *snapname)
{
}

main(int argc, char **argv)
{
	int c ;
	char *fsdbdir = NULL ;
	char *snapname = NULL ;

	while ( (c = getopt(argc, argv, "?s:f:")) != EOF) {
		switch (c) {
			case 'f':
				fsdbdir = strdup(optarg) ;
				break ;
			case 's':
				snapname = strdup(optarg) ;
				break ;
			case '?':
			default:
				usage() ;
		}
	}

	if ((! fsdbdir) || (! snapname)) {
		usage() ;
	}

	lock_block_device() ;
	make_snapshot(fsdbdir, snapname) ;
	unlock_block_device() ;
	start_new_fs(snapname) ;
}
