
#include "FileBlockMap.hpp"
#include "fileblockmap.h"
#include "PT.hpp"
#include <iostream>

using namespace std ;

namespace SynerEdge {

FileBlockMap::FileBlockMap(string filename) : fbname(filename), deleted(false), bytecount(0), currentindex(1), rebuildbyindex(false)
{
	int create = 0 ;

//	cout << "FileBlockMap: " << filename << endl ;
	if (! exists(filename)) {
		create = 1 ;
	}

	fbmap = fileblockmap_open(const_cast<char *>(filename.c_str()), create) ;
	if (fbmap == NULL) {
//		cout << "fileblockmap_open() failed" << endl ;
	}

	if (! create) {
		// File exists, so lets populate the hash map
		FILEBLOCKP fblock ;
		fblock = fileblockmap_getnextblock(fbmap, LSTART) ;
		while (1) {
			if (! fblock) {
				break ;
			}
			int blocknum = fileblock_getblocknum(fblock) ;
			FileBlockMapHashVal val(fblock) ;
			blocksetByIndex[currentindex] = val ;
			blocksetByBlocknum[blocknum] = val ;
			bytecount += fileblock_getbytesused(fblock) ;
			currentindex++ ;
			fblock = fileblockmap_getnextblock(fbmap, LNEXT) ;
		}
	}
}

FileBlockMap::~FileBlockMap()
{
	int save = 1 ;

	if ( deleted ) {
		save = 0 ;
	}

	//blocksetByBlocknum.clear() ;
	//blocksetByIndex.clear() ;

	//cout << "Saving fbmap " << fbname << " save flag = " << save << endl ;
	fileblockmap_close(fbmap, save) ;
}

bool
FileBlockMap::save()
{
	int rc ;

	PT("FileBlockMap::save()") ;
	rc = fileblockmap_save(fbmap) ;
	if (rc == 1) {
		PT("Leaving FileBlockMap::save()") ;
		return true ;
	}

	return false ;
}

bool
FileBlockMap::exists(string filename)
{
	int rc ;

	rc = fileblockmap_exists(const_cast<char *>(filename.c_str())) ;

	return (rc == 1) ? true : false ;
}

bool
FileBlockMap::deleteMap()
{
	if (! deleted) {
		deleted = true ;
		int rc = fileblockmap_delete(const_cast<char *>(fbname.c_str())) ;
		return (rc == 1) ? true : false ;
	}

	return false ;
}

bool
FileBlockMap::blocknumExists(int blocknum)
{
	blocksetIterator = blocksetByBlocknum.find(blocknum) ;
	if (blocksetIterator != blocksetByBlocknum.end()) {
		return true ;
	}

	return false ;
}

bool
FileBlockMap::indexExists(int index)
{
	blocksetIterator = blocksetByIndex.find(index) ;
	if (blocksetIterator != blocksetByIndex.end()) {
		return true ;
	}

	return false ;
}

bool
FileBlockMap::addBlock(int blocknum, int bytes)
{
	if (! deleted) {
		if (! blocknumExists(blocknum)) {
			// If blocknum is not already in set then add.
			FILEBLOCKP fblock = fileblockmap_addblock(fbmap, blocknum, bytes) ;
			if (fblock != NULL) {
				FileBlockMapHashVal val(fblock) ;
				blocksetByBlocknum[blocknum] = val ;
				blocksetByIndex[currentindex] = val ;
				currentindex++ ;
				bytecount += bytes ;
				return true ;
			}
		}
	}

	return false ;
}

bool
FileBlockMap::removeBlock(int blocknum)
{
	if (! deleted) {
		if (! blocknumExists(blocknum)) {
			return false ;
		}
		FileBlockMapHashVal val = blocksetByBlocknum[blocknum] ;
		FILEBLOCKP fblock = val.getFileBlock() ;
		int bytes = fileblock_getbytesused(fblock) ;
		int rc = fileblockmap_removeblock(fbmap, blocknum) ;
		if (rc == 1) {
			blocksetByBlocknum.erase(blocknum) ;
			bytecount -= bytes ;
			rebuildbyindex = true ;
			return true ;
		}
	}

	return false ;
}

bool
FileBlockMap::updateBlock(int blocknum, int bytes)
{
	if (! deleted) {
		if ( ! blocknumExists(blocknum)) {
			return false ;
		}
		// To speed up the operation we use the pointer stored in
		// the hash and apply our updates to it.
		FileBlockMapHashVal val = blocksetByBlocknum[blocknum] ;
		FILEBLOCKP fblock = val.getFileBlock() ;
		int oldbytes = fileblock_getbytesused(fblock) ;
		bytecount -= oldbytes ;
		bytecount += bytes ;
		fileblock_setbytesused(fblock, bytes) ;
		return true ;
	}

	return false ;
}

int
FileBlockMap::getBlockByteCount(int blocknum)
{
	if (! blocknumExists(blocknum)) {
		return -1 ;
	}

	FileBlockMapHashVal val = blocksetByBlocknum[blocknum] ;
	FILEBLOCKP fblock = val.getFileBlock();
	return fileblock_getbytesused(fblock) ;
}

bool
FileBlockMap::blockExists(int blocknum)
{
	if (! deleted) {
		return blocknumExists(blocknum) ;
	}

	return false ;
}

int
FileBlockMap::getBlockCount()
{
	if (! deleted) {
		return fileblockmap_getblockcount(fbmap) ;
	}

	return 0 ;
}

int 
FileBlockMap::getByteCount()
{
	if (! deleted) {
		return bytecount ;
	}

	return 0 ;
}

bool
FileBlockMap::addHost(string host)
{
	if (! deleted) {
		int rc = fileblockmap_addhost(fbmap, const_cast<char *>(host.c_str())) ;
		return (rc == 1) ? true : false ;
	}

	return false ;
}

bool
FileBlockMap::removeHost(string host)
{
	if (! deleted) {
		int rc = fileblockmap_removehost(fbmap, const_cast<char *>(host.c_str())) ;
		return (rc == 1) ? true : false ;
	}

	return false ;
}

bool
FileBlockMap::hostExists(string host)
{
	if (! deleted) {
		int rc = fileblockmap_findhost(fbmap, const_cast<char *>(host.c_str())) ;
		return (rc == 1) ? true : false ;
	}

	return false ;
}

int
FileBlockMap::getBlockNumAt(int index)
{
	if (rebuildbyindex) {
		// A block was removed, so we need to rebuild the index first
		blocksetByIndex.clear() ;
		FILEBLOCKP fblock ;
		currentindex = 1 ;
		fblock = fileblockmap_getnextblock(fbmap, LSTART) ;
		while (1) {
			if (! fblock) {
				break ;
			}
			FileBlockMapHashVal val(fblock) ;
			//cout << "Adding to index " << currentindex <<
			//	"The blocknum " << fileblock_getblocknum(fblock) <<
			//	endl ;
			blocksetByIndex[currentindex] = val ;
			currentindex++ ;
			fblock = fileblockmap_getnextblock(fbmap, LNEXT) ;
		}
		rebuildbyindex = false ;
	}

	if (! indexExists(index)) {
		return 0 ;
	}

	FileBlockMapHashVal val = blocksetByIndex[index] ;

	return val.getBlockNum() ;
}

} // namespace SynerEdge
