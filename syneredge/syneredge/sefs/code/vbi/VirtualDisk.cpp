#include "VirtualDisk.hpp"

/* 
 * Author: Glenn Tarcea
 * Modified by: Atul Prakash to add caching functionality.
 */

namespace SynerEdge {

VirtualDisk::VirtualDisk(string name, int blocksize, VirtualBlockFileList &vbfilelist)
	: name(name), blocksize(blocksize),isopen(false),
		blockonwrite(false)
{
	useCache = true;           // For debugging. To diskable cache use, set to false.
	blockcache = new BlockCacheStd(this, cachesize, blocksize);
	for (VirtualBlockFileListIterator itor = vbfilelist.begin() ;
			itor != vbfilelist.end() ; itor++) {
		vbflist.push_back(*itor);
	}
}

VirtualDisk::~VirtualDisk()
{
}


bool
VirtualDisk::open()
{
	if (isopen) return true;
	for (VirtualBlockFileListIterator itor = vbflist.begin() ;
			itor != vbflist.end() ; itor++) {
		bool opened = (*itor)->open() ;
		if (! opened) {
			return false ;
		}

		int bfblocksize = (*itor)->getBlockSize() ;
		if (bfblocksize != blocksize) {
			return false ;
		}
	}

	isopen = true ;

	return true ;
}

bool 
VirtualDisk::close()
{
	if (! isopen) {
		return true ;
	}

	for (VirtualBlockFileListIterator itor = vbflist.begin() ;
			itor != vbflist.end() ; itor++) {
		(*itor)->close() ;
	}
	isopen = false;

	return true ;
}

uint64 
VirtualDisk::getNumBlocks()
{
	VirtualBlockFileListIterator itor = vbflist.begin() ;

	return (*itor)->getNumBlocks() ;
}

uint64 
VirtualDisk::getNumFreeBlocks()
{
	VirtualBlockFileListIterator itor = vbflist.begin() ;
	uint64 returnval = (*itor)->getNumFreeBlocks() ;
	return returnval;
}

uint64 
VirtualDisk::getFreeBlock()
{
	VirtualBlockFileListIterator itor = vbflist.begin() ;

	return (*itor)->getFreeBlock() ; // How do we allocate blocks across mirrors?
}

//
// For the moment lets assume all operations succeed
//

#define DO_OPERATION(a) \
        for ( VirtualBlockFileListIterator itor = vbflist.begin() ; \
                        itor != vbflist.end() ; itor++) {\
		(*itor)->a ; \
	}


bool 
VirtualDisk::disk_releaseBlock(uint64 blocknum)
{
	DO_OPERATION(releaseBlock(blocknum))

	return true ;
}

bool 
VirtualDisk::flushFreeBlockList()
{
	DO_OPERATION(flushFreeBlockList())

	return true ;
}

int 
VirtualDisk::getBlockSize() const
{
	return blocksize ;
}



bool 
VirtualDisk::writeBlock(uint64 blocknum, int offset, int size, char *data)
{
	if (useCache) {
		return blockcache->writeBlock(blocknum, offset, size, data);
	} else {
		return disk_writeBlock(blocknum, offset, size, data);
	}
}

bool VirtualDisk::writeBlock(Block &block)
{
	if (useCache) {
		return blockcache->writeBlock(block);
	} else {
		return disk_writeBlock(block);
	}
}

bool VirtualDisk::readBlock(uint64 blocknum, Block &block)
{
	if (useCache) {
		return blockcache->readBlock(blocknum, block);
	} else {
		return disk_readBlock(blocknum, block);
	}
}

bool 
VirtualDisk::zeroBlock(uint64 blocknum)
{

	if (useCache) {
		return blockcache->zeroBlock(blocknum);
	} else {
		return disk_zeroBlock(blocknum);
	}
}

bool
VirtualDisk::validateAccess(uint64 blocknum)
{

	if (blocknum <= 0 || blocknum > getNumBlocks())
		return false;
	else return true;
}

bool 
VirtualDisk::releaseBlock(uint64 blocknum)
{
	if (!validateAccess(blocknum)) {
		return false;
	}

	if (useCache) {
		return blockcache->releaseBlock(blocknum);
	} else {
		return disk_releaseBlock(blocknum);
	}
}

bool 
VirtualDisk::disk_writeBlock(uint64 blocknum, int offset, int size, char *data)
{
	DO_OPERATION(writeBlock(blocknum,offset,size,data))

	return true ;
}

bool 
VirtualDisk::disk_writeBlock(Block &block)
{
	DO_OPERATION(writeBlock(block))

	return true ;
}

bool 
VirtualDisk::disk_readBlock(uint64 blocknum, Block &block)
{
        for ( VirtualBlockFileListIterator itor = vbflist.begin() ;
                        itor != vbflist.end() ; itor++) {
		bool status = (*itor)->readBlock(blocknum, block) ; 
		if (status) {
			return true ;
		}
	}

	return false ;
}

bool 
VirtualDisk::disk_zeroBlock(uint64 blocknum)
{
	DO_OPERATION(zeroBlock(blocknum))

	return true ;
}

void 
VirtualDisk::setMirrorBlocking(bool blockonwrite)
{
	this->blockonwrite = blockonwrite ;
}

bool 
VirtualDisk::getMirrorBlocking()
{
	return blockonwrite ;
}

bool 
VirtualDisk::repair()
{
	return false ;
}

} // namespace SynerEdge
