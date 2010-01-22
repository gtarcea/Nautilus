
#include "VirtualBlockFile.hpp"

/* Author:  Glenn Tarcea 
           Created initial version that worked with BlockFile (not NetworkBlockFile).
		 Assumed that BlockFiles knew their block number range.
 * Modifications: Atul Prakash
           Changed it to so that it works with any time of iBlockIO.
		 iBlockIO does not return startingBlockNum(), like BlockFile.
		 This required significant changes to the code so that mapping
		 information from blockid to blockfilebase is stored in this class.
		 Each iBlockIO is assumed to start from Block Number 1 now.

		 Also, changed the BlockFileList so that you pass in any type
		 of iBlockIOs, rather than pathnames of blockfiles.
*/		 
	  
		 
 

namespace SynerEdge {

VirtualBlockFile::VirtualBlockFile(string name, BlockFileList &bflist, int blocksize) 
		: name(name), blocksize(blocksize), startingblocknum(1),
		  endingblocknum(0), isopen(false)
{
	int blockcount = 0;
	for ( BlockFileListIterator itor = bflist.begin() ;
			itor != bflist.end() ; itor++) {
		iBlockIO *bf = (*itor) ;
		blockMap.push_back(bf);
	}
}

VirtualBlockFile::~VirtualBlockFile()
{
	// cout << "VirtualBlockFile::~VirtualBlockFile() Called" << endl ;
}

bool
VirtualBlockFile::open()
{

	// Open also creates a mapping from blockids to individual
     // blockfiles. An assumption is the number of blocks in
     // a blockfile does not change after a blockfile is made
	// part of a virtual block file. 

	// Dynamic administrative changes have to be dealt with later.
	// That is like repartitioning the disk or changing the size 
	// of a partition and must be done very carefully after more research.

	iBlockIO *blockfb;
	uint64 blockcount = 0;
	for (BlockMapIterator itor = blockMap.begin() ;
			itor != blockMap.end() ; itor++) {
		
		blockfb = (*itor);
		bool opened = blockfb->open() ;
		if (! opened) {
			return false ;
		}

		uint bfblocksize = blockfb->getBlockSize() ;
		uint64 bfnumblocks = blockfb->getNumBlocks();
		if (bfblocksize != blocksize) {
			return false ;
		}
		blockcount += bfnumblocks;
		if (blockcount+1 < bfnumblocks) { // wrap around 
			assert(false);
			return (false);
		}
		blockrange[blockcount+1] = blockfb;
	}
	numblocks = blockcount;
	isopen = true ;

	return true ;
}

bool
VirtualBlockFile::close()
{
	if (! isopen) {
		return true ;
	}

	for (BlockMapIterator itor = blockMap.begin() ;
			itor != blockMap.end() ; itor++) {
		(*itor)->close() ;
	}

	return true ;
}

uint64
VirtualBlockFile::getNumBlocks()
{
	return numblocks;
	/*
	for ( BlockMapIterator itor = blockMap.begin() ;
			itor != blockMap.end() ; itor++) {
		numblocks += (*itor).second.getBlockFile()->getNumBlocks() ;
	}

	return numblocks ;
	*/
}

uint64 
VirtualBlockFile::getNumFreeBlocks()
{
	uint64 numfreeblocks = 0 ;

	for ( BlockMapIterator itor = blockMap.begin() ;
			itor != blockMap.end() ; itor++) {
		numfreeblocks += (*itor)->getNumFreeBlocks() ;
	}

	return numfreeblocks ;
}

uint64 
VirtualBlockFile::getFreeBlock()
{
	for ( BlockMapIterator itor = blockMap.begin() ;
			itor != blockMap.end() ; itor++) {
		uint64 freeblocknum = (*itor)->getFreeBlock() ;
		if (freeblocknum != 0) {
			return freeblocknum ;
		}
	}

	// If we get here then we are out of space.
	return 0 ;
}


iBlockIO *
VirtualBlockFile::findBlockForBlocknum(uint64 blocknum, uint64 *index)
{

	BlockRangeListIterator prevnode;
	if (blocknum <= 0 || blocknum > numblocks) return NULL;
	// Find the element corresponding to the largest key such that the key < blocknum
	blockrangeIterator = blockrange.upper_bound(blocknum);
	if (blockrangeIterator == blockrange.end()) { // past the end of last node
		*index = 0;
		return NULL;
	} else if (blockrangeIterator == blockrange.begin()) { // first node
		*index = blocknum;
		return (*blockrangeIterator).second;
	} else {   // got to figure out the relative address with respect to previous block
		prevnode = blockrangeIterator;
		prevnode--;
		// e.g., two disks with 100 blocks would have 101 and 201 in blockrange.
		// lookup of 101 should compute to 101 - (101 - 1) or 1 in the 2nd disk..
		*index = blocknum - ((*prevnode).first - 1) ;  
		return (*blockrangeIterator).second;
	}
}


bool 
VirtualBlockFile::releaseBlock(uint64 blocknum)
{
	uint64 index;
	iBlockIO *fb = findBlockForBlocknum(blocknum, &index);
	if (!fb) return false;
	return fb->releaseBlock(index);
}

bool 
VirtualBlockFile::flushFreeBlockList()
{
	bool allsucceeded = true ;
	for ( BlockMapIterator itor = blockMap.begin() ;
			itor != blockMap.end() ; itor++) {
		bool rc = (*itor)->flushFreeBlockList() ;
		if (! rc) {
			allsucceeded = false ;
		}
	}

	return allsucceeded ;
}

int 
VirtualBlockFile::getBlockSize() const
{
	return blocksize ;
}

bool 
VirtualBlockFile::writeBlock(uint64 blocknum, int offset, int size, char *data)
{
	uint64 index;
	iBlockIO *fb = findBlockForBlocknum(blocknum, &index);
	if (!fb) return false;
	return fb->writeBlock(index,offset,size,data);
}

bool 
VirtualBlockFile::writeBlock(Block &block)
{
	uint64 index;
	uint64 blocknum = block.getBlockNum();
	iBlockIO *fb = findBlockForBlocknum(blocknum, &index);
	if (!fb) return false;
	// Map the block numbers to the individal disk
	uint64 blockid = block.getBlockNum();
	block.setBlockNum(index);
	bool returnval = fb->writeBlock(block);
	block.setBlockNum(blockid);
	return returnval;
}


bool 
VirtualBlockFile::readBlock(uint64 blocknum, Block &block)
{
	uint64 index;
	iBlockIO *fb = findBlockForBlocknum(blocknum, &index);
	if (!fb) return false;
	// Map the block numbers to the individal disk
	bool returnval = fb->readBlock(index, block);
	block.setBlockNum(blocknum);
	return returnval;
}


bool 
VirtualBlockFile::zeroBlock(uint64 blocknum)
{
	uint64 index;
	iBlockIO *fb = findBlockForBlocknum(blocknum, &index);
	if (!fb) return false;
	// Map the block numbers to the individal disk
	return fb->zeroBlock(index);
}


} // Namespace SynerEdge
