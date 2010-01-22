
#ifndef __VirtualBlockFile_INCLUDE_
#define __VirtualBlockFile_INCLUDE_

#include <string>
#include <list>
#include <map>
#include "boost/utility.hpp"
#include "iBlockIO.hpp"

using namespace std ;

namespace SynerEdge {

class BlockMapEntry {
public:
	BlockMapEntry(iBlockIO *bf) : blockfile(bf) {} ;
	BlockMapEntry() {}
	iBlockIO *getBlockFile() { return blockfile ; }
private:
	iBlockIO *blockfile ;

} ;

class VirtualBlockFile : public iBlockIO, private boost::noncopyable {

public:
	typedef std::list<iBlockIO *> BlockFileList ;
	typedef std::list<iBlockIO *>::iterator BlockFileListIterator;

	VirtualBlockFile(string name, BlockFileList &bflist, int blocksize) ;
	~VirtualBlockFile() ;
	bool open() ;
	bool close() ;
	uint64 getNumBlocks() ;
	uint64 getNumFreeBlocks() ;
	uint64 getFreeBlock() ;	  // allocates and returns a free block. Block no longer freek
	bool releaseBlock(uint64 blocknum) ; // marks the block free.
	bool flushFreeBlockList() ;
	int getBlockSize() const ;
	bool writeBlock(uint64 blocknum, int offset, int size, char *data) ;
	bool writeBlock(Block &block) ;
	bool readBlock(uint64 blocknum, Block &block) ;
	bool zeroBlock(uint64 blocknum) ;
private:
	bool entryExists(string blockfile) ;
	iBlockIO *findBlockForBlocknum(uint64 blocknum, uint64 *index);


	// Map is simply a list of iBlockIOs that make up
	// the VirtualBlockFile. 

	typedef std::list<iBlockIO *> BlockMap ;
	typedef std::list<iBlockIO *>::iterator BlockMapIterator;
	typedef std::map<uint64, iBlockIO *> BlockRangeList;
	typedef std::map<uint64, iBlockIO *>::iterator BlockRangeListIterator;


	BlockRangeList blockrange;
	BlockRangeListIterator blockrangeIterator;

	string name ;
	int blocksize ;
	bool isopen ;
	uint64 startingblocknum ;
	uint64 endingblocknum ;
	uint64 numblocks;
	BlockMap blockMap ;
} ;

} ; // namespace SynerEdge
#endif // __VirtualBlockFile_INCLUDE_
