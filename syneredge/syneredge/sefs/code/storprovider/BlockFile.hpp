
#ifndef __BLOCKFILE_INCLUDE_
#define __BLOCKFILE_INCLUDE_

#include "SynerEdge.hpp"
#include "FreeBlockMap.hpp"
#include <fstream>
#include <string>
#include <Block.hpp>
#include "iBlockIO.hpp"

#include "boost/utility.hpp"
#include "boost/thread.hpp"
#include "boost/thread/once.hpp"
#include "boost/thread/mutex.hpp"

using std::string ;
using std::fstream ;

namespace SynerEdge {

	class BlockFile : public iBlockIO, private boost::noncopyable 
	{
	public:
		BlockFile(string filename) ;
		//		BlockFile() {}
		~BlockFile() ;

		bool open() ;
		bool close() ;
		static bool exists(string blockfilename) ;
		bool create(uint64 startingblocknum, uint64 numblocks, int blocksize) ;
		string getFilename() const ;
		uint64 getNumBlocks() ;
		uint64 getStartingBlockNum() const ;
		uint64 getNumFreeBlocks() ;
		uint64 getFreeBlock() ; // allocates a block, unfreeing it.
		bool releaseBlock(uint64 blocknum) ; // frees up the block
		bool flushFreeBlockList() ;
		int getBlockSize() const ;
		bool writeBlock(uint64 blocknum, int offset, int size, char *data) ; // returns false if block is free or doesn't exist
		bool writeBlock(Block &block) ; // returns false if block is free or doesn't exist
		bool readBlock(uint64 blocknum, Block &block) ; // returns false if block is free or doesn't exist
		bool zeroBlock(uint64 blocknum) ;
		//void printStuff() ;

private:
		// Help functions
		bool validateAccess(uint64 blocknum) ;
		bool validateAccessAndCheckNotFree(uint64 blocknum);
		bool flushFreeBlockList(bool lock) ;

		// data members
		uint64 numfreeblocks ;      // This is fake stuff. freeblockmap is the real stuff.
		uint64 numblocks ;
		uint64 startingblocknum ;
		int blocksize ;
		string filename ;
		fstream file ;
		FreeBlockMap *freeblockmap;
		fstream freeblockmapfile ;

		boost::mutex _mtx ;

		static const int headersize = sizeof(uint64)*3 +sizeof(int);
} ;

} ; // namespace SynerEdge

#endif /* __BLOCKFILE_INCLUDE_ */
