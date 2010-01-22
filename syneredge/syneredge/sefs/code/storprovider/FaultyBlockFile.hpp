
#ifndef __FAULTY_BLOCKFILE_INCLUDE_
#define __FAULTY_BLOCKFILE_INCLUDE_

#include <fstream>
#include <string>
#include "SynerEdge.hpp"
#include "Block.hpp"
#include "iBlockIO.hpp"
#include "BlockFile.hpp"

#include "boost/utility.hpp"
#include "boost/thread.hpp"
#include "boost/thread/once.hpp"
#include "boost/thread/mutex.hpp"

using std::string ;
using std::fstream ;

namespace SynerEdge {

	class FaultyBlockFile : public iBlockIO
	{
	public:
		FaultyBlockFile(iBlockIO *bf) ;
		~FaultyBlockFile() ;
		bool open() ;
		bool close() ;
		static bool exists(string blockfilename) ;
		bool create(int64 startingblocknum, int64 numblocks, int blocksize) ;
		string getFilename() const ;
		uint64 getNumBlocks() ;
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

		void enableFailures();
		void disableFailures();

	private:
		iBlockIO *bfp;
		bool failmode;          // true if the BlockFile is acting as a failed physical disk
	};

}; // namespace SynerEdge

#endif /* __FAULTY_BLOCKFILE_INCLUDE_ */
