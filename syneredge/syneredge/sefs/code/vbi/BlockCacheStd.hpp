#ifndef __BlockCacheStd_INCLUDE_
#define __BlockCacheStd_INCLUDE_


#include "BlockCacheBase.hpp"
#include <map>
#include <list>
#include "boost/thread/read_write_mutex.hpp"
#include "boost/thread/mutex.hpp"

/* Author: Atul Prakash */


using namespace std;
class BlockCacheStdUnitTest;

namespace SynerEdge {

	class CacheBlockNode {
	public:

		~CacheBlockNode();
		


		Block *data;
		boost::mutex mutex;
		boost::condition OkToUse;  // block is being read/written.
		bool dirty;              // Is the block dirty in the cache
		bool dirtyWhileDiskWritePending;
		int AW, WW, AR, WR;    // reader-writer status
		// read/written.
		enum IO_STATUS {DISKREADPENDING, DISKWRITEPENDING, OK};
		enum FREE_STATUS {NONE, FREEDELAYED, FREEPENDING, FREEFAILED, FREEDONE};
		IO_STATUS io_status;
		FREE_STATUS free_status;
		// bool toBeFreed;          // indicates that the block should be freed up at the next opportunity.
		// bool free;               // The node has been freed up. It can be reused. Information in it is invalid.
		CacheBlockNode *prev;        // LRU order
		CacheBlockNode *next;
  
	};


	
	//class SynerEdge::BlockCacheStdUnitTest;



	class BlockCacheStd: public BlockCacheBase {
		friend class BlockCacheStdUnitTest;
	public:
		BlockCacheStd(VirtualDisk *disk, unsigned int numBlocks, 
				    unsigned int blockSize): 
			BlockCacheBase(disk, numBlocks, blockSize)
		{
			headPtr = tailPtr = NULL;
			currentCacheSize = 0;

		}
		bool writeBlock(uint64 blockID, unsigned int offset, unsigned 
					 int size, char *data);
		bool writeBlock(Block &block);
		bool readBlock(uint64 blockID, Block &block);
		bool zeroBlock(uint64 blocknum);
		bool releaseBlock(uint64 blocknum);
		void processDirtyBlocks();

		// The following should be private. I need to figure out how to make BlockCacheStdUnitTest a friend
          // of this class so that it can exercise private functions
		private:  
		CacheBlockNode *allocateBlock(); // allocate a block
		CacheBlockNode *lookup(uint64 blockNum);
		int writeBytes(uint64 blockID, unsigned int start, 
					unsigned int numBytes, char *buf);
		int readBytes(uint64 blockID, unsigned int start, 
				    unsigned int numBytes,  char *buf);
		void makeLRUnode(CacheBlockNode *bp);
		void deleteLRUnode(CacheBlockNode *bp);



		// Each Node is placed in a map for fast lookup and on the LRU list for fast deletion
		map<uint64, CacheBlockNode *> bcache;
		CacheBlockNode *headPtr;            // list of Nodes in LRU order. Points to least recently used node.
		CacheBlockNode *tailPtr;           // pointer to most recently used node.
		list<CacheBlockNode *> freelist;               // Free list of nodes.
		unsigned int currentCacheSize;      // Number of current non-free 

		// Use an in-memory cache and a local write-transaction log

		boost::mutex freelistmutex;
		boost::mutex lrumutex;
		boost::mutex bcachemutex;
	};

}; // namespace SynerEdge

#endif // __BlockCacheStd_INCLUDE_
