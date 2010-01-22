#ifndef __BlockCacheBase_INCLUDE_
#define __BlockCacheBase_INCLUDE_


#include "SynerEdge.hpp"
#include "Block.hpp"

typedef unsigned long long uint64;


using namespace std;

namespace SynerEdge {
	class VirtualDisk;

	class BlockCacheBase {
	public:
		BlockCacheBase(VirtualDisk *disk, unsigned int numBlocks, 
					unsigned int blockSize): 
			numBlocks(numBlocks), blockSize(blockSize), disk(disk)  
		{
		}
	
  
		virtual bool writeBlock(uint64 blockID, unsigned int offset, 
						    unsigned int size, char *data) = 0;
		virtual bool writeBlock(Block &block) = 0;
		virtual bool readBlock(uint64 blockID, Block &block) = 0;
		virtual bool zeroBlock(uint64 blocknum) = 0;
		virtual bool releaseBlock(uint64 blocknum) = 0;
		virtual void processDirtyBlocks() = 0;

	protected: // These are fronts for operations on the virtual disk
		virtual bool disk_writeBlock(uint64 blockID, unsigned int offset, 
							    unsigned int size, char *data);
		virtual bool disk_writeBlock(Block &block);
		virtual bool disk_readBlock(uint64 blocknum, Block &block);
		virtual bool disk_zeroBlock(uint64 blocknum);
		virtual bool disk_releaseBlock(uint64 blocknum);

		unsigned int numBlocks;
		unsigned int blockSize;
		VirtualDisk *disk;
	};

}; // namespace SynerEdge
#endif //__BlockCacheBase_INCLUDE_
