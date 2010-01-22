
#ifndef __iBlockIO_INCLUDE_
#define __iBlockIO_INCLUDE_

#include "SynerEdge.hpp"
#include <Block.hpp>

namespace SynerEdge {

class iBlockIO {
	public:
		iBlockIO() {} ;
		virtual ~iBlockIO() {} ;

		virtual bool open() = 0 ;
		virtual bool close() = 0 ;
		virtual uint64 getNumBlocks() = 0 ;
		virtual uint64 getNumFreeBlocks() = 0 ;
		virtual uint64 getFreeBlock() = 0 ;
		virtual bool releaseBlock(uint64 blocknum) = 0 ;
		virtual bool flushFreeBlockList() = 0 ;
		virtual int getBlockSize() const = 0 ;
		virtual bool writeBlock(uint64 blocknum, int offset, int size, char *data) = 0 ;
		virtual bool writeBlock(Block &block) = 0 ;
		virtual bool readBlock(uint64 blocknum, Block &block) = 0 ;
		virtual bool zeroBlock(uint64 blocknum) = 0 ;

} ;

} ; // namespace SynerEdge

#endif /* __iBlockIO_INCLUDE_ */
