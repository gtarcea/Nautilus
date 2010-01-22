
#ifndef __NetworkBlockFile_INCLUDE_
#define __NetworkBlockFile_INCLUDE_

#include "SynerEdge.hpp"
#include <Block.hpp>
#include "iBlockIO.hpp"
#include "SocketBase.hpp"
#include "StorProvClntInterface.hpp"
#include "boost/utility.hpp"
#include "boost/thread.hpp"
#include "boost/thread/once.hpp"
#include "boost/thread/mutex.hpp"

namespace SynerEdge {

class NetworkBlockFile : public iBlockIO, private boost::noncopyable {
	public:
		NetworkBlockFile(ClientSocket &sock) ;
		virtual ~NetworkBlockFile() ;

		virtual bool open() ;
		virtual bool close() ;
		virtual uint64 getNumBlocks() ;
		virtual uint64 getNumFreeBlocks() ;
		virtual uint64 getFreeBlock() ;
		virtual bool releaseBlock(uint64 blocknum) ;
		virtual bool flushFreeBlockList() ;
		virtual int getBlockSize() const ;
		virtual bool writeBlock(uint64 blocknum, int offset, int size, char *data) ;
		virtual bool writeBlock(Block &block) ;
		virtual bool readBlock(uint64 blocknum, Block &block) ;
		virtual bool zeroBlock(uint64 blocknum) ;

	private:
		boost::mutex _mtx ;
		uint64 numfreeblocks ;
		uint64 numblocks ;
		uint64 startingblocknum ;
		int blocksize ;
		StorProvClntInterface *sp ;

} ; // class NetworkBlockFile

} ; // namespace SynerEdge

#endif /* __NetworkBlockFile_INCLUDE_ */
