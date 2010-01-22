
#ifndef __StorProvClntInterface_INCLUDE_
#define __StorProvClntInterface_INCLUDE_

#include "SynerEdge.hpp"
#include "BlockFile.hpp"
#include "SocketBase.hpp"
#include "OrbBase.hpp"

namespace SynerEdge {

class StorProvClntInterface
{
public:
	StorProvClntInterface(ClientSocket &socket) ;

        int64 getNumBlocks() ;
        int64 getNumFreeBlocks() ;
        int64 getFreeBlock() ;
	int getBlockSize() ;
        bool releaseBlock(int64 blocknum) ;
        bool flushFreeBlockList() ;
        bool writeBlock(int64 blocknum, int offset, int size, std::string data) ;
        bool readBlock(int64 blocknum, Block &block) ;
        bool zeroBlock(int64 blocknum) ;

private:
	// Helper functions
	void constructIR(InvokeRequest &ir, std::wstring methodName) ;

	ClientSocket &socket ;
	int requestId ;

} ; // class StorProvSvrInterface

} ; // namespace SynerEdge

#endif // __StorProvClntInterface_INCLUDE_
