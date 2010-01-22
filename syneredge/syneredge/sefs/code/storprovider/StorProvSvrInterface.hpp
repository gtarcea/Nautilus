
#ifndef __StorProvSvrInterface_INCLUDE_
#define __StorProvSvrInterface_INCLUDE_

#include "SynerEdge.hpp"
#include "BlockFile.hpp"
#include "XdrStream.hpp"
#include "OrbBase.hpp"

namespace SynerEdge {

class StorProvSvrInterface
{
public:
	StorProvSvrInterface(BlockFile &bf) ;

        XdrStream *getNumBlocks(Context &ctx, XdrStream &xdrs) ;
        XdrStream *getNumFreeBlocks(Context &ctx, XdrStream &xdrs) ;
        XdrStream *getFreeBlock(Context &ctx, XdrStream &xdrs) ;
        XdrStream *getBlockSize(Context &ctx, XdrStream &xdrs) ;
        XdrStream *releaseBlock(Context &ctx, XdrStream &xdrs) ;
        XdrStream *flushFreeBlockList(Context &ctx, XdrStream &xdrs) ;
        XdrStream *writeBlock(Context &ctx, XdrStream &xdrs) ;
        XdrStream *readBlock(Context &ctx, XdrStream &xdrs) ;
        XdrStream *zeroBlock(Context &ctx, XdrStream &xdrs) ;

private:
	BlockFile &bf ;
	int blockSize ;

} ; // class StorProvSvrInterface

} ; // namespace SynerEdge

#endif // __StorProvSvrInterface_INCLUDE_
