
#include "StorProvSvrInterface.hpp"
#include <iostream>

using namespace std ;

namespace SynerEdge {

StorProvSvrInterface::StorProvSvrInterface(BlockFile &bf)
	: bf(bf)
{
	blockSize = bf.getBlockSize() ;
}

XdrStream *
StorProvSvrInterface::getNumBlocks(Context &ctx, XdrStream &xdrs)
{
	int64 result;
	XdrStream *res = new XdrStream();
	result =  bf.getNumBlocks() ;
	(*res) << result;
	return res;
}

XdrStream *
StorProvSvrInterface::getBlockSize(Context &ctx, XdrStream &xdrs)
{
	int result ;
	XdrStream *res = new XdrStream() ;
	result = bf.getBlockSize() ;
	(*res) << result ;
	return res ;
}

XdrStream *
StorProvSvrInterface::getNumFreeBlocks(Context &ctx, XdrStream &xdrs)
{
	int64 result;
	XdrStream *res = new XdrStream();
	result =  bf.getNumFreeBlocks() ;
	(*res) << result;
	return res;
}

XdrStream *
StorProvSvrInterface::getFreeBlock(Context &ctx, XdrStream &xdrs)
{
	int64 result;
	XdrStream *res = new XdrStream();
	result =  bf.getFreeBlock();
	(*res) << result;
	return res;
}

XdrStream *
StorProvSvrInterface::releaseBlock(Context &ctx, XdrStream &xdrs)
{
	int len ;
	int64 blocknum;
	bool result;
	XdrStream *res = new XdrStream();
	//xdrs >> len ;
	//cout << "len = " << len << endl ;
	xdrs >> blocknum;
	result =  bf.releaseBlock(blocknum);
	(*res) << result;
	return res;
}

XdrStream *
StorProvSvrInterface::flushFreeBlockList(Context &ctx, XdrStream &xdrs)
{
	bool result;
	XdrStream *res = new XdrStream();
	result =  bf.flushFreeBlockList();
	(*res) << result;
	return res;
}

XdrStream *
StorProvSvrInterface::writeBlock(Context &ctx, XdrStream &xdrs)
{
	int len ;
	int64 blocknum;
	int offset ;
	int size ;
	string data ;
	bool result;
	XdrStream *res = new XdrStream();
//	xdrs >> len ;
	//cout << "len = " << len << endl ;
	xdrs >> blocknum ;
//	cout << "blocknum = " << blocknum << endl ;
	xdrs >> offset ;
//	cout << "offset = " << offset << endl ;
	xdrs >> size ;
//	cout << "size = " << size << endl ;
	xdrs >> data ;
	result =  bf.writeBlock(blocknum, offset, size, const_cast<char *>(data.c_str()));
	(*res) << result;
	return res;
}

XdrStream *
StorProvSvrInterface::readBlock(Context &ctx, XdrStream &xdrs)
{
	int len ;
	int64 blocknum;
	int result;
	Block block(blockSize) ;
	XdrStream *res = new XdrStream();
	//xdrs >> len ;
	xdrs >> blocknum;
	result =  bf.readBlock(blocknum, block);
	(*res) << result;
	if (result) {
	//	cout << "result is true" << endl ;
		(*res) << blocknum ;
	//	(*res) << block.isFree() ;
		(*res) << block.getNumBytes() ;
		(*res) << block.getSize() ;
		(*res) << string(block.getBytes()) ;
	}
	return res;
}

XdrStream *
StorProvSvrInterface::zeroBlock(Context &ctx, XdrStream &xdrs)
{
	int len ;
	int64 blocknum ;
	int result;
	XdrStream *res = new XdrStream();
	//xdrs >> len ;
	xdrs >> blocknum;
	result =  bf.zeroBlock(blocknum);
	(*res) << result;
	return res;
}

} // namespace SynerEdge
