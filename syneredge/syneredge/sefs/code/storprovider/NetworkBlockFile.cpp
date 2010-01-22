
#include "NetworkBlockFile.hpp"
#include "StorProvClntInterface.hpp"
#include "PT.hpp"

using namespace std ;
namespace SynerEdge {

int blocksize = -1 ;

NetworkBlockFile::NetworkBlockFile(ClientSocket &sock)
{
	sp = new StorProvClntInterface(sock) ;
	blocksize = sp->getBlockSize() ;
}

NetworkBlockFile::~NetworkBlockFile()
{
	// Not sure what to do here...
}

bool 
NetworkBlockFile::open()
{
	return true ; // Nothing to do?
}

bool 
NetworkBlockFile::close()
{
	// Nothing to do?
	return true ;
}

uint64 
NetworkBlockFile::getNumBlocks()
{
	return sp->getNumBlocks() ;
}

uint64
NetworkBlockFile::getNumFreeBlocks()
{
	return sp->getNumFreeBlocks() ;
}

uint64 
NetworkBlockFile::getFreeBlock()
{
	uint64 blocknum ;
	PT("Entering NetworkingBlockFile::getFreeBlock()") ;
	blocknum = sp->getFreeBlock() ;
	PT("Leaving NetworkingBlockFile::getFreeBlock()") ;
	return blocknum ;
}

bool 
NetworkBlockFile::releaseBlock(uint64 blocknum)
{
	return sp->releaseBlock(blocknum) ;
}

bool 
NetworkBlockFile::flushFreeBlockList()
{
	return sp->flushFreeBlockList() ;
}

int 
NetworkBlockFile::getBlockSize() const
{
	return blocksize ;
}

bool 
NetworkBlockFile::writeBlock(uint64 blocknum, int offset, int size, char *data)
{
	// ?? what to do about conversion to string...
	return sp->writeBlock(blocknum, offset, size, string(data, size)) ;
}

bool 
NetworkBlockFile::writeBlock(Block &block)
{
	return sp->writeBlock(block.getBlockNum(), 0, block.getNumBytes(),
			string(block.getBytes(), block.getNumBytes()) ) ;
}

bool 
NetworkBlockFile::readBlock(uint64 blocknum, Block &block)
{
	return sp->readBlock(blocknum, block) ;
}

bool 
NetworkBlockFile::zeroBlock(uint64 blocknum)
{
	return sp->zeroBlock(blocknum) ;
}

} // namespace SynerEdge
