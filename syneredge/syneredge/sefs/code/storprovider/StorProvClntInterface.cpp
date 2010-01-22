#include "XdrStream.hpp"
#include "OrbBase.hpp"
#include "StorProvClntInterface.hpp"
#include "StringConversion.hpp"
#include "PT.hpp"

namespace SynerEdge {

StorProvClntInterface::StorProvClntInterface(ClientSocket &socket)
		: socket(socket), requestId(1)
{
}

void
StorProvClntInterface::constructIR(InvokeRequest &ir, std::wstring methodName)
{
	ir.requestId = requestId++ ;
	ir.interfaceName = L"spsi" ;
	ir.versionNumber = L"1_0" ;
	ir.methodName = methodName ;
}

#define DECLAREVARS() \
	XdrStream xd ; \
	InvokeRequest ir ; \
	XdrStream xdresult ; \
	InvokeResponse resp ; \
	int len ; \
	int len2 ;

#define DOREQUEST(name) \
	PT("      start constructIR") ;\
	constructIR(ir, name) ; \
	PT("      past constructIR") ;\
	PT("      start xd << ir") ; \
	xd << ir ; \
	PT("      past xd << ir") ; \
	PT("      start socket << xd") ; \
	socket << xd ; \
	PT("      past socket << xd") ; \
	PT("      start socket >> xdresult") ; \
	socket >> xdresult ; \
	PT("      past socket >> xdresult") ; \
	PT("      start xdresult >> resp") ; \
	xdresult >> resp ; 
	

int64 
StorProvClntInterface::getNumBlocks()
{
	DECLAREVARS()
	DOREQUEST(L"getNumBlocks")

	int64 numblocks ;
	resp.results >> numblocks ;

	return numblocks ;
}

int64 
StorProvClntInterface::getNumFreeBlocks()
{
	DECLAREVARS()
	DOREQUEST(L"getNumFreeBlocks")

	int64 numfreeblocks ;
	resp.results >> numfreeblocks ;

	return numfreeblocks ;
}

int
StorProvClntInterface::getBlockSize()
{
	DECLAREVARS()
	DOREQUEST(L"getBlockSize")

	int blocksize ;
	resp.results >> blocksize ;

	return blocksize ;
}

int64 
StorProvClntInterface::getFreeBlock()
{
	PT("Start getFreeBlock") ;
	PT("  Start DECLAREVARS()") ;
	DECLAREVARS()
	PT("  End DECLAREVARS()") ;
	PT("  START DOREQUEST") ;
	DOREQUEST(L"getFreeBlock")
	PT("  END DOREQUEST") ;

	int64 blocktouse ;
	resp.results >> blocktouse ;
	PT("End getFreeBlock") ;

	return blocktouse ;
}

bool 
StorProvClntInterface::releaseBlock(int64 blocknum)
{
	DECLAREVARS()

	ir.parameters << blocknum ;

	DOREQUEST(L"releaseBlock")

	bool rc ;
	resp.results >> rc ;

	return rc ;
}

bool 
StorProvClntInterface::flushFreeBlockList()
{
	DECLAREVARS()
	DOREQUEST(L"flushFreeBlockList")

	bool rc ;
	resp.results >> rc ;

	return rc ;
}

bool 
StorProvClntInterface::writeBlock(int64 blocknum, int offset, int size, string data)
{
	DECLAREVARS()

	ir.parameters << blocknum << offset << size << data ;

	DOREQUEST(L"writeBlock")

	bool rc ;
	resp.results >> rc ;

	return rc ;
}

bool 
StorProvClntInterface::readBlock(int64 blocknum, Block &block)
{
	DECLAREVARS()

	ir.parameters << blocknum ;

	DOREQUEST(L"readBlock") ;
	bool rc ;
	resp.results >> rc ;
	if (! rc) {
		return false ;
	}
	int64 blocknum2 ;
 	resp.results >> blocknum2 ;
	//bool isfree ;
	//resp.results >>  isfree ;
	int numbytes ;
	resp.results >> numbytes ;
	int size ;
	resp.results >> size ;
	string data ;
	resp.results >> data ;

	block.setSize(size) ;
	block.setBytes(0, const_cast<char *>(data.c_str()), numbytes) ;
	block.setNumBytes(numbytes) ;
	//block.setIsFree(isfree) ;
	block.setBlockNum(blocknum2) ;

	return rc ;
}

bool 
StorProvClntInterface::zeroBlock(int64 blocknum)
{
	DECLAREVARS()

	ir.parameters << blocknum ;

	DOREQUEST(L"zeroBlock") ;

	bool rc ;
	resp.results >> rc ;

	return rc ;
}

} // namespace SynerEdge
