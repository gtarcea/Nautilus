
#include "StorProvOrb.hpp"
#include "StringConversion.hpp"
#include "PT.hpp"

namespace SynerEdge {

StorProvOrb::StorProvOrb(ClientSocket &sock, StorProvSvrInterface &spsi)
	: socket(sock)
{
	si = new ServerInterface(L"spsi", L"1_0");
	(*si) += new MethodDelegate<StorProvSvrInterface>(L"getNumFreeBlocks", spsi, &StorProvSvrInterface::getNumFreeBlocks);
	(*si) += new MethodDelegate<StorProvSvrInterface>(L"getFreeBlock", spsi, &StorProvSvrInterface::getFreeBlock);
	(*si) += new MethodDelegate<StorProvSvrInterface>(L"releaseBlock", spsi, &StorProvSvrInterface::releaseBlock);
	(*si) += new MethodDelegate<StorProvSvrInterface>(L"flushFreeBlockList", spsi, &StorProvSvrInterface::flushFreeBlockList);
	(*si) += new MethodDelegate<StorProvSvrInterface>(L"writeBlock", spsi, &StorProvSvrInterface::writeBlock);
	(*si) += new MethodDelegate<StorProvSvrInterface>(L"readBlock", spsi, &StorProvSvrInterface::readBlock);
	(*si) += new MethodDelegate<StorProvSvrInterface>(L"zeroBlock", spsi, &StorProvSvrInterface::zeroBlock);
	(*si) += new MethodDelegate<StorProvSvrInterface>(L"getBlockSize", spsi, &StorProvSvrInterface::getBlockSize);
	(*si) += new MethodDelegate<StorProvSvrInterface>(L"getNumBlocks", spsi, &StorProvSvrInterface::getNumBlocks);

	orb = new OrbBase() ;

	(*orb) += (*si) ;
}

StorProvOrb::~StorProvOrb()
{
}

void
StorProvOrb::processCalls()
{
	while (true) {
		// Could multithread here so that each function call runs
		// in a separate thread
		
		XdrStream xd;
		PT("Start socket >> xd") ;
		socket >> xd;
		PT("Past socket >> xd") ;

		InvokeRequest ir;
		PT("Start xd >> ir") ;
		xd >> ir;
		PT("Start xd >> ir") ;

		Context ctx;
		InterfaceVersionPair ivp(ir.interfaceName, ir.versionNumber);
		PT("start orb->find()") ;
		ServerInterface &calledsi = orb->find(ivp);
		PT("past orb->find()") ;
		PT("start calledsi()") ;
		XdrStream *resultstream = calledsi(ir.methodName, ctx, ir.parameters);
		PT("past calledsi()") ;

		InvokeResponse iresp;
		iresp.requestId = ir.requestId;
		iresp.errorMessage = L"Hello";
	
		PT("start iresp.results << *resultstream") ;
		iresp.results = *resultstream;
		PT("past iresp.results << *resultstream") ;

		XdrStream finalresult;
		PT("start finalresult << iresp") ;
		finalresult << iresp;
		PT("past finalresult << iresp") ;
		PT("start socket << finalresult") ;
		socket << finalresult;
		PT("past socket << finalresult") ;
		PT("start delete resultstream") ;
		delete resultstream;
		PT("past delete resultstream") ;
	}

}

bool
StorProvOrb::shutdown()
{
	return false ;
}

} // namespace SynerEdge
