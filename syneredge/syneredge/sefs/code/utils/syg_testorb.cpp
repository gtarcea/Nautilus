#include "SynerEdge.hpp"
#include "OrbBase.hpp"
#include "XdrStream.hpp"
#include "boost/format.hpp"
#include "boost/thread/thread.hpp"
#include "boost/thread/mutex.hpp"
#include "boost/thread/condition.hpp"

using namespace SynerEdge;

class MyServerInterface
{
public:
	XdrStream *func(Context &ctx, XdrStream &xdrs)
	{
		std::wcout << "Inside of func()" << std::endl ;
		int len ;
		int i;
		int result;
		XdrStream *res = new XdrStream();	
		xdrs >> len ;
		xdrs >> i;
		std::wcout << "i = " << i << std::endl ;
		result =  realfunc(i);
		(*res) << result;
		return res;
	}	

	int realfunc(int i)
	{
		std::wcout << "Called realfunc!" << std::endl ;
		std::wcout << "Returning " << (i*52) << std::endl ;
		return (i * 52);
	}
};

int main(int argc, char **argv)
{
        try
        {
		MyServerInterface serverif;
		ServerInterface si(L"si", L"0_1");
		//std::wcout << L"before deletegate add" << std::endl;
		si += new MethodDelegate<MyServerInterface>(L"func", serverif, &MyServerInterface::func);
		//std::wcout << L"after deletegate add" << std::endl;

		OrbBase orb;
		//std::wcout << "Before si add" << std::endl;
		orb += si;
		//std::wcout << "After si add" << std::endl;
	
                Protocol tcp(L"tcp");
                Host hst(L"localhost", false);
                Service serv(L"sygsrv", tcp);

                //std::wcout << L"tcpserversocket make: " << std::endl;
                TCPServerSocket socket(serv, false, 10);
                socket.setReuseAddress(true);
                //std::wcout << L"listening..." << std::endl;
                socket.listenSocket();

                //std::wcout << L"accepting" << std::endl;
                ClientSocket *cli = socket.acceptSocket();
                cli->setTimeout(10000);

		//std::wcout << "Past (*cli) >> xd" << std::endl ;

		InvokeRequestOrResponse irr1;
		(*cli) >> irr1;
		//std::wcout << "Past xd >> ir" << std::endl ;
		InvokeRequest ir = irr1.invokeRequest;

		Context ctx;
		InterfaceVersionPair ivp(ir.interfaceName, ir.versionNumber);
		//std::wcout << "Past constructing ivp" << std::endl ;
		ServerInterface &calledsi = orb.find(ivp);
		//std::wcout << "Past orb.find" << std::endl ;
		//std::wcout << "Before calledsi "<< "methodname = " << ir.methodName << std::endl ;
		XdrStream *resultstream = calledsi(ir.methodName, ctx, ir.parameters);
		//std::wcout << "Past calledsi "<< "methodname = " << ir.methodName << std::endl ;

		InvokeResponse iresp;
		iresp.requestId = ir.requestId;
		iresp.errorMessage = L"Hello";
		iresp.results << *resultstream;

		InvokeRequestOrResponse irr2(iresp);
	
		(*cli) << irr2;
		//std::wcout << "Past (*cli) << finalresult" << std::endl ;

		delete resultstream;

	}
	catch (SynerEdgeException &e)
	{
		std::wcout << e.getMsg() << std::endl;
	}
}

