#include "OrbBase.hpp"
#include "boost/format.hpp"
#include <netinet/in.h>
#include <byteswap.h>

namespace SynerEdge
{

int BaseInterface::requestCounter = 0;
boost::mutex BaseInterface::mtx;
boost::once_flag ResponseMap::_sentry = BOOST_ONCE_INIT;
ResponseMap *ResponseMap::instance_ = 0;

XdrStream &operator<<(XdrStream &xdr, const LoginValues &val)
{
	xdr << val.username << val.password;	
	return xdr;
}

XdrStream &operator>>(XdrStream &xdr, LoginValues &val)
{
	xdr >> val.username >> val.password;
	return xdr;
}

XdrStream &operator<<(XdrStream &xdr, const InvokeRequest &val)
{
	xdr << val.requestId << val.interfaceName << val.versionNumber
	    << val.methodName << val.parameters;
	return xdr;
}

XdrStream &operator>>(XdrStream &xdr, InvokeRequest &val)
{
	xdr >> val.requestId >> val.interfaceName >> val.versionNumber
	    >> val.methodName >> val.parameters;

	return xdr;
}

XdrStream &operator<<(XdrStream &xdr, const InvokeResponse &val)
{
	xdr << val.requestId << val.errorMessage << val.results;
	return xdr;
}

XdrStream &operator>>(XdrStream &xdr, InvokeResponse &val)
{
	xdr >> val.requestId;
	xdr >> val.errorMessage;
	xdr >> val.results;

	return xdr;
}

MethodDelegateBase::MethodDelegateBase() {}

void ResponseMap::createInstance()
{
	instance_ = new ResponseMap();
}

ResponseMap *ResponseMap::instance()
{
	boost::call_once(&ResponseMap::createInstance, _sentry);
	return instance_;
}

void ResponseMap::createEntry(InvokeRequest &ir)
{
	ResponseMapWithWait::iterator itor = responseMap.find(ir.requestId);
	if (itor == responseMap.end())
	{
		InvokeResponseWithWait *resp = new InvokeResponseWithWait();
		resp->invokeResponse.requestId = ir.requestId;
		responseMap[ir.requestId] = resp;
	}
	else
	{
		throw OrbException(L"RequestId was already in map");
	}
}

InvokeResponse ResponseMap::waitOnEntry(InvokeRequest &ir)
{
	InvokeResponse result;
	ResponseMapWithWait::iterator itor = responseMap.find(ir.requestId);
	if (itor != responseMap.end())
	{
		InvokeResponseWithWait *resp = (*itor).second;
		boost::mutex::scoped_lock lk(resp->mtx, true);
		while (resp->signaled == 0)
			(resp->cnd).wait(lk);
		result = resp->invokeResponse;
		responseMap.erase(itor);
	}
	else
	{
		throw OrbException(L"RequestId was not found in map");
	}
	return result;
}

void ResponseMap::flagResponse(InvokeResponse &ir)
{
	ResponseMapWithWait::iterator itor = responseMap.find(ir.requestId);
	if (itor != responseMap.end())
	{
		InvokeResponseWithWait *resp = (*itor).second;
		boost::mutex::scoped_lock lk(resp->mtx, true);
		resp->invokeResponse = ir;
		resp->signaled = 1;
		(resp->cnd).notify_one();
	}
	else
	{
		throw OrbException(L"RequestId was not found in map");
	}
}

BaseInterface::BaseInterface()
: interfaceName(), interfaceVersion()
{}

int BaseInterface::incrementRequestCounter()
{
	boost::mutex::scoped_lock lk(mtx);
	return ++requestCounter;
}

BaseInterface::BaseInterface(const BaseInterface &copy) 
: interfaceName(copy.interfaceName), interfaceVersion(copy.interfaceVersion)
{
	for (MethodMap::const_iterator itor = copy.methodMap.begin();
	     itor != copy.methodMap.end();
	     itor++)
	{
		methodMap[(*itor).first] = ((*itor).second)->clone();
	}
}

BaseInterface::BaseInterface(
	const std::wstring &interfaceName_, 
	const std::wstring &interfaceVersion_)
: interfaceName(interfaceName_), interfaceVersion(interfaceVersion_)
{}

BaseInterface &BaseInterface::operator=(const BaseInterface &equal)
{
	// clear current internal method map.
	for (MethodMap::iterator itor = methodMap.begin();
	     itor != methodMap.end();
	     itor++)
	{
		delete (*itor).second;
		methodMap.erase(itor);
	}

	// duplicate method map from serverInterface on RHS.
	for (MethodMap::const_iterator itor = equal.methodMap.begin();
	     itor != equal.methodMap.end();
	     itor++)
	{
		methodMap[(*itor).first] = ((*itor).second)->clone();
	}

	// copy interface name and interface version.
	interfaceName = equal.interfaceName;
	interfaceVersion = equal.interfaceVersion;

	return *this;
}

BaseInterface::~BaseInterface() 
{
	for (MethodMap::iterator itor = methodMap.begin();
	     itor != methodMap.end();
	     itor++)
	{
		delete (*itor).second;
	}
}

void BaseInterface::operator+=(MethodDelegateBase *method)
{
	std::wstring methName = method->getName();
	if (methName.size() != 0)
	{
		MethodMap::iterator itor = methodMap.find(method->getName());
		if (itor == methodMap.end())
		{
			methodMap[method->getName()] = method;
		}
		else
		{
			boost::wformat fmt(L"Server interface %s version %s already contains a mapping for method %s");
			fmt % interfaceName % interfaceVersion % method->getName();
			delete method;
			throw OrbException(fmt.str());
		}
	}
	else
	{
		delete method;
		throw OrbException(L"Method names cannot be empty name in a server interface");
	}
}

void BaseInterface::operator-=(MethodDelegateBase *method)
{
	MethodMap::iterator itor = methodMap.find(method->getName());
	if (itor != methodMap.end())
	{
		delete (*itor).second;
		methodMap.erase(itor);
		delete method;	
	}
	else
	{
		boost::wformat fmt(L"Server interface %s version %s does not contain a mapping for method %s");
		fmt % interfaceName % interfaceVersion % method->getName();
		delete method;
		throw OrbException(fmt.str());
	}
}

XdrStream *ServerInterface::operator()(const std::wstring &methodName, Context &ctx, XdrStream &params)
{
	XdrStream *result = 0;
	MethodMap::iterator itor = methodMap.find(methodName);
	if (itor != methodMap.end())
	{
		MethodDelegateBase *base = (*itor).second;
		result = (*base)(ctx, params);
	}
	else
	{
		boost::wformat fmt(L"Server interface %s version %s does not contain a mapping for method %s");
		fmt % interfaceName % interfaceVersion % methodName;
		throw OrbException(fmt.str());
	}

	return result;
}

XdrStream *ClientInterface::operator()(const std::wstring &methodName, Context &ctx, XdrStream &params)
{
	InvokeRequest ir;
	ir.requestId = incrementRequestCounter();
	ir.interfaceName = interfaceName;
	ir.versionNumber = interfaceVersion;
	ir.methodName = methodName;
	ir.parameters = params;
	
	InvokeRequestOrResponse irr(ir);

	ResponseMap::instance()->createEntry(ir);
	(*ctx.clientSocket) << irr;
	InvokeResponse iresp = ResponseMap::instance()->waitOnEntry(ir);
	
	if (iresp.errorMessage.size() != 0)
	{
		throw SynerEdgeException(iresp.errorMessage);
	}

	XdrStream *result = new XdrStream(iresp.results);
	return result;
}

InterfaceVersionPair BaseInterface::getInterfaceVersion() const
{
	InterfaceVersionPair result(interfaceName, interfaceVersion);
	return result;
}

bool BaseInterface::authorize(const Context &ctx)
{
	return true;
}

OrbBase::OrbBase() : stopRequested(false) {}

OrbBase::~OrbBase() {}

bool OrbBase::authenticate(const LoginValues &loginValues)
{
	return true;
}

void OrbBase::operator+=(ServerInterface &iface)
{
	const InterfaceVersionPair key = iface.getInterfaceVersion();

	ServerInterfaceMap::iterator itor = serverInterfaceMap.find(key);
	if (itor == serverInterfaceMap.end())
	{
		serverInterfaceMap[key] = iface;
	}
	else
	{
		boost::wformat fmt(L"A server interface with name %s and version %s has already been added to this Orb.");
		fmt % key.first % key.second;
		throw OrbException(fmt.str());
	}
}

void OrbBase::operator-=(ServerInterface &iface)
{
	InterfaceVersionPair key = iface.getInterfaceVersion();

	ServerInterfaceMap::iterator itor = serverInterfaceMap.find(key);
	if (itor != serverInterfaceMap.end())
	{
		serverInterfaceMap.erase(itor);
	}
	else
	{
		boost::wformat fmt(L"Could not find interface %s version %s in this orb.");
		fmt % key.first % key.second;
		throw OrbException(fmt.str());
	}
}

ServerInterface &OrbBase::find(const InterfaceVersionPair &key)
{
	ServerInterfaceMap::iterator itor = serverInterfaceMap.find(key);
	if (itor != serverInterfaceMap.end())
	{
		return (*itor).second;
	}
	else
	{
		boost::wformat fmt(L"Could not find interface %s version %s in this orb.");
		fmt % key.first % key.second;
		throw OrbException(fmt.str());
	}
}

InvokeRequest::InvokeRequest() {}
InvokeRequest::InvokeRequest(const InvokeRequest &copy)
: requestId (copy.requestId), interfaceName(copy.interfaceName),
  versionNumber(copy.versionNumber), methodName(copy.methodName),
  parameters(copy.parameters)
{
}

InvokeRequest &InvokeRequest::operator=(const InvokeRequest &equal)
{
	if (this == &equal) return *this;

	requestId = equal.requestId;
	interfaceName = equal.interfaceName;
	versionNumber = equal.versionNumber;
	methodName = equal.methodName;
	parameters = equal.parameters;
}

InvokeResponse::InvokeResponse() {}
InvokeResponse::InvokeResponse(const InvokeResponse &copy)
: requestId(copy.requestId), errorMessage(copy.errorMessage),
  results(copy.results)
{
}

InvokeResponse &InvokeResponse::operator=(const InvokeResponse &equal)
{
	if (this == &equal) return *this;

	requestId = equal.requestId;
	errorMessage =	equal.errorMessage;
	results = equal.results;

	return *this;
}

InvokeRequestOrResponse::InvokeRequestOrResponse()
: isRequest(false)
{}

InvokeRequestOrResponse::InvokeRequestOrResponse(const InvokeRequest &ir)
: isRequest(true), invokeRequest(ir)
{}

InvokeRequestOrResponse::InvokeRequestOrResponse(const InvokeResponse &ir)
: isRequest(false), invokeResponse(ir)
{}

XdrStream &operator<<(XdrStream &xdr, const InvokeRequestOrResponse &irr)
{
	xdr << irr.isRequest;
	if (irr.isRequest)
	{
		xdr << irr.invokeRequest;
	}
	else
	{
		xdr << irr.invokeResponse;
	}
	return xdr;
}

XdrStream &operator>>(XdrStream &xdr, InvokeRequestOrResponse &irr)
{
	bool isRequest;
	xdr >> isRequest;
	irr.isRequest = isRequest;

	if (isRequest)
	{
		xdr >> irr.invokeRequest;
	}
	else
	{
		xdr >> irr.invokeResponse;
	}
	return xdr;
}

ClientSocket &operator<<(ClientSocket &soc, const InvokeRequestOrResponse &irr)
{
	XdrStream xdr;
	uint32_t irrSize;

	if (irr.isRequest)
	{
		irrSize = (6 * sizeof(uint32_t)) + XdrStream::roundToBoundary(irr.invokeRequest.interfaceName.size()) + XdrStream::roundToBoundary(irr.invokeRequest.versionNumber.size()) + XdrStream::roundToBoundary(irr.invokeRequest.methodName.size()) + irr.invokeRequest.parameters.getBufferSize();
		xdr << irrSize << irr.isRequest << irr.invokeRequest;
	}
	else
	{
		irrSize = (4 * sizeof(uint32_t)) + XdrStream::roundToBoundary(irr.invokeResponse.errorMessage.size()) + irr.invokeResponse.results.getBufferSize();
		xdr << irrSize << irr.isRequest << irr.invokeResponse;
	}

	boost::mutex::scoped_lock lk(soc.getMutex());
	if (! soc.sendSocket(xdr.getAndClearBuffer()))
	{
		std::wcout << L"send failed" << std::endl;
	}
	
	return soc;
}

ClientSocket &operator>>(ClientSocket &soc, InvokeRequestOrResponse &irr)
{

	std::string xdrsizestr = soc.recvSocket(sizeof(int32_t));
	int32_t xdrsize = ntohl
		(*(reinterpret_cast<const int32_t *>(xdrsizestr.c_str())));

	std::string xdrbuffer =
		soc.recvSocket(static_cast<size_t>(xdrsize));

	XdrStream xdr(xdrbuffer);

	xdr >> irr.isRequest;
	if (irr.isRequest)
	{
		xdr >> irr.invokeRequest;
	}
	else
	{
		xdr >> irr.invokeResponse;
	}

	return soc;
}

void OrbBase::processContext(Context &ctx)
{
	while ((! stopRequested) && (ctx.clientSocket != NULL) && (! ctx.clientSocket->isClosed()))
	{
		try
		{
		InvokeRequestOrResponse irr;
		(*ctx.clientSocket) >> irr;

		if (irr.isRequest)
		{
			InterfaceVersionPair ivp(irr.invokeRequest.interfaceName, irr.invokeRequest.versionNumber);
			try
			{
				ServerInterface &sif = find(ivp);

				XdrStream *res = sif(irr.invokeRequest.methodName, ctx, irr.invokeRequest.parameters);
				
				irr.invokeResponse.results = *res;
				delete res;
			}
			catch (SynerEdgeException &ex)
			{
				irr.invokeResponse.errorMessage = ex.getMsg();
			}
			catch (...)
			{
				irr.invokeResponse.errorMessage = L"Unknown exception thrown in ";
				irr.invokeResponse.errorMessage += irr.invokeRequest.methodName;
			}
			
			irr.isRequest = false;
			irr.invokeResponse.requestId = irr.invokeRequest.requestId;

			(*ctx.clientSocket) << irr;
		}
		else
		{
			ResponseMap::instance()->flagResponse(irr.invokeResponse);
		}

		}
		catch (SynerEdgeException &e)
		{
			std::wcout << L"Exception in processContext: " << e.getMsg() << std::endl;
		}
	}
}

ServerOrb::ExecuteThread::ExecuteThread(ServerOrb *instance_)
: instance(instance_)
{}

void ServerOrb::ExecuteThread::operator()()
{
	while (! instance->stopRequested)
	{
		Context ctx = instance->que.waitAndPop();

		try
		{
			instance->processContext(ctx);
		}
		catch (SynerEdgeException &ex)
		{
			std::wcout << L"exception in executethread: " << ex.getMsg() << std::endl;
		}

		delete ctx.clientSocket;
		delete ctx.additionalData;
	}
}

ServerOrb::ListenThread::ListenThread(ServerOrb *instance_)
: instance(instance_)
{}

void ServerOrb::ListenThread::operator()()
{
	instance->tcpserv.listenSocket();
	while (! instance->stopRequested)
	{
		ClientSocket *clisock = instance->tcpserv.acceptSocket();
		clisock->setTimeout(instance->timeout);

		Context ctx(clisock, L"", L"");
		instance->que.pushAndNotify(ctx);
	}
}

ServerOrb::ServerOrb(const Service &serv, bool isIP6, int queuelen)
: tcpserv(serv, isIP6, queuelen), listenThread(0),
  started(false), threadcount(0), timeout(-1)
{
	tcpserv.setReuseAddress(true);
}

void ServerOrb::start(unsigned threadcount_)
{
	threadcount = threadcount_;
	createListenThread();
} 

void ServerOrb::join()
{
	listenThread->join();
	thrgroup.join_all();
}

void ServerOrb::createListenThread()
{
	listenThread = new boost::thread(ListenThread(this));
	for (unsigned i = 0; i < threadcount; i++)
	{
		thrgroup.add_thread(new boost::thread(ExecuteThread(this)));
	};
}

void ServerOrb::setTimeout(int milliseconds)
{
	timeout = milliseconds;
}

ClientOrb::ClientOrb(SocketAddress &addr_, const std::wstring &uname, const std::wstring &password)
: listenThread(0), context(0, uname, password), addr(addr_)
{}

void ClientOrb::start()
{
	context.clientSocket = new ClientSocket(addr);
	context.clientSocket->setTimeout(timeout);
	context.clientSocket->connectSocket();
	createListenThread();
}

void ClientOrb::setTimeout(int timeout_)
{
	timeout = timeout_;
}

void ClientOrb::join()
{
	listenThread->join();
}
void ClientOrb::createListenThread()
{
	listenThread = new boost::thread(ListenThread(this));
}

ClientOrb::ListenThread::ListenThread(ClientOrb *instance_)
: instance(instance_)
{}

void ClientOrb::ListenThread::operator()()
{
	instance->processContext(instance->context);
}

}

