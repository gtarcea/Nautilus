#ifndef SynerEdge_OrbBase_h__
#define SynerEdge_OrbBase_h__

#include "XdrStream.hpp"
#include "SynerEdge.hpp"
#include "SemaQueue.hpp"
#include <map>
#include <string>
#include <list>
#include "boost/utility.hpp"
#include "boost/thread.hpp"
#include "boost/thread/mutex.hpp"
#include "boost/thread/condition.hpp"

namespace SynerEdge
{

class OrbException : public SynerEdgeException
{
public:
	OrbException(const std::wstring &msg) : SynerEdgeException(msg) {}
};

struct LoginValues
{
	std::wstring username;
	std::wstring password;

};

XdrStream &operator<<(XdrStream &xdr, const LoginValues &loginValues);
XdrStream &operator>>(XdrStream &xdr, const LoginValues &loginValues);

class AdditionalContextData
{
public:
	AdditionalContextData() {}
	virtual ~AdditionalContextData() {}
};

class Context
{
public:
	Context() 
	: clientSocket(0), additionalData(0), username(), password() 
	{}

	Context(ClientSocket *soc, 
		const std::wstring &uname, 
		const std::wstring &passwd) 
	: clientSocket(soc), username(uname), password(passwd), 
	  additionalData(0) 
	{}
	virtual ~Context() {}

	std::wstring username;
	std::wstring password;
	ClientSocket *clientSocket;
	AdditionalContextData *additionalData;
};

struct InvokeRequest
{
	InvokeRequest();
	InvokeRequest(const InvokeRequest &copy);
	InvokeRequest &operator=(const InvokeRequest &equal);

	uint32_t requestId;
	std::wstring interfaceName;
	std::wstring versionNumber;
	std::wstring methodName;
	XdrStream parameters;
};

XdrStream &operator<<(XdrStream &xdr, const InvokeRequest &invokeReq);
XdrStream &operator>>(XdrStream &xdr, InvokeRequest &invokeReq);

struct InvokeResponse
{
	InvokeResponse();
	InvokeResponse(const InvokeResponse &copy);
	InvokeResponse &operator=(const InvokeResponse &equal);

	uint32_t requestId;
	std::wstring errorMessage; 
	XdrStream results;
};

XdrStream &operator<<(XdrStream &xdr, const InvokeResponse &invokeResp);
XdrStream &operator>>(XdrStream &xdr, InvokeResponse &invokeResp);

struct InvokeRequestOrResponse
{
	InvokeRequestOrResponse();
	InvokeRequestOrResponse(const InvokeRequest &ir);
	InvokeRequestOrResponse(const InvokeResponse &ir);
	bool isRequest;
	InvokeRequest invokeRequest;
	InvokeResponse invokeResponse;	
};

XdrStream &operator<<(XdrStream &xdr, const InvokeRequestOrResponse &invokeReqOrResp);
XdrStream &operator>>(XdrStream &xdr, InvokeRequestOrResponse &invokeRequestOrResponse);

ClientSocket &operator<<(ClientSocket &soc, const InvokeRequestOrResponse &invokeReqOrResp);
ClientSocket &operator>>(ClientSocket &soc, InvokeRequestOrResponse &invokeRequestOrResponse);

typedef XdrStream (*methodfn)(Context &ctx, XdrStream &parameters);

struct InvokeResponseWithWait
{
	InvokeResponseWithWait() : signaled(0) {}

	InvokeResponse invokeResponse;
	boost::mutex mtx;
	boost::condition cnd;
	int signaled;
};

class ResponseMap : boost::noncopyable
{
public:
	~ResponseMap() {}
	static boost::once_flag _sentry;
	static void createInstance();

	static ResponseMap *instance();

	void createEntry(InvokeRequest &ir);
	InvokeResponse waitOnEntry(InvokeRequest &ir);
	void flagResponse(InvokeResponse &ir);
private:
	static ResponseMap *instance_;

	typedef std::map<int, InvokeResponseWithWait*> ResponseMapWithWait;

	ResponseMapWithWait responseMap;

	ResponseMap() {}
	ResponseMap(const ResponseMap &copy);
	ResponseMap &operator=(const ResponseMap &equal);
};

class MethodDelegateBase
{
public:
	MethodDelegateBase();
	virtual XdrStream *operator() (Context &ctx, XdrStream &params) = 0;
	virtual MethodDelegateBase *clone() = 0;
	virtual bool operator==(const MethodDelegateBase &equals)
	{
		return isEqual(equals);
	}
	virtual bool isEqual(const MethodDelegateBase &equals) = 0;
	virtual std::wstring getName() const = 0;
};

template<class T> class MethodDelegate : public MethodDelegateBase
{
public:
	typedef XdrStream *(T::*MemberPtr)(Context &ctx, XdrStream &params);

	MethodDelegate() : mname(), mptr(0), obj(0) { }
	MethodDelegate(const MethodDelegate &copy) : mname(copy.mname), mptr(copy.mptr), obj(copy.obj) {}
	MethodDelegate(const std::wstring &mname_, T &obj_, MemberPtr mptr_) : mname(mname_), mptr(mptr_), obj(&obj_) { }

	virtual ~MethodDelegate() {}
	virtual XdrStream *operator()(Context &ctx, XdrStream &params)
	{
		return (obj->*mptr)(ctx, params);
	}	
	virtual MethodDelegateBase *clone()
	{
		return new MethodDelegate<T>(*this);
	}
	virtual bool isEqual(const MethodDelegateBase &equals)
	{
		bool result = false;
		const MethodDelegate *test = 
			dynamic_cast<const MethodDelegate *>(&equals);
		if (test != 0)
		{
			result = ((obj == test->obj) && (mptr == test->mptr));
		}
		return result;
	}
	virtual std::wstring getName() const
	{
		return mname;
	}
private:
	T *obj;
	std::wstring mname;
	MemberPtr mptr;
};

typedef std::pair< std::wstring, std::wstring> InterfaceVersionPair;

class BaseInterface
{
public:
	BaseInterface();
	BaseInterface(const BaseInterface &copy);
	BaseInterface(const std::wstring &interfaceName, const std::wstring &interfaceVersion);
	BaseInterface &operator=(const BaseInterface &equal);
	virtual ~BaseInterface();

	void operator+=(MethodDelegateBase *method);
	void operator-=(MethodDelegateBase *method);

	virtual XdrStream *operator()(const std::wstring &methodName, Context &ctx, XdrStream &params) = 0;

	virtual bool authorize(const Context &ctx);
	InterfaceVersionPair getInterfaceVersion() const;

protected:
	static int requestCounter;
	static boost::mutex mtx;
	static int incrementRequestCounter();

	typedef std::map< std::wstring, MethodDelegateBase*> MethodMap;

	std::wstring interfaceName;
	std::wstring interfaceVersion;

	MethodMap methodMap;
};

class ServerInterface : public BaseInterface
{
public:
	ServerInterface(const std::wstring &interfaceName, const std::wstring &versionNumber) : BaseInterface(interfaceName, versionNumber)
	{}
	ServerInterface() : BaseInterface() {}
	ServerInterface(const ServerInterface &copy) : BaseInterface(copy) {}

	virtual XdrStream *operator()(const std::wstring &methodName, Context &ctx, XdrStream &params);
};

class ClientInterface : public BaseInterface
{
public:
	ClientInterface(const std::wstring &interfaceName, const std::wstring &versionNumber) : BaseInterface(interfaceName, versionNumber) {}
	ClientInterface() : BaseInterface() {}
	ClientInterface(const ServerInterface &copy) : BaseInterface(copy) {}

	virtual XdrStream *operator()(const std::wstring &methodName, Context &ctx, XdrStream &params);
};

class OrbBase
{
public:
	OrbBase();
	virtual ~OrbBase();

	void operator+=(ServerInterface &iface);
	void operator-=(ServerInterface &iface);
	
	ServerInterface &find(const InterfaceVersionPair &interfaceVersion);

	virtual bool authenticate(const LoginValues &loginValues);

	void processContext(Context &ctx);
protected:
	bool stopRequested;

private:
	typedef std::map<InterfaceVersionPair, ServerInterface> 
		ServerInterfaceMap;

	ServerInterfaceMap serverInterfaceMap;

};

class ClientOrb : public OrbBase
{
public:
	ClientOrb(SocketAddress &addr, const std::wstring &uname, const std::wstring &password);
	void start();
	void join();

	void setTimeout(int timeout);
	Context &getContext() { return context; }

protected:
	class ListenThread
	{
	public:
		ListenThread(ClientOrb *instance_);
		void operator()();
	private:
		ClientOrb *instance;
	};

private:
	boost::thread *listenThread;
	SocketAddress addr;
	Context context;
	int timeout;

	void createListenThread();
};

class ServerOrb : public OrbBase
{
public:
	ServerOrb(const Service &serv, bool isIP6, int queuelen);

	void setTimeout(int milliseconds);

	void start(unsigned threadcount);
	void join();

protected:
	class ExecuteThread
	{
	public:
		ExecuteThread(ServerOrb *instance);
		void operator()();

	private:
		ServerOrb *instance;
	};

	class ListenThread
	{
	public:
		ListenThread(ServerOrb *instance);
		void operator()();
	private:
		ServerOrb *instance;
	};

private:
	boost::thread *listenThread;

	SemaQueue<Context> que;
	TCPServerSocket tcpserv;
	boost::thread_group thrgroup;
	bool started;
	int threadcount;
	int timeout;

	void createListenThread();

};

}

#endif

