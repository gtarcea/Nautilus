#ifndef SynerEdge_Socket_h__
#define SynerEdge_Socket_h__

#include "SynerEdge.hpp"
#include "SocketAddress.hpp"
#include "Net.hpp"
#include "Observer.hpp"
#include "boost/utility.hpp"
//#include "SSLSocket.hpp"

#include <sstream>

namespace SynerEdge
{

typedef int socket_handle;

class SocketException : public SynerEdgeException
{
public:
	SocketException(const std::wstring &msg)
	: SynerEdgeException(msg)
	{}
};

class SocketTimeout : public SocketException
{
public:
	SocketTimeout() : SocketException(L"Timeout") {}
};

class SocketBase : public Observable, private boost::noncopyable
{
public:
	SocketBase(const Protocol &proto, bool isIP6);
	SocketBase(socket_handle handle, const Protocol &proto, bool isIP6);
	virtual ~SocketBase();

	ObservableEvent<BaseEventArgs> socketClosing;

	virtual void closeSocket();
	virtual bool isClosed() const;
	virtual void requestStop(); 
	virtual bool isStopRequested() const;
	void setBufferSize(unsigned int bufferSize);
	void allocateSocket();

	const Protocol &getProtocol() const;
	bool isIP6() const;

	boost::mutex &getMutex() { return mtx; }

protected:
	struct InnerData;

	virtual void reallocate();
	virtual void onSocketClosing(const BaseEventArgs &e);

	InnerData &innerData;
private:
	unsigned int bufferSize;
	boost::mutex mtx;

	void applyBufferSize();

	// non-copyable semantics
	SocketBase(const SocketBase &copy);
	SocketBase &operator=(const SocketBase &equal);
};

class ServerSocket : public SocketBase
{
public:
	ServerSocket(const Service &serv, bool isBroadcast, bool isIP6);
	virtual ~ServerSocket();

	const Service &getService() const;
	const SocketAddress &getSocketAddress() const;

	void setReuseAddress(bool reuseIt);
	void setNoDelay(bool nodelay) ;
	void bindSocket();

protected:
	virtual void reallocate();

private:
	void applyReuse();
	void applyNoDelay() ;

	Service service;
	SocketAddress address;
	bool reuseAddress;
	bool noDelay ;

};

class ClientSocket : public SocketBase
{
public:
	ClientSocket(const SocketAddress &addr);
	ClientSocket(int handle, const SocketAddress &addr);
	virtual ~ClientSocket();

	const SocketAddress &getSocketAddress() const;
	void setTimeout(int millis);
	int getTimeout() const;
	virtual void connectSocket();

	virtual std::string recvSocket(size_t numchars);
	virtual bool sendSocket(std::string buffer);

protected:
	virtual void reallocate();

private:
	enum PollEnum { PollRead, PollWrite };

	SocketAddress _sockaddr;
	std::stringstream internalBuffer;
	std::streamsize readPosition;
	std::streamsize writePosition;
	int timeout;

	bool pollSocket(enum PollEnum pollType);
	bool recvSocketInternal(size_t numchars);
};

/*class SSLClientSocket : public ClientSocket
{
public:
	SSLClientSocket(const SocketAddress &addr, const SSLContext &context);
	SSLClientSocket(int handle, const SocketAddress &addr, 
	                const SSLContext &context);

	virtual void connectSocket();

	virtual std::string recvSocket(size_t numchars);
	virtual bool sendSocket(std::string buffer);
	
protected:
	virtual void reallocate();

private:
	SSLContext &ctx;
};*/

class TCPServerSocket : public ServerSocket
{
public:
	TCPServerSocket(const Service &serv, bool isIP6, int queuelen);
	virtual ~TCPServerSocket();

	virtual ClientSocket *acceptSocket();
	void listenSocket();

protected:
	virtual void reallocate();

private:
	int queuelen;
};

/*

class SSLTCPServerSocket : public TCPServerSocket
{
public:
	SSLTCPServerSocket(const Service &serv, bool isIP6, 
	                   int queuelen, SSLContext &context);
	virtual ~SSLTCPServerSocket();

	virtual ClientSocket *acceptSocket();

protected:
	virtual void reallocate();

private:
	SSLContext &ctx;
};*/

}

#endif

