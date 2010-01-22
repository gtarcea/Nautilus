#include "SocketBase.hpp"
#include "StringConversion.hpp"

#include <sstream>

#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/poll.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <stdio.h>
#include <signal.h>
#include <unistd.h>
#include <string.h>
#include <strings.h>
#include <errno.h>

//#define DO_PT

#include "PT.hpp"

using namespace std ;

namespace SynerEdge
{

struct SocketBase::InnerData
{
	InnerData(const Protocol &proto, bool isIP6_)
	: protocol(proto), handle(-1), isIP6(isIP6_), stopRequested(false),
	  isAllocated(false)
	{}

	InnerData(socket_handle handle_, const Protocol &proto, bool isIP6_)
	: protocol(proto), handle(handle_), isIP6(isIP6_), stopRequested(false),
	  isAllocated(true)
	{}

	~InnerData() {}

	InnerData(const InnerData &copy)
	: protocol(copy.protocol), handle(copy.handle), isIP6(copy.isIP6),
	  stopRequested(copy.stopRequested), isAllocated(copy.isAllocated)
	{}

	InnerData &operator=(const InnerData &equal)
	{
		if (this == &equal) return *this;
		protocol = equal.protocol;
		handle = equal.handle;
		isIP6 = equal.isIP6;
		stopRequested = equal.stopRequested;
		isAllocated = equal.isAllocated;
		return *this;
	}

	Protocol protocol;
	socket_handle handle;
	bool isIP6;	
	bool stopRequested;
	bool isAllocated;
};

SocketBase::SocketBase(const Protocol &proto, bool isIP6)
: innerData(*(new InnerData(proto, isIP6))), socketClosing(this),
  bufferSize(0)
{}

SocketBase::SocketBase(socket_handle handle, const Protocol &proto, bool isIP6)
: innerData(*(new InnerData(handle, proto, isIP6))), socketClosing(this),
  bufferSize(0)
{}

SocketBase::~SocketBase()
{
	closeSocket();
	delete &innerData;
}

void SocketBase::allocateSocket()
{
	if (innerData.isIP6)
	{
		innerData.handle = socket(PF_INET6, 
			innerData.protocol.getType(), 
			innerData.protocol.getNumber());
		if (innerData.handle == -1)
		{
			throw NetException(StringConversion::syserr());
		}
		innerData.isAllocated = true;
	}
	else
	{
		innerData.handle = socket(PF_INET, 
			innerData.protocol.getType(), 
			innerData.protocol.getNumber());
		if (innerData.handle == -1)
		{
			throw NetException(StringConversion::syserr());
		}
		innerData.isAllocated = true;
	}

	applyBufferSize();
}

void SocketBase::onSocketClosing(const BaseEventArgs &e)
{
	socketClosing(e);
}

void SocketBase::closeSocket()
{
	if (! isClosed())
	{
		BaseEventArgs e;
		onSocketClosing(e);

		::close(innerData.handle);
		innerData.isAllocated = false;
	}
}

bool SocketBase::isClosed() const
{
	return ! innerData.isAllocated;
}

bool SocketBase::isStopRequested() const
{
	return innerData.stopRequested;
}

void SocketBase::requestStop()
{
	innerData.stopRequested = true;
}

void SocketBase::reallocate()
{
	closeSocket();
	allocateSocket();
}

const Protocol &SocketBase::getProtocol() const
{
	return innerData.protocol;
}

bool SocketBase::isIP6() const
{
	return innerData.isIP6;
}

void SocketBase::applyBufferSize()
{
	if (bufferSize > 0)
	{
		int val = static_cast<int>(bufferSize);

		if (setsockopt(innerData.handle, SOL_SOCKET, SO_SNDBUF,
	               	reinterpret_cast<char *>(&val), sizeof(val)) < 0)
		{
			throw SocketException(StringConversion::socketerr());	
		}

		if (setsockopt(innerData.handle, SOL_SOCKET, SO_RCVBUF,
	               	reinterpret_cast<char *>(&val), sizeof(val)) < 0)
		{
			throw SocketException(StringConversion::socketerr());	
		}
	}
}

void SocketBase::setBufferSize(unsigned int bufferSize_)
{
	bufferSize = bufferSize_;

	if (innerData.handle != -1)
	{
		applyBufferSize();
	}
}

ServerSocket::ServerSocket(const Service &serv, bool isBroadcast, bool isIP6)
: SocketBase(serv.getProtocol(), isIP6), service(serv), noDelay(false),
  address(serv, isBroadcast, isIP6)
{}

ServerSocket::~ServerSocket()
{}

const Service &ServerSocket::getService() const
{
	return service;
}

const SocketAddress &ServerSocket::getSocketAddress() const
{
	return address;
}

void ServerSocket::applyReuse()
{
	int val = (reuseAddress) ? 1 : 0;

	if (setsockopt(innerData.handle, SOL_SOCKET, SO_REUSEADDR,
	               reinterpret_cast<char *>(&val), sizeof(val)) < 0)
	{
		throw SocketException(StringConversion::socketerr());
	}
}

void ServerSocket::setNoDelay(bool nodelay)
{
	noDelay = nodelay ;
	if (noDelay) {
		if (innerData.handle != -1) {
			applyNoDelay() ;
		}
	}
}

void ServerSocket::applyNoDelay()
{
	int val = 1 ;

	int rc = setsockopt(innerData.handle, IPPROTO_TCP, 1,
		reinterpret_cast<char *>(&val), sizeof(val)) ;

	if (rc < 0) {
		cout << "setNoDelay() failed" << endl ;
		throw SocketException(StringConversion::socketerr());
	}
}
		
void ServerSocket::setReuseAddress(bool reuseIt)
{
	reuseAddress = reuseIt;
	if (innerData.handle != -1)
	{
		applyReuse();
	}
}

void ServerSocket::bindSocket()
{
	allocateSocket();

	applyReuse();

	if (noDelay) {
		applyNoDelay() ;
	}

	const struct sockaddr *sa = address.getSockAddr();
	socklen_t sl = address.getSockAddrSize();

	if (! innerData.isIP6)
	{
		IPAddress ip(((sockaddr_in *) sa)->sin_addr);
	}
	else
	{
		IPAddress ip6(((sockaddr_in6 *) sa)->sin6_addr);
	}


	if (bind(innerData.handle, sa, sl) < 0)
	{
		throw SocketException(StringConversion::socketerr());
	}
}

void ServerSocket::reallocate()
{
	SocketBase::reallocate();
	bindSocket();
}

TCPServerSocket::TCPServerSocket(const Service &serv, bool isIP6, int queuelen_)
: ServerSocket(serv, false, isIP6), queuelen(queuelen_)
{}

TCPServerSocket::~TCPServerSocket()
{}

void TCPServerSocket::reallocate()
{
	ServerSocket::reallocate();
	listenSocket();
}

void TCPServerSocket::listenSocket()
{
	bindSocket();
	if (listen(innerData.handle, queuelen) < 0)
	{
		throw SocketException(StringConversion::socketerr());
	}
}

ClientSocket *TCPServerSocket::acceptSocket()
{
	socket_handle ssock = -1;
	int maxaddrsize = SocketAddress::getSockAddrV4Size();
	if (SocketAddress::getSockAddrV6Size() > maxaddrsize)
		maxaddrsize = SocketAddress::getSockAddrV6Size();

	char buf[maxaddrsize];
	socklen_t alen = sizeof(buf);

	ClientSocket *result = 0;
	bool cont = true;
	while (cont)
	{
		ssock = accept(innerData.handle, 
		               reinterpret_cast<sockaddr *>(buf),
	                       &alen);

		if (ssock < 0)
		{
			int myerrno = errno ;
			if (isStopRequested()) break;
			if (myerrno == EINTR) continue;
			throw SocketException(StringConversion::socketerr());
		}
		else
		{
			SocketAddress addr(Protocol(L"tcp"), 
		                   reinterpret_cast<sockaddr *>(buf), alen);

			result = new ClientSocket(ssock, addr);
			cont = false;
		}
	}

	return result;
}

ClientSocket::ClientSocket(const SocketAddress &socketAddress)
: SocketBase(socketAddress.getProtocol(), socketAddress.isIP6()),
  _sockaddr(socketAddress), readPosition(0), writePosition(0), timeout(-1)
{}

ClientSocket::ClientSocket(socket_handle handle, const SocketAddress &socketAddress)
: SocketBase(handle, socketAddress.getProtocol(), socketAddress.isIP6()),
  _sockaddr(socketAddress), readPosition(0), writePosition(0), timeout(-1)
{}

ClientSocket::~ClientSocket()
{
}

void ClientSocket::reallocate()
{
	SocketBase::reallocate();
	connectSocket();	
}

const SocketAddress &ClientSocket::getSocketAddress() const
{
	return _sockaddr;
}

void ClientSocket::setTimeout(int millis)
{
	timeout = millis;
}

int ClientSocket::getTimeout() const
{
	return timeout;
}

void ClientSocket::connectSocket()
{
	if (innerData.handle < 0)
	{
		allocateSocket();

		bool cont = true;
		while (cont)
		{
			const struct sockaddr *sa = _sockaddr.getSockAddr();
			socklen_t st = _sockaddr.getSockAddrSize();

			if (connect(innerData.handle, sa, st) != 0)
			{
				innerData.handle = -1 ;
				if (isStopRequested()) break;
				if (errno == EINTR) continue;
				throw SocketException(L"Unable to connect to socket");
			}
			else
			{
				cont = false;
			}
		}
	}
	else
	{
		throw SocketException(L"Error - socket is already connected!");
	}
}

bool ClientSocket::sendSocket(std::string msg)
{
	if (isClosed())
	{
		throw SocketException(L"Socket is closed");
	}

	bool result = true;
	ssize_t numsent = 0;
	const char *bufstart = msg.c_str();
	size_t msglen = msg.size();

	while (msglen > 0)
	{
		if (! pollSocket(PollWrite))
		{
			result = false;
			throw SocketTimeout();
			break;
		}

		if ((numsent = send(innerData.handle, bufstart, msglen, 0)) < 0)
		{
			if (errno == EINTR) continue;
			if (isStopRequested()) 
			{
				std::wcout << "stop was requested" << std::endl;
				result = false;
				break;
			}
			throw SocketException(StringConversion::socketerr());
		}
		else
		{
			bufstart += numsent;
			msglen -= numsent;
		}
	}

	return result;
}

bool ClientSocket::recvSocketInternal(size_t numchars)
{
	bool result = true;
	if (isClosed())
	{
		throw SocketException(L"Socket is closed");
	}

	ssize_t numrecv = 0;
	ssize_t msglen = numchars;
	char buf[8192];

	while (msglen > 0)
	{
		if (! pollSocket(PollRead))
		{
			result = false;
			throw SocketTimeout();
			break;
		}
		numrecv = recv(innerData.handle, buf, sizeof(buf), 0);
		if (numrecv < 0)
		{
			if (errno == EINTR) continue;
			if (isStopRequested()) 
			{
				result = false;
				break;
			}
			throw SocketException(StringConversion::socketerr());
		}
		else if (numrecv == 0)
		{
			result = false;
			closeSocket();	
			break;
		}
		else 
		{
			msglen -= numrecv;
			internalBuffer.seekp(writePosition, std::ios::beg);
			internalBuffer.write(buf, numrecv);
			writePosition += numrecv;
		}
	}

	return result;
	
}

std::string ClientSocket::recvSocket(size_t innumchars)
{
	size_t charsRemaining = innumchars;
	std::stringstream result;

	while (charsRemaining > 0)
	{
	size_t numchars = charsRemaining;
	if (numchars > 32768)
	{
		numchars = 32768;
	}

	internalBuffer.seekg(readPosition, std::ios::beg);
	char buf[numchars];
	std::streamsize len = internalBuffer.readsome(buf, numchars);
	readPosition += len;

	bool gotAllCharsExpected = true;
	if (len < numchars)
	{
		internalBuffer.clear();
		internalBuffer.str("");
		readPosition = 0;
		writePosition = 0;
		internalBuffer.seekg(0, std::ios::beg);
		internalBuffer.seekp(0, std::ios::beg);

		gotAllCharsExpected = recvSocketInternal(numchars - len);
		if (! gotAllCharsExpected)
		{
			throw SocketTimeout();
		}

		internalBuffer.seekg(readPosition, std::ios::beg);
		std::streamsize pluslen = 
			internalBuffer.readsome(&buf[len], numchars-len);
		readPosition += pluslen;
		len += pluslen;
	}

	charsRemaining -= len;
	result << std::string(buf, len);

	if (! gotAllCharsExpected) break;
	}

	return result.str();
}

bool ClientSocket::pollSocket(enum PollEnum pollType)
{
	bool result = true; //hope for the best...
	struct pollfd ufds[1];
	unsigned int nfds = 1;
	short revents_canread = POLLIN;
	short revents_canwrite = POLLOUT;

	ufds[0].fd = innerData.handle;
	ufds[0].revents = 0;
	switch (pollType)
	{
	case PollRead :
		ufds[0].events = POLLIN;
		break;
	case PollWrite : 
		ufds[0].events = POLLOUT;
		break;
	}

	while (1)
	{
	PT("Calling poll()") ;
	int res = poll(ufds, nfds, timeout);
	PT("Past poll()") ;
	if (res < 0)
	{
		if (errno == EINTR) continue;
		else throw SocketException(StringConversion::socketerr());
	} 
	else if (res == 0)
	{
		result = false;
	} 
	else
	{
		if (res != 1)
		{
			throw SocketException(L"Unexpected result from poll");
		}
		else
		{
			if ((pollType == PollRead) && 
			    ((ufds[0].revents & revents_canread) == 0))
			{
				std::wstringstream errs;
				errs << L"Problem with socket poll: ";
				if (ufds[0].revents | POLLERR)
				{
					errs << L" POLLERR";
				}
				if (ufds[0].revents | POLLHUP)
				{
					errs << L" POLLHUP";
				}
				if (ufds[0].revents | POLLNVAL)
				{
					errs << L" POLLNVAL";
				}
				throw SocketException(errs.str());
			}

			if ((pollType == PollWrite) &&
			    ((ufds[0].revents & revents_canwrite) == 0))
			{
				std::wstringstream errs;
				errs << L"Problem with socket poll: ";
				if (ufds[0].revents | POLLERR)
				{
					errs << L" POLLERR";
				}
				if (ufds[0].revents | POLLHUP)
				{
					errs << L" POLLHUP";
				}
				if (ufds[0].revents | POLLNVAL)
				{
					errs << L" POLLNVAL";
				}
				throw SocketException(errs.str());
			}
			
		}
	}
	break;
	}

	return result;
}

};
