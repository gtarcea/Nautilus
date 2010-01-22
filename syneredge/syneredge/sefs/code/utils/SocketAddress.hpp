#ifndef SocketAddress_h__
#define SocketAddress_h__

#include "Net.hpp"

struct sockaddr;

namespace SynerEdge
{

class SocketAddressException : public SynerEdgeException
{
public:
	SocketAddressException(const std::wstring &msg)
	: SynerEdgeException(msg)
	{}
};

class SocketAddress
{
public:
	SocketAddress(const Host &hst, const Service &service);
	SocketAddress(const Service &serv, bool isBroadcast, bool isIP6);
	SocketAddress(const Protocol &proto, const sockaddr *addr, int addrLen);
	SocketAddress(const SocketAddress &copy);

	SocketAddress &operator=(const SocketAddress &equal);
	~SocketAddress();

	bool operator==(const SocketAddress &equal) const;

	bool isIP6() const;
	bool isBroadcast() const;
	const Protocol &getProtocol() const;
	short getPort() const;

	//const in_addr &getInAddr() const;
	static size_t getSockAddrV4Size();
	//const in6_addr &getIn6Addr() const;
	static size_t getSockAddrV6Size();
	const sockaddr *getSockAddr() const;
	size_t getSockAddrSize() const;

private:
	struct InnerData;
	InnerData &innerData;
};

}

#endif

