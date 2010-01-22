#include "SocketAddress.hpp"
#include <iostream>

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>

namespace SynerEdge
{

struct SocketAddress::InnerData
{

	InnerData(const Host &hst, const Service &serv)
	: isIP6(hst.isIP6()), isBroadcast(false), protocol(serv.getProtocol())
	{
		if (hst.isIP6())
		{
			memset(&addr6, 0, sizeof(addr6));
			addr6.sin6_addr = hst.getIP().getIn6Addr();
			addr6.sin6_family = AF_INET6;
			addr6.sin6_port = htons(serv.getPort());
		}	
		else
		{
			memset(&addr, 0, sizeof(addr));
			addr.sin_addr = hst.getIP().getInAddr();
			addr.sin_family = AF_INET;
			addr.sin_port = htons(serv.getPort());
		}
	}

	InnerData(const Service &serv, bool isBroadcast_, bool isIP6_)
	: isBroadcast(isBroadcast_), isIP6(isIP6_), protocol(serv.getProtocol())
	{
		if (isIP6)
		{
			memset(&addr6, 0, sizeof(addr6));
			if (isBroadcast)
			{
				throw NetException(L"IPV6 does not support broadcast addressing");
			}
			else
			{
				addr6.sin6_addr = in6addr_any;
			}

			addr6.sin6_family = AF_INET6;
			addr6.sin6_port = htons(serv.getPort());
		}
		else
		{
			memset(&addr, 0, sizeof(addr));
			if (isBroadcast)
			{
				addr.sin_addr.s_addr = 
					htonl(INADDR_BROADCAST);
			}
			else
			{
				addr.sin_addr.s_addr = 
					htonl(INADDR_ANY);
			}

			addr.sin_family = AF_INET;
			addr.sin_port = htons(serv.getPort());
		}
	}

	InnerData(const Protocol &proto, const sockaddr *addr_, int addrLen)
	: protocol(proto), isBroadcast(false)
	{
		if (addrLen == sizeof(addr6))
		{
			memcpy(&addr6, addr_, addrLen);
			isIP6 = true;
		}
		else if (addrLen == sizeof(addr))
		{
			memcpy(&addr, addr_, addrLen);	
			isIP6 = false;
		}
		else
		{
			throw SocketAddressException(
				L"Unknown socket address size");
		}
	}

	InnerData(const InnerData &copy)
	: isIP6(copy.isIP6), isBroadcast(copy.isBroadcast), addr(copy.addr),
		addr6(copy.addr6), protocol(copy.protocol)
	{}

	bool operator==(const InnerData &equal) const
	{
		bool result = false;
		if (isIP6)
		{
			if (isBroadcast)
			{
				result = equal.isBroadcast;
			}
			else
			{
				if (equal.isIP6)
				{
					result = (addr6.sin6_addr.s6_addr == 
						equal.addr6.sin6_addr.s6_addr);
				}
				else
				{
					result = false;
				}
			}
		}
		else
		{
			if (isBroadcast)
			{
				result = equal.isBroadcast;
			}
			else
			{
				if (! equal.isIP6)
				{
					result = (addr.sin_addr.s_addr == 
						equal.addr.sin_addr.s_addr);
				}
				else
				{
					result = false;
				}
			}
		}

		return result;
	}

	bool isIP6;
	bool isBroadcast;
	sockaddr_in addr;
	sockaddr_in6 addr6;
	Protocol protocol;
};

SocketAddress::SocketAddress(const Host &hst, const Service &serv)
: innerData(*(new InnerData(hst, serv)))
{}

SocketAddress::SocketAddress(const Service &serv, bool isBroadcast, bool isIP6)
: innerData(*(new InnerData(serv, isBroadcast, isIP6)))
{}

SocketAddress::SocketAddress(const Protocol &proto, const sockaddr *inaddr, 
                             int inaddrlen)
: innerData(*(new InnerData(proto, inaddr, inaddrlen)))
{}

SocketAddress::SocketAddress(const SocketAddress &copy)
: innerData(*(new InnerData(copy.innerData)))
{}

SocketAddress::~SocketAddress()
{
	delete &innerData;
}

SocketAddress &SocketAddress::operator=(const SocketAddress &equal) 
{
	if (this == &equal) return *this;
	innerData = equal.innerData;
	return *this;
}

bool SocketAddress::operator==(const SocketAddress &equal) const
{
	return (innerData == equal.innerData);
}

bool SocketAddress::isIP6() const
{
	return innerData.isIP6;
}

bool SocketAddress::isBroadcast() const
{
	return innerData.isBroadcast;
}

const Protocol &SocketAddress::getProtocol() const
{
	return innerData.protocol;
}

short SocketAddress::getPort() const
{
	short port;
	if (innerData.isIP6)
	{
		port = ntohs(innerData.addr6.sin6_port);
	}
	else
	{
		port = ntohs(innerData.addr.sin_port);
	}

	return port;
}

/*
const in_addr &SocketAddress::getInAddr() const
{
	if (innerData.isIP6)
	{
		throw NetException(L"SocketAddress is an IPv6 address, expected IPv4");
	}
	else
	{
		return innerData.addr.sin_addr;
	}
}
*/

/*
const in6_addr &SocketAddress::getIn6Addr() const
{
	if (! innerData.isIP6)
	{
		throw NetException(L"SocketAddress is an IPv6 address, expected IPv4");
	}
	else
	{
		return innerData.addr6.sin6_addr;
	}
}
*/

size_t SocketAddress::getSockAddrV4Size()
{
	return sizeof(struct sockaddr_in);
}

size_t SocketAddress::getSockAddrV6Size()
{
	return sizeof(struct sockaddr_in6);
}

const sockaddr *SocketAddress::getSockAddr() const
{
	const sockaddr *result = 0;
	if (innerData.isIP6)
	{
		result = reinterpret_cast<const sockaddr *>(&(innerData.addr6));
	}
	else
	{
		result = reinterpret_cast<const sockaddr *>(&(innerData.addr));
	}
	return result;
}

size_t SocketAddress::getSockAddrSize() const
{
	size_t result = 0;
	if (innerData.isIP6)
	{
		result = sizeof(struct sockaddr_in6);
	}
	else
	{
		result = sizeof(struct sockaddr_in);
	}
	return result;
}


}
