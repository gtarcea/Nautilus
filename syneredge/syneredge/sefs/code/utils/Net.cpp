#include "Net.hpp"

#include <assert.h>
#ifndef _WIN32
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#endif

#include "StringConversion.hpp"

namespace SynerEdge
{

struct IPAddress::InnerData
{
	InnerData(const in6_addr &inAddr)
	: ip6(inAddr)
	{
		isIP6 = true;
		char buf[INET6_ADDRSTRLEN+1];
		for (int i = 0; i < INET6_ADDRSTRLEN + 1; i++) 
			buf[i] = '\0';
		const char *cs = inet_ntop(AF_INET6, 
				static_cast<const void *>(&inAddr),
				buf, INET6_ADDRSTRLEN);

		if (cs == NULL)
		{
			throw NetException(StringConversion::syserr());
		}
		ipString = StringConversion::toUTF16(cs);

	}

	InnerData(const in_addr &inAddr)
	: ip(inAddr)
	{
		isIP6 = false;
		char buf[INET_ADDRSTRLEN+1];
		for (int i = 0; i < INET_ADDRSTRLEN + 1; i++) 
			buf[i] = '\0';
		const char *cs = inet_ntop(AF_INET, 
				static_cast<const void *>(&inAddr),
				buf, INET_ADDRSTRLEN);

		if (cs == NULL)
		{
			throw NetException(StringConversion::syserr());
		}

		ipString = StringConversion::toUTF16(cs);
	}

	InnerData(bool isIP6_) : isIP6(isIP6_), ipString(), ip(), ip6()
	{}

	InnerData() 
	: isIP6(false), ipString(), ip(), ip6()
	{}

	InnerData(const InnerData &copy)
	: isIP6(copy.isIP6), ip(copy.ip), ip6(copy.ip6), ipString(copy.ipString)
	{}

	InnerData &operator=(const InnerData &equal)
	{
		isIP6 = equal.isIP6;
		ip = equal.ip;
		ip6 = equal.ip6;
		ipString = equal.ipString;
	}
	
	bool operator==(const InnerData &equal) const
	{
		return (ipString == equal.ipString);
	}

	bool isIP6;
	struct in_addr ip;
	struct in6_addr ip6;
	std::wstring ipString;
};

struct Host::InnerData
{
	void InnerData::setFromHostent(struct hostent &he)
	{
		timeStamp = DateTime();
		isIP6 = (he.h_addrtype == AF_INET6);
		if (isIP6)
		{
			in6_addr **ina = 0;

			for (ina = reinterpret_cast<in6_addr**>(he.h_addr_list);
			     ((*ina) != 0);
			     ina++)
			{
				addresses.push_back(IPAddress(*(*ina)));
			}
		}
		else
		{
			in_addr **ina = 0;

			for (ina = reinterpret_cast<in_addr**>(he.h_addr_list);
			     ((*ina) != 0);
			     ina++)
			{
				addresses.push_back(IPAddress(*(*ina)));
			}
		}

		for (char **alias = he.h_aliases; (*alias); alias++)
		{
			std::wstring al = StringConversion::toUTF16(*alias);
			aliases.push_back(al);
		}
		name = StringConversion::toUTF16(he.h_name);
	}

	InnerData()
	: name(), aliases(), addresses(), isIP6(false), timeStamp()
	{}

	InnerData(const IPAddress &ip)
	: name(ip.toString()), aliases(), addresses(), isIP6(ip.isIP6()),
	  timeStamp()
	{
		addresses.push_back(ip);
	}

	InnerData &operator=(const InnerData &equal)
	{
		name = equal.name;
		aliases = equal.aliases;
		addresses = equal.addresses;
		isIP6 = equal.isIP6;
		timeStamp = equal.timeStamp;	
	}

	InnerData(const InnerData &copy) 
	: name(copy.name), aliases(copy.aliases), addresses(copy.addresses), 
	  isIP6(copy.isIP6), timeStamp()
	{}

	bool operator==(const InnerData &equal) const
	{
		return ((name == equal.name) && 
			(addresses == equal.addresses));
	}

	std::wstring name;
	std::list<std::wstring> aliases;
	std::list<IPAddress> addresses;
	bool isIP6;
	DateTime timeStamp;
};

struct Service::InnerData
{
	void InnerData::setFromServent(const servent &se)
	{
		name = StringConversion::toUTF16(se.s_name);
	  	port = ntohs(se.s_port);
	  	protocol = Protocol(StringConversion::toUTF16(se.s_proto));

		for (char **alias = se.s_aliases;
		     (*alias);
		     alias++)
		{
			std::wstring al = StringConversion::toUTF16(*alias);
			aliases.push_back(al);
		}
	}

	InnerData(const Protocol &proto)
	: port(-1), protocol(proto), aliases(), name()
	{}
	
	InnerData(short port, const Protocol &proto)
	: port(port), protocol(proto), aliases(), name()
	{}

	InnerData(const InnerData &copy)
	: name(copy.name), port(copy.port), protocol(copy.protocol),
	  aliases(copy.aliases)
	{}

	InnerData &operator=(const InnerData &equal)
	{
		name = equal.name;		
		protocol = equal.protocol;
		aliases = equal.aliases;
		port = equal.port;	
	}

	InnerData() 
	: name(), protocol(L"tcp"), aliases(), port(-1)
	{}

	bool operator==(const InnerData &equal) const
	{
		return ((protocol == equal.protocol) &&
			(port == equal.port));
	}

	std::wstring name;
	Protocol protocol;
	std::list<std::wstring> aliases;
	short port;
};

struct Protocol::InnerData
{
	// p_proto == 17 implies UDP which requires DGRAM,
	// all other protocol numbers default to STREAM.
	void InnerData::setFromProtoent(const protoent &pe)
	{
		name = StringConversion::toUTF16(pe.p_name);
		number = pe.p_proto;
	  	typ = (pe.p_proto == 17) ? SOCK_DGRAM : SOCK_STREAM;

		for (char **alias = pe.p_aliases; 
		     (*alias);
		     alias++)
		{
			std::wstring al = StringConversion::toUTF16(*alias);
			aliases.push_back(al);
		}
	}

	InnerData(const InnerData &copy)
	: name(copy.name), aliases(copy.aliases), number(copy.number), 
	  typ(copy.typ)
	{}
	
	InnerData &operator=(const InnerData &equal)
	{
		name = equal.name;
		aliases = equal.aliases;
		number = equal.number;
		typ = equal.typ;
	}

	InnerData()
	: name(), aliases(), number(-1), typ(0)
	{}

	bool operator==(const InnerData &equal) const
	{
		return (number == equal.number);
	}

	std::wstring name;
	std::list<std::wstring> aliases;
	int number;
	int typ;
};

IPAddress::IPAddress(const in_addr &in) 
: innerData(*(new IPAddress::InnerData(in)))
{}

IPAddress::IPAddress(const in6_addr &in) 
: innerData(*(new IPAddress::InnerData(in)))
{}

IPAddress::IPAddress(const std::wstring &addr) 
: innerData(*(new IPAddress::InnerData()))
{
	int result = inet_pton(AF_INET, StringConversion::toUTF8(addr).c_str(), 
		static_cast<void *>(&(innerData.ip)));
	if (result < 0)
	{
		throw NetException(StringConversion::syserr());
	}
	else if (result == 0)
	{
		result = inet_pton(AF_INET6, 
			StringConversion::toUTF8(addr).c_str(), 
			static_cast<void *>(&(innerData.ip6)));

		innerData.isIP6 = true;
		if (result < 0)
		{
			throw NetException(StringConversion::syserr());
		}
		else if (result == 0)
		{
			throw NetException(L"String is not a valid IP address");
		}
	}
	else
	{
		innerData.isIP6 = false;
	}

	innerData.ipString = addr;
}

IPAddress::IPAddress(const std::wstring &addr, bool isIP6)
: innerData(*(new IPAddress::InnerData(isIP6)))
{
	int result = 0;
	if (isIP6)
	{
		result = inet_pton(AF_INET6, 
			StringConversion::toUTF8(addr).c_str(),
			static_cast<void *>(&(innerData.ip6)));
	}
	else
	{
		result = inet_pton(AF_INET, 
			StringConversion::toUTF8(addr).c_str(),
			static_cast<void *>(&(innerData.ip)));
	}

	if (result < 0)
	{
		throw NetException(StringConversion::syserr());
	} 
	else if (result == 0)
	{
		if (isIP6)
		{
			throw NetException(L"Invalid IP6 Address");
		}
		else
		{
			throw NetException(L"Invalid IP4 Address");
		}
	} 
	innerData.ipString = addr;
}

IPAddress::IPAddress(const IPAddress &copy)
: innerData(*(new InnerData(copy.innerData)))
{}

IPAddress &IPAddress::operator=(const IPAddress &equal)
{
	if (this == &equal) return *this;
	innerData.operator=(equal.innerData);
	return *this;
}

bool IPAddress::operator==(const IPAddress &equal) const
{
	return (innerData == equal.innerData);
}

IPAddress::~IPAddress()
{
	delete &innerData;
}

bool IPAddress::isIP6() const
{
	return innerData.isIP6;
}

const in6_addr &IPAddress::getIn6Addr() const
{
	if (! isIP6()) 
		throw NetException(
		L"Address is of wrong type, requested IP6 is IP4");

	return innerData.ip6;
}
	
const in_addr &IPAddress::getInAddr() const
{
	if (isIP6()) 
		throw NetException(
		L"Address is of wrong type, requested IP4 is IP6");

	return innerData.ip;
}

const std::wstring &IPAddress::toString() const 
{
	return innerData.ipString;
}

Host::Host(const std::wstring &ipOrHostname, bool isIP6)
: innerData(* new InnerData())
{
	char buf[16384];
	hostent he;
	hostent *heptr = 0;
	int errres;

	int af = (isIP6) ? AF_INET6 : AF_INET;
	int result = gethostbyname2_r(
		StringConversion::toUTF8(ipOrHostname).c_str(), af,
		&he, buf, sizeof(buf),
		&heptr, &errres);

	if (heptr != NULL)
	{
		innerData.setFromHostent(he);
	}
	else
	{
		IPAddress ipa(ipOrHostname, isIP6);

		if (isIP6)
		{
			result = gethostbyaddr_r(
				reinterpret_cast<const void *>(
					&(ipa.getIn6Addr())), 
				sizeof(in6_addr), AF_INET6, &he, buf,
				sizeof(buf), &heptr, &errres);
		}
		else
		{
			result = gethostbyaddr_r(
				reinterpret_cast<const void *>(
					&(ipa.getInAddr())), 
				sizeof(in_addr), AF_INET, &he, buf,
				sizeof(buf), &heptr, &errres);
		}

		if (heptr != NULL)
		{
			innerData.setFromHostent(he);
		}
		else
		{
			throw NetException(StringConversion::syserr());
		}
	}

}

Host::Host(const IPAddress &ip)
: innerData(*(new InnerData(ip)))
{}

Host::Host() 
: innerData(*(new InnerData()))
{}

IPAddress Host::getIP() const
{
	if (innerData.addresses.size() > 0)
	{
		return innerData.addresses.front();
	}
	else
	{
		throw NetException(L"No addresses for host");
	}
}

Host::Host(const Host &copy)
: innerData(*(new InnerData(copy.innerData)))
{}

bool Host::operator==(const Host &equal) const
{
	return (innerData == equal.innerData);
}

Host::~Host()
{
	delete &innerData;
}

Host &Host::operator=(const Host &equal)
{
	if (this == &equal) return *this;
	innerData.operator=(equal.innerData);
	return *this;
}

const std::wstring &Host::getName() const
{
	return innerData.name;
}

const std::list<IPAddress> &Host::getAddresses() const
{
	return innerData.addresses;
}

const std::list<std::wstring> &Host::getAliases() const
{
	return innerData.aliases;
}

bool Host::isIP6() const
{
	return innerData.isIP6;
}

DateTime Host::getTimeStamp() const
{
	return innerData.timeStamp;
}

Protocol::Protocol(const std::wstring &name)
: innerData(*(new InnerData()))
{
	protoent pe;
	protoent *peptr = 0;
	char buf[16384];

	int result = getprotobyname_r(StringConversion::toUTF8(name).c_str(), 
			&pe, buf, sizeof(buf), &peptr);
	
	if (peptr != NULL)
	{
		innerData.setFromProtoent(pe);
	}
	else
	{
		throw NetException(L"Invalid protocol name");
	}
}

Protocol::Protocol(int number)
: innerData(*(new InnerData()))
{
	protoent pe;
	protoent *peptr = 0;
	char buf[16384];

	int result = getprotobynumber_r(number, &pe, buf, sizeof(buf),
			&peptr);

	if (peptr != NULL)
	{
		innerData.setFromProtoent(pe);
	}
	else
	{
		throw NetException(L"Invalid protocol number");
	}
}

Protocol::Protocol(const Protocol &copy)
: innerData(*(new InnerData(copy.innerData)))
{}

Protocol &Protocol::operator=(const Protocol &equal)
{
	if (this == &equal) return *this;
	innerData.operator=(equal.innerData);
	return *this;
}

bool Protocol::operator==(const Protocol &equal) const
{
	return (innerData == equal.innerData);
}

Protocol::~Protocol()
{
	delete &innerData;
}

int Protocol::getType() const
{
	return innerData.typ;
}

int Protocol::getNumber() const
{
	return innerData.number;
}

const std::wstring &Protocol::getName() const
{
	return innerData.name;
}

const std::list<std::wstring> &Protocol::getAliases() const
{
	return innerData.aliases;
}

Service::Service(const std::wstring &name, const Protocol &proto)
: innerData(*(new InnerData(proto)))
{
	servent se;
	servent *septr = 0;
	char buf[16384];

	int result = getservbyname_r(StringConversion::toUTF8(name).c_str(), 
			StringConversion::toUTF8(proto.getName()).c_str(), 
			&se, buf, sizeof(buf), &septr);

	if (septr != NULL)
	{
		innerData.setFromServent(se);
	}
	else
	{
		throw NetException(L"Invalid service name/protocol");
	}
}

Service::Service(short port, const Protocol &proto)
: innerData(*(new InnerData(proto)))
{
	servent se;
	servent *septr = 0;
	char buf[16384];

	int result = getservbyport_r(port, 
			StringConversion::toUTF8(proto.getName()).c_str(),
			&se, buf, sizeof(buf), &septr);

	if (septr != NULL)
	{
		innerData.setFromServent(se);
	}
	else
	{
		innerData.port = port;
	}
}

Service::Service(const Service &copy)
: innerData(*(new InnerData(copy.innerData)))
{}

Service &Service::operator=(const Service &equal)
{
	if (this == &equal) return *this;
	innerData.operator=(equal.innerData);
	return *this;
}

bool Service::operator==(const Service &equal) const
{
	return ((innerData.port == equal.innerData.port) &&
	        (innerData.protocol == equal.innerData.protocol));
}

Service::~Service()
{
	delete &innerData;
}

short Service::getPort() const
{
	return innerData.port;
}

const std::wstring &Service::getName() const
{
	return innerData.name;
}

const std::list<std::wstring> &Service::getAliases() const
{
	return innerData.aliases;
}

const Protocol &Service::getProtocol() const
{
	return innerData.protocol;
}

}
