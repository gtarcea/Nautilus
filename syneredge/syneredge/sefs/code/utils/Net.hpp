#ifndef SynerEdge_Net_h__
#define SynerEdge_Net_h__

#include "SynerEdge.hpp"
#include "DateTime.hpp"

#include <string>
#include <list>

struct in_addr;
struct in6_addr;

namespace SynerEdge
{

class NetException : public SynerEdgeException
{
public:
	NetException(const std::wstring &msg) : SynerEdgeException(msg) {}
	virtual ~NetException() {}
};

class IPAddress
{
public:
	IPAddress(const std::wstring &ipAddrString, bool isIP6);
	IPAddress(const std::wstring &ipAddrString);
	IPAddress(const in_addr  &in);
	IPAddress(const in6_addr  &in);
	IPAddress(const IPAddress &copy);

	IPAddress &operator=(const IPAddress &equal);

	virtual ~IPAddress();
	bool operator==(const IPAddress &RHS) const;

	bool isIP6() const;
	const std::wstring &toString() const;
	const in_addr &getInAddr() const;
	const in6_addr &getIn6Addr() const;

private:
	struct InnerData;

	InnerData &innerData;
};

class Host
{
public:
	typedef std::list<std::wstring>::const_iterator AliasItor;
	typedef std::list<IPAddress>::const_iterator AddressItor;

	Host(const IPAddress &);
	Host(const std::wstring &ipOrHostName, bool isIP6);
	Host(const Host &copy);
	Host();

	virtual ~Host();
	Host &operator=(const Host &copy);

	IPAddress getIP() const;
	const std::wstring &getName() const;
	bool isIP6() const;
	DateTime getTimeStamp() const;

	const std::list<std::wstring> &getAliases() const;
	const std::list<IPAddress> &getAddresses() const;

	bool operator==(const Host &RHS) const;
private:
	struct InnerData;

	InnerData &innerData;
};

class Protocol
{
public:
	typedef std::list<std::wstring>::const_iterator AliasItor;

	Protocol(const std::wstring &protoname);
	Protocol(int number);
	Protocol(const Protocol &copy);

	virtual ~Protocol();
	Protocol &operator=(const Protocol &equal);

	const std::list<std::wstring> &getAliases() const;
	const std::wstring &getName() const;
	int getNumber() const;
	int getType() const;

	bool operator==(const Protocol &RHS) const;
private:
	struct InnerData;

	InnerData &innerData;
};

class Service
{
public:
	typedef std::list<std::wstring>::const_iterator AliasItor;

	Service(const std::wstring &servicename, const Protocol &proto);
	Service(short port, const Protocol &proto);
	Service(const Service &copy);

	Service &operator=(const Service &equal);
	virtual ~Service();

	const std::wstring &getName() const;
	short getPort() const;
	const Protocol &getProtocol() const;
	const std::list<std::wstring> &getAliases() const;

	bool operator==(const Service &RHS) const;
private:
	struct InnerData;

	InnerData &innerData;
};

}

#endif

