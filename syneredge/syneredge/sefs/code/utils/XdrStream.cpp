#include "StringConversion.hpp"
#include "XdrStream.hpp"
#include <netinet/in.h>
#include <byteswap.h>

namespace SynerEdge
{

const size_t XdrStream::BoundarySize = sizeof(int32_t);

XdrStream::XdrStream()
: internalBuffer(), readPosition(0), writePosition(0)
{}

XdrStream::~XdrStream()
{}

XdrStream::XdrStream(const std::string &instr)
: internalBuffer(instr), readPosition(0), writePosition(0)
{}

XdrStream::XdrStream(const XdrStream &copy)
: internalBuffer(copy.internalBuffer.str()), readPosition(copy.readPosition),
  writePosition(copy.writePosition)
{}

XdrStream &XdrStream::operator=(const XdrStream &equal)
{
	internalBuffer.str(equal.internalBuffer.str());
	readPosition = equal.readPosition;
	writePosition = equal.writePosition;
}

XdrStream &XdrStream::operator>>(int32_t &val)
{
	size_t round = sizeof(int32_t);
	char buf[round];
	internalBuffer.seekg(readPosition, std::ios::beg);
	std::streamsize numread = internalBuffer.readsome(buf, round);
	readPosition += numread;

	if (numread != round)
	{
		throw XdrStreamException(L"Buffer size too small to read int32.");
	}

	val = ntohl(*(reinterpret_cast<int32_t *>(buf)));
	return *this;
}

XdrStream &XdrStream::operator>>(uint32_t &val)
{
	int32_t signedval;
	(*this) >> signedval;
	val = static_cast<uint32_t>(signedval);
	return *this;
}

XdrStream &XdrStream::operator>>(int64_t &val)
{
	size_t round = sizeof(int64_t);
	char buf[round];
	internalBuffer.seekg(readPosition, std::ios::beg);
	std::streamsize numread = internalBuffer.readsome(buf, round);
	readPosition += numread; 

	if (numread != round)
	{
		throw XdrStreamException(L"Buffer size too small to read int64.");
	}

	val = ntohll(*(reinterpret_cast<int64_t *>(buf)));
	return *this;
}

XdrStream &XdrStream::operator>>(uint64_t &val)
{
	int64_t signedval;
	(*this) >> signedval;
	val = static_cast<uint64_t>(signedval);	
	return *this;
}

XdrStream &XdrStream::operator>>(std::string &val)
{
	int32_t len;
	(*this) >> len;

	size_t round = roundToBoundary(len);
	size_t lastpos = static_cast<size_t>(internalBuffer.tellp());
	if ((lastpos - readPosition) < round)
	{
		throw XdrStreamException(L"Buffer to small to read string");
	}

	char buf[round];
	internalBuffer.seekg(readPosition, std::ios::beg);
	std::streamsize numread = internalBuffer.readsome(buf, round);
	readPosition += numread;

	if (numread != round)
	{
		throw XdrStreamException(L"Buffer too small to read string");
	}

	val = std::string(buf, len);
	return *this;
}

XdrStream &XdrStream::operator>>(float &val)
{
	size_t round = sizeof(float);
	char buf[round];
	internalBuffer.seekg(readPosition, std::ios::beg);
	std::streamsize numread = internalBuffer.readsome(buf, round);
	readPosition += numread;

	if (numread != round)
	{
		throw XdrStreamException(L"Buffer to small to read float");
	}

	val = *(reinterpret_cast<float *>(buf));
	return *this;
}

XdrStream &XdrStream::operator>>(double &val)
{
	size_t round = sizeof(double);
	char buf[round];
	internalBuffer.seekg(readPosition, std::ios::beg);
	std::streamsize numread = internalBuffer.readsome(buf, round);
	readPosition += numread;

	if (numread != round)
	{
		throw XdrStreamException(L"Buffer to small to read double");
	}

	val = *(reinterpret_cast<double *>(buf));
	return *this;
}

XdrStream &XdrStream::operator>>(bool &val)
{
	int32_t bval;
	(*this) >> bval;

	if (bval == 1)
		val = true;
	else
		val = false;
	return *this;
}

XdrStream &XdrStream::operator>>(XdrStream &val)
{
	std::string mystr;
	(*this) >> mystr;

	//std::wcout << L"pulled xdr string that is " << mystr.size()
	//	<< " chars long." << std::endl;
	val.internalBuffer.str(mystr);
	val.readPosition = 0;
	val.writePosition = mystr.size();

	return *this;
}

XdrStream &XdrStream::operator>>(std::wstring &val)
{
	std::string utf8;
	(*this) >> utf8;
	val = StringConversion::toUTF16(utf8);

	return *this;
}


int64_t XdrStream::ntohll(int64_t val)
{
	if (htonl(1) != 1) 
		return bswap_64(val);
	else
		return val;
}

int64_t XdrStream::htonll(int64_t val)
{
	if (htonl(1) != 1) 
		return bswap_64(val);
	else
		return val;
}

size_t XdrStream::roundToBoundary(size_t insize)
{
	return
	((size_t) ((insize + BoundarySize - 1) / BoundarySize)) * BoundarySize;
}

XdrStream &XdrStream::operator<<(int32_t val)
{
	size_t round = sizeof(int32_t);
	char buf[round];
	*(reinterpret_cast<int32_t *>(buf)) = htonl(val);
	internalBuffer.seekp(writePosition, std::ios::beg);
	internalBuffer.write(buf, round);
	writePosition += round;
	return *this;	
}

XdrStream &XdrStream::operator<<(uint32_t val)
{
	(*this) << static_cast<int32_t>(val);
	return *this;
}

XdrStream &XdrStream::operator<<(int64_t val)
{
	size_t round = sizeof(int64_t);
	char buf[round];
	*(reinterpret_cast<int64_t *>(buf)) = htonll(val);
	internalBuffer.seekp(writePosition, std::ios::beg);
	internalBuffer.write(buf, round);
	writePosition += round;
	return *this;	
}

XdrStream &XdrStream::operator<<(uint64_t val)
{
	(*this) << static_cast<int64_t>(val);
	return *this;
}

XdrStream &XdrStream::operator<<(const char *val)
{
	(*this) << std::string(val);
	return *this;
}

XdrStream &XdrStream::operator<<(const std::string &val)
{
	internalBuffer.seekp(writePosition, std::ios::beg);
	(*this) << static_cast<uint32_t>(val.size());

	internalBuffer.write(val.c_str(), val.size());

	size_t round = roundToBoundary(val.size());
	for (size_t i = 0; i < round - val.size(); i++)
	{
		internalBuffer.write("\0", 1);
	}
	writePosition += round; 
	return *this;	
}

XdrStream &XdrStream::operator<<(float val)
{
	size_t round = sizeof(float);
	char buf[round];
	*(reinterpret_cast<float *>(buf)) = val;
	internalBuffer.seekp(writePosition, std::ios::beg);
	internalBuffer.write(buf, round);
	writePosition += round; 
	return *this;	
}

XdrStream &XdrStream::operator<<(double val)
{
	size_t round = sizeof(double);
	char buf[round];
	*(reinterpret_cast<double *>(buf)) = val;
	internalBuffer.seekp(writePosition, std::ios::beg);
	internalBuffer.write(buf, round);
	writePosition += round; 
	return *this;	
}

XdrStream &XdrStream::operator<<(bool val)
{
	int32_t bval = (val) ? 1 : 0;
	(*this) << bval;

	return *this;
}

XdrStream &XdrStream::operator<<(const XdrStream &val)
{
	std::string mystr = val.internalBuffer.str();
	//std::wcout << L"encoding xdrbuffer that is : " << mystr.size() << L" bytes long" << std::endl;
	(*this) << val.internalBuffer.str();
	return *this;
}

XdrStream &XdrStream::operator<<(const wchar_t *val)
{
	(*this) << std::wstring(val);
	return *this;
}

XdrStream &XdrStream::operator<<(const std::wstring &val)
{
	(*this) << StringConversion::toUTF8(val);
	return *this;
}


std::string XdrStream::getAndClearBuffer()
{
	std::string result = internalBuffer.str();
	internalBuffer.clear();
	internalBuffer.str("");
	readPosition = 0;
	writePosition = 0;
	internalBuffer.seekp(0, std::ios::beg);
	internalBuffer.seekg(0, std::ios::beg);
	return result;
}

void XdrStream::reportSize()
{
	std::string result = internalBuffer.str();
	std::wcout << "reportSize: " << result.size() << std::endl;
	for (size_t i = 0; i < result.size(); i++)
	{
		std::wcout << L"result[" << i << L"]=" << (int) result[i]
		<< std::endl;
	}
}

void XdrStream::addBufferWithoutCounting(const std::string &val)
{
	internalBuffer.write(val.c_str(), val.size());
}

ClientSocket &operator<<(ClientSocket &soc, XdrStream &xdr)
{
	boost::mutex::scoped_lock lk(soc.getMutex());
	std::string xdrstr = xdr.getAndClearBuffer();

	char buf[sizeof(int32_t)];
	*(reinterpret_cast<int32_t *>(buf)) = 
		htonl(static_cast<int32_t>(xdrstr.size()));
	soc.sendSocket(std::string(buf, sizeof(buf)));

	soc.sendSocket(xdrstr);
	return soc;
}

ClientSocket &operator>>(ClientSocket &soc, XdrStream &xdr)
{
	boost::mutex::scoped_lock lk(soc.getMutex());
	std::string xdrsizestr = soc.recvSocket(sizeof(int32_t));
	int32_t xdrsize = ntohl
		(*(reinterpret_cast<const int32_t *>(xdrsizestr.c_str())));
	
	std::string xdrbuffer = 
		soc.recvSocket(static_cast<size_t>(xdrsize));

	xdr.addBufferWithoutCounting(xdrbuffer);
	return soc;
}

}

