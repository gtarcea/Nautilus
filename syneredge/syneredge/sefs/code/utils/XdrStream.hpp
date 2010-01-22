#ifndef SynerEdge_XdrStream_h__
#define SynerEdge_XdrStream_h__

#include <sstream>
#include <stdint.h>
#include "SocketBase.hpp"

namespace SynerEdge
{

class XdrStreamException : public SynerEdgeException
{
public:
	XdrStreamException(const std::wstring &msg)
	: SynerEdgeException(msg)
	{}
	virtual ~XdrStreamException() {}
};

class XdrStream
{
public:
	XdrStream();
	XdrStream(const XdrStream &copy);
	XdrStream(const std::string &instr);
	virtual ~XdrStream();

	XdrStream &operator=(const XdrStream &equal);

	XdrStream &operator>>(int32_t &val);
	XdrStream &operator>>(uint32_t &val);
	XdrStream &operator>>(int64_t &val);
	XdrStream &operator>>(uint64_t &val);
	XdrStream &operator>>(std::string &val);
	XdrStream &operator>>(float &val);
	XdrStream &operator>>(double &val);
	XdrStream &operator>>(bool &val);
	XdrStream &operator>>(XdrStream &val);
	XdrStream &operator>>(std::wstring &val);

	XdrStream &operator<<(int32_t val);
	XdrStream &operator<<(uint32_t val);
	XdrStream &operator<<(int64_t val);
	XdrStream &operator<<(uint64_t val);
	XdrStream &operator<<(const std::string &val);
	XdrStream &operator<<(float val);
	XdrStream &operator<<(double val);
	XdrStream &operator<<(bool val);
	XdrStream &operator<<(const XdrStream &val);
	XdrStream &operator<<(const std::wstring &val);
	XdrStream &operator<<(const wchar_t *val);
	XdrStream &operator<<(const char *val);

	void addBufferWithoutCounting(const std::string &val);

	std::string getAndClearBuffer();
	void reportSize();
	size_t getBufferSize() const { return internalBuffer.str().size(); }

	static int64_t ntohll(int64_t val);
	static uint64_t ntohll(uint64_t val);
	static int64_t htonll(int64_t val);
	static uint64_t htonll(uint64_t val);

	static size_t roundToBoundary(size_t insize);

private:
	std::streamsize readPosition;
	std::streamsize writePosition;
	std::stringstream internalBuffer;

	static const size_t BoundarySize;
	
};

ClientSocket &operator<<(ClientSocket &soc, XdrStream &xdrStream);
ClientSocket &operator>>(ClientSocket &soc, XdrStream &xdrStream);

}

#endif

