
#ifndef __iFileIO_INCLUDE_
#define __iFileIO_INCLUDE_

#include "SynerEdge.hpp"
#include <string>

using std::string ;

namespace SynerEdge {

class iFileIO {

public:
	iFileIO() {} ;
	virtual ~iFileIO() {} ;

	virtual bool open(string filename, int flags) = 0 ;
	virtual bool release() = 0 ;
	virtual int read(int64 offset, char *data, int length) = 0 ;
	virtual int write(int64 offset, const char *data, int length) = 0;
	virtual int64 getSize() = 0 ;
	virtual bool truncate(int64 at) = 0 ;
	virtual bool isAccessible(string filename, int flags) = 0 ;
	virtual int blockSize() = 0 ;
	virtual bool flush() = 0 ;

} ; // class iFileIO

} ; // namespace SynerEdge

#endif // __iFileIO_INCLUDE_
