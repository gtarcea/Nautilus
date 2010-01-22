#ifndef StringConversion_hpp__
#define StringConversion_hpp__

#include "SynerEdge.hpp"
#include <string>

namespace SynerEdge
{

class StringConversionException : public SynerEdgeException
{
public:
        StringConversionException(const std::wstring &msg) : SynerEdgeException(msg) {}
        ~StringConversionException() {}
};

class StringConversion
{
public:
	StringConversion() {}
	~StringConversion() {}
	// default copy constructor and operator= overload are fine.

	static std::wstring toUTF16(
		const std::wstring &prefix, 
		syg_ulong_ptr key);
	static std::wstring toUTF16(const std::string &str);
	static std::string toUTF8(const std::wstring &str);
	static std::wstring syserr();
	static std::wstring socketerr();

private:
	static std::wstring wstrerror(int errnum);

};

}

#endif
