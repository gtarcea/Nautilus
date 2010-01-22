#include <iostream>
#include <fstream>
#include <string>
#include "boost/format.hpp"
#include <cwchar>
#include <cerrno>
#include <cstring>
#include <sstream>

#include "StringConversion.hpp"

namespace SynerEdge
{

std::wstring StringConversion::toUTF16(const std::string &str)
{
	std::wstring result;
	std::mbstate_t state;
	memset(&state, '\0', sizeof(state));
	const char *cs = str.c_str();
	size_t l = mbsrtowcs(NULL, &cs, 0, &state);

	if (l == ((size_t) -1))
	{
		throw StringConversionException(wstrerror(errno));
	}

	wchar_t *wcs = new wchar_t[l+1];
	size_t l2 = mbsrtowcs(wcs, &cs, l, &state);

	if (l2 >= 0)
	{
		wcs[l2] = L'\0';
		result = std::wstring(wcs);
		delete[] wcs;
	}
	else
	{
		delete[] wcs;
		throw StringConversionException(wstrerror(errno));
	}
	return result;
}

std::string StringConversion::toUTF8(const std::wstring &str)
{
	std::string result;
	std::mbstate_t state;
	memset(&state, '\0', sizeof(state));
	const wchar_t *wcs = str.c_str();
	size_t l = wcsrtombs(NULL, &wcs, 0, &state);

	if (l == ((size_t) -1))
	{
		throw StringConversionException(wstrerror(errno));
	}

	char *cs = new char[l+1];

	size_t l2 = wcsrtombs(cs, &wcs, l, &state);
	if (l2 >= 0)
	{
		cs[l2] = '\0';
		result = std::string(cs, l2);
		delete[] cs;
	}
	else
	{
		delete[] cs;
		throw StringConversionException(wstrerror(errno));
	}
	return result;
}

std::wstring StringConversion::wstrerror(int errnum)
{
	std::wstring result;
	char buf[2048];
	char *res = strerror_r(errnum, buf, sizeof(buf));

	if (res == 0) 
	{
		boost::wformat fmt(L"Error - errnum: %d is not valid!");
		fmt % errnum;
		result = fmt.str();
	}
	else
	{
		try
		{
			result = toUTF16(std::string(res));
		}
		catch (StringConversionException &exp)
		{
			boost::wformat fmt(L"Message for errnum %d is corrupt");
			fmt % errnum;
			result = fmt.str();
		}
	}
	return result;
}

std::wstring StringConversion::syserr()
{
	return wstrerror(errno);
}

std::wstring StringConversion::socketerr()
{
	return wstrerror(errno);
}

std::wstring StringConversion::toUTF16(const std::wstring &prefix, syg_ulong_ptr key)
{
	std::wstringstream ss;
	ss << prefix << key;
	return ss.str();
}

}
