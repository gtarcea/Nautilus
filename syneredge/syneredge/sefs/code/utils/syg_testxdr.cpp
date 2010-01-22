#include "XdrStream.hpp"
#include "StringConversion.hpp"

using namespace SynerEdge;

int main(int argc, char **argv)
{
	try
	{
	XdrStream x;

	x << (int32_t) 32;
	x << (float) 4.652;
	x << std::string("This is a test");
	x << (int32_t) 42;

	std::string res;
	int32_t i = 0;
	float f = 0;
	int32_t j = 0;

	x >> i >> f >> res >> j;

	std::wcout << L"i = " << i << ", f = " << f << ", res = " << StringConversion::toUTF16(res) << ", j=" << j << std::endl;

	}
	catch (SynerEdgeException &e)
	{
	std::wcout << e << std::endl;
	}
	return 0;
}
