#include "MyTestServerInterface.hpp"
#include "XdrStream.hpp"

namespace SynerEdge
{

MyTest_0_1_ServerInterface::MyTest_0_1_ServerInterface() : ServerInterface(L"MyTest", L"0_1")
{
	(*this) += new MethodDelegate<MyTest_0_1_ServerInterface>(L"func1", *this, &MyTest_0_1_ServerInterface::func1_delegate);
	(*this) += new MethodDelegate<MyTest_0_1_ServerInterface>(L"func2", *this, &MyTest_0_1_ServerInterface::func2_delegate);

}

XdrStream *MyTest_0_1_ServerInterface::func1_delegate(Context &ctx, XdrStream &params)
{
	XdrStream *result = new XdrStream();
	std::wstring msg;
	int i;

	params >> msg >> i;
	func1_srv(msg, i);
	
	return result;
}


XdrStream *MyTest_0_1_ServerInterface::func2_delegate(Context &ctx, XdrStream &params)
{
	XdrStream *result = new XdrStream();
	double d; 
	int i;
	params >> d >> i;

	std::wstring funcres = func2_srv(d, i);

	(*result) << funcres;
	return result;
}


void MyTest_0_1_ServerInterface::func1_srv(const std::wstring &msg, int i)
{
	std::wcout << L"inside func1_srv: " << msg << L"-" << i << std::endl;
}

std::wstring MyTest_0_1_ServerInterface::func2_srv(double d, int i)
{
	std::wcout << L"inside func2_srv: " << d << L"-" << i << std::endl;
	return L"func2_result";
}


MyTest_0_1_ClientInterface::MyTest_0_1_ClientInterface(ClientOrb &orb)
: orb(orb), ClientInterface(L"MyTest", L"0_1")
{
}

void MyTest_0_1_ClientInterface::func1(const std::wstring &msg, int i)
{
	XdrStream params;
	params << msg << i;

	XdrStream *xdrresult = operator()(L"func1", orb.getContext(), params);

	delete xdrresult;
}

std::wstring MyTest_0_1_ClientInterface::func2(double d, int i)
{
	std::wstring result;
	XdrStream params;
	params << d << i;

	XdrStream *xdrresults = operator()(L"func2", orb.getContext(), params);

	(*xdrresults) >> result;
	delete xdrresults;

	return result;
}

}
