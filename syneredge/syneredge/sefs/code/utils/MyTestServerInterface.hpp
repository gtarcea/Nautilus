#ifndef MyTestServerInterface_hh
#define MyTestServerInterface_hh

#include "OrbBase.hpp"

namespace SynerEdge
{

class MyTest_0_1_ServerInterface : public ServerInterface
{
public:
	MyTest_0_1_ServerInterface();
	~MyTest_0_1_ServerInterface() {}

	XdrStream *func1_delegate(Context &ctx, XdrStream &params);
	XdrStream *func2_delegate(Context &ctx, XdrStream &params);

	void func1_srv(const std::wstring &msg, int i);
	std::wstring func2_srv(double d, int i);
};

class MyTest_0_1_ClientInterface : public ClientInterface
{
public:
	MyTest_0_1_ClientInterface(ClientOrb &orb);
	~MyTest_0_1_ClientInterface() {}

	void func1(const std::wstring &msg, int i);
	std::wstring func2(double d, int i);

private:
	ClientOrb &orb;
};

}

#endif

