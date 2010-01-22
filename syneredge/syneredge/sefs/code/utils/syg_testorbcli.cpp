#include "SocketBase.hpp"
#include "SocketAddress.hpp"
#include "Net.hpp"
#include "StringConversion.hpp"
#include "Timer.hpp"
#include "OrbBase.hpp"
#include "MyTestServerInterface.hpp"

#include <iostream>

using namespace SynerEdge;

int main(int argc, char **argv)
{
	try
	{
		Protocol tcp(L"tcp");
		std::wcout << L"make proto: " << &tcp << std::endl;
		std::wcout << L"making host" << std::endl;
		Host hst(L"localhost", false);
		std::wcout << L"making serv" << std:: endl;
		Service serv(L"sygsrv", tcp);

		std::wcout << L"making addr" << std::endl;
		SocketAddress addr(hst, serv);

		std::wcout << hst.getIP().toString() << std::endl;
		std::wcout << serv.getPort() << std::endl;

		std::wcout << L"constructing orb" << std::endl;
		ClientOrb orb(addr, L"", L"");
		std::wcout << L"setting timeout" << std::endl;
		orb.setTimeout(10000);
		std::wcout << L"starting orb" << std::endl;
		orb.start();

		std::wcout << L"creating client interface" << std::endl;
		MyTest_0_1_ClientInterface mytest(orb);

		std::wcout << L"calling func1" << std::endl;
		mytest.func1(L"ravi", 27);

		std::wcout << L"calling func2" << std::endl;
		std::wstring f2res = mytest.func2(17.5, 8);

		std::wcout << L"f2res=" << f2res << std::endl;

		Timer::sleep(5000);

		std::wcout << L"at end of real statements" << std::endl;
	}
	catch (SynerEdgeException &e)
	{
		std::wcout << e.getMsg() << std::endl;
	}

	std::wcout << L"done" << std::endl;
}
