#include "OrbBase.hpp"
#include <iostream>

#include "MyTestServerInterface.hpp"

using namespace SynerEdge;

int main(int argc, char ** argv)
{
	{
	Protocol tcp(L"tcp");
	Service serv(L"sygsrv", tcp);
	ServerOrb sorb(serv, false, 10);

	MyTest_0_1_ServerInterface mytest;

	sorb += mytest;

	std::wcout << L"Before start" << std::endl;
	sorb.start(10);
	sorb.setTimeout(12000);
	
	std::wcout << L"Before join" << std::endl;

	sorb.join();

	std::wcout << L"after join" << std::endl;
	}

	std::wcout << L"Before return" << std::endl;
	return 0;
}
