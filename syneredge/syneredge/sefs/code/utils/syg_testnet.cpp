#include "Net.hpp"
#include "StringConversion.hpp"
#include "StartupParameters.hpp"

#include <iostream>
#include <string>

using namespace SynerEdge;

int main(int argc, char **argv)
{
	std::wstring hname = L"syneredgecvs.dnsalias.com";
	StartupParameters::instance()->storeStartupArgs(argc, argv);

	StartupParameters::argsiterator argitor = StartupParameters::instance()->args_begin();
	argitor++;
	if (argitor != StartupParameters::instance()->args_end())
	{
		hname = *argitor;
	}

	std::wcout << "hname: " << hname << std::endl;

	try
	{
	// add an IPv6 entry to your hosts table to see this work!
	// ::1	localhost
	Host local(L"localhost", true);
	std::wcout << local.getIP().toString()
		   << std::endl;

	Host hst(hname, false);
	std::wcout << hst.getName() << std::endl;
	std::wcout << hst.getIP().toString() << std::endl;
	std::list<IPAddress> addresses = hst.getAddresses();
	for (std::list<IPAddress>::iterator itor = addresses.begin();
	     itor != addresses.end();
	     itor++)
	{
		std::wcout << L"address: " 
			   << (*itor).toString()
			   << std::endl;
	}
	std::list<std::wstring> aliases = hst.getAliases();
	for (std::list<std::wstring>::iterator itor = aliases.begin();
	     itor != aliases.end();
	     itor++)	
	{
		std::wcout << L"alias: " 
			   << (*itor) << std::endl;
	}

	Protocol proto(L"tcp");
	std::wcout << L"Name: " 
		   << proto.getName() 
		   << L", Number: "
		   << proto.getNumber() << std::endl;
	
	Service serv(L"finger", proto.getName());
	std::wcout << L"Service Name: "
		   << serv.getName()
		   << L", Service Number: " 
		   << serv.getPort() << std::endl;

	} catch (SynerEdgeException &e) {
	std::wcout << e.getMsg() << std::endl;
	}

}
