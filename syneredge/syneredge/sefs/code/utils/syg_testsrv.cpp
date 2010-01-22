#include "Net.hpp"
#include "SocketAddress.hpp"
#include "SocketBase.hpp"
#include "StringConversion.hpp"
#include "Timer.hpp"

#include <iostream>

using namespace SynerEdge;

int main(int argc, char **argv)
{

	try
	{
		Protocol tcp(L"tcp");
		//Host hst(L"localhost", false);
		Service serv(L"sygsrv", tcp);

		std::wcout << L"tcpserversocket make: " << std::endl;
		TCPServerSocket socket(serv, false, 10);
		socket.setReuseAddress(true);
		std::wcout << L"listening..." << std::endl;
		socket.listenSocket();

		std::wcout << "accepting" << std::endl;
		ClientSocket *cli = socket.acceptSocket();	
		cli->setTimeout(10000);

		std::wcout << "receiving" << std::endl;
		std::string str = cli->recvSocket(5);

		if (str.size() != 5)
		{
			std::wcout << L"timeout: " << StringConversion::toUTF16(str) << std::endl;
		}
		else
		{
			std::wcout << "msg: " << StringConversion::toUTF16(str) << std::endl;
			cli->sendSocket(str + "-back");
		}

		std::wcout << "sleeping" << std::endl;

		delete cli;
		std::wcout << "done" << std::endl;
	}
	catch (SynerEdgeException &e)
	{
		std::wcout << e.getMsg() << std::endl;
	}
};
