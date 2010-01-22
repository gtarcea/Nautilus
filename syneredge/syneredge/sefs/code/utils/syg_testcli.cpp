#include "SocketBase.hpp"
#include "SocketAddress.hpp"
#include "Net.hpp"
#include "StringConversion.hpp"
#include "Timer.hpp"

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

		std::wcout << L"making socket" << std::endl;
		ClientSocket sock(addr);
		sock.setTimeout(10000);

		std::wcout << L"connecting socket" << std::endl;
		sock.connectSocket();

		std::wcout << L"sending hello" << std::endl;
		sock.sendSocket("hello");
		std::wcout << L"receiving" << std::endl;
		std::string msgback = sock.recvSocket(5);
		if (msgback.size() != 5)
		{
			std::wcout << L"timeout: " << StringConversion::toUTF16(msgback) << std::endl;
		}
		else
		{
			std::wcout << "recd: " << StringConversion::toUTF16(msgback) << std::endl;
		}
		Timer::sleep(2000);
		msgback = sock.recvSocket(1);
		if (msgback.size() != 1)
		{
			std::wcout << L"timeout: " << StringConversion::toUTF16(msgback) << std::endl;
		}
		else
		{
			std::wcout << "recd: " << StringConversion::toUTF16(msgback) << std::endl;
		}
		Timer::sleep(2000);
		msgback = sock.recvSocket(4);
		if (msgback.size() != 4)
		{
			std::wcout << L"timeout: " << StringConversion::toUTF16(msgback) << std::endl;
		}
		else
		{
			std::wcout << "recd: " << StringConversion::toUTF16(msgback) << std::endl;
		}

		std::wcout << L"at end of real statements" << std::endl;
	}
	catch (SynerEdgeException &e)
	{
		std::wcout << e.getMsg() << std::endl;
	}

	std::wcout << L"done" << std::endl;
}
