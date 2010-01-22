/*
**
** This is the Storage Provider "server". Because it is most likely behind a router
** or a firewall, and thus cannot be directly connected to, the server actually
** connects up to the file system. The file system then sends requests down this link
** that the Storage Provider responds to. In this sense the server portion is reversed
** as its the Storage Provider making the initial connection as opposed to the SynerEdge
** server doing it.
**
*/

#include "SynerEdge.hpp"
#include "StorProvSvrInterface.hpp"
#include "SocketBase.hpp"
#include "StorProvOrb.hpp"
#include "StringConversion.hpp"
#include "Timer.hpp"
#include "boost/format.hpp"
#include "boost/thread/thread.hpp"
#include "boost/thread/mutex.hpp"
#include "boost/thread/condition.hpp"
#include <iostream>
#include <fstream>

using namespace SynerEdge;
using namespace std ;

BlockFile *bf ;

string hostname ;
string blockfile ;
uint64 startingblocknum ;
uint64 numblocks ;
int blocksize ;

void
fieldSPRequests()
{
//	Protocol tcp(L"tcp");
//	Service serv(L"sygsrv", tcp);

//	TCPServerSocket socket(serv, false, 10);
//	socket.setReuseAddress(true);
//	socket.listenSocket();
//	ClientSocket *cli = socket.acceptSocket();
//	cli->setTimeout(10000);

	Protocol tcp(L"tcp");
	Host hst(StringConversion::toUTF16(hostname), false);
	Service serv(L"sygsrv", tcp);
	SocketAddress addr(hst, serv);

	ClientSocket cli(addr);
	cli.setTimeout(2000);

	bool tryagain = true ;
	// Try to connect forever
	while (tryagain) {
		try {
			cli.connectSocket();
			tryagain = false ;
		} catch (SynerEdgeException &e) {
			cout << "Sleeping before retrying connect" << endl ;
			Timer::sleep(5000) ;
		}
	}

	StorProvSvrInterface spsi((*bf)) ;
	StorProvOrb spOrb(cli, spsi) ;

	spOrb.processCalls() ;
}

void
fieldRequests()
{
	fieldSPRequests() ; // Start threads here
}

bool
setupBlockfile()
{
	bf = new BlockFile(blockfile) ;

	if (bf->exists(blockfile)) {
		if (! bf->open()) {
			return false ;
		}
	} else {
		if (! bf->create(startingblocknum, numblocks, blocksize) ) {
			return false ;
		}

		if (! bf->open()) {
			return false ;
		}
	}

	return true ;
}

bool
readConfig()
{
	// File format (very simple):
	// Server hostname
	// blockfile path
	// starting blocknum
	// number of blocks
	// blocksize
	//
	// starting blocknum, number of blocks and blocksize are only used if the blockfile
	// need to be created.

	ifstream file ;

	file.open("/etc/syneredge/storprov.config", ios::in) ;
	if (! file.is_open()) {
		return false ;
	}

	if (file >> hostname) {
	} else {
		return false ;
	}

	if (file >> blockfile) {
	} else {
		return false ;
	}

	if (file >> startingblocknum) {
	} else {
		return false ;
	}

	if (file >> numblocks) {
	} else {
		return false ;
	}

	if (file >> blocksize) {
	} else {
		return false ;
	}

	file.close() ;

	return true ;
}

int main(int argc, char **argv)
{
	if (! readConfig() ) {
		cout << "Unable to read configuration file" << endl ;
		exit(1) ;
	}

	if (! setupBlockfile() ) {
		cout << "Unable to open blockfile" << blockfile << endl ;
		exit(1) ;
	}

        try
        {
		fieldRequests() ;
	}
	catch (SynerEdgeException &e)
	{
		std::wcout << e.getMsg() << std::endl;
	}
}

