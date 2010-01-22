#include "NetworkBlockFileUnitTest.hpp"
#include "BlockFile.hpp"
#include "Block.hpp"
#include "SocketBase.hpp"
#include "Timer.hpp"
#include <stdlib.h>
#include <iostream>
using namespace std ;
using namespace SynerEdge ;

// CPPUNIT_TEST_SUITE_REGISTRATION(NetworkBlockFileUnitTest) ;

void
NetworkBlockFileUnitTest::setUp()
{
	system("$SYGHOME/code/utest/setupstorprovtest.sh START") ;
	system("syg_storprovider > /dev/null 2>&1 &") ;
	Timer::sleep(1000) ;
	//cout << "Setting up socket" << endl ;
	Protocol tcp(L"tcp");
	Host hst(L"localhost", false);
	Service serv(L"sygsrv", tcp);

	//std::wcout << L"tcpserversocket make: " << std::endl;
	socket = new TCPServerSocket(serv, false, 10);
	socket->setReuseAddress(true);
	socket->listenSocket();
	cli = socket->acceptSocket();
	cli->setTimeout(10000);
	bf = new NetworkBlockFile((*cli)) ;
}

void
NetworkBlockFileUnitTest::tearDown()
{
//	cout << "NetworkBlockFileUnitTest::tearDown()" << endl ;
	delete bf ;
	delete socket ;
	delete cli ;
	system("$SYGHOME/code/utest/setupstorprovtest.sh STOP") ;
//	delete tb1 ;
//	delete tb2 ;
}

void NetworkBlockFileUnitTest::testOpenFile()
{
//	cout << "NetworkBlockFileUnitTest::testOpenFile()" << endl ;
	CPPUNIT_ASSERT(bf->open() == true) ;
	CPPUNIT_ASSERT(bf->getNumBlocks() == 100) ;
	CPPUNIT_ASSERT(bf->getNumFreeBlocks() == 100) ;
	CPPUNIT_ASSERT(bf->getBlockSize() == 1024) ;
}

void NetworkBlockFileUnitTest::testReadWriteBlocks()
{
	char *buf = "DEF456123" ;
//	cout << "NetworkBlockFileUnitTest::testReadWriteBlocks()" << endl ;
	CPPUNIT_ASSERT(bf->open() == true) ;
	uint64 blocknum = bf->getFreeBlock() ;
	CPPUNIT_ASSERT(blocknum != 0) ;
	CPPUNIT_ASSERT(bf->writeBlock(blocknum, 0, 10, buf) == true) ;
	Block b ;
	CPPUNIT_ASSERT(bf->readBlock(blocknum, b) == true) ;
	CPPUNIT_ASSERT(b.getNumBytes() == 10) ;

	blocknum = bf->getFreeBlock() ;
	CPPUNIT_ASSERT(blocknum != 0) ;
	CPPUNIT_ASSERT(bf->writeBlock(blocknum, 0, 10, buf) == true) ;
	CPPUNIT_ASSERT(bf->readBlock(blocknum, b) == true) ;
	CPPUNIT_ASSERT(b.getNumBytes() == 10) ;
	CPPUNIT_ASSERT(b.getBlockNum() == blocknum) ;

	blocknum = bf->getFreeBlock() ;
	CPPUNIT_ASSERT(blocknum != 0) ;
	b.setBlockNum(blocknum) ;
	CPPUNIT_ASSERT(bf->writeBlock(b) == true) ;
	CPPUNIT_ASSERT(bf->readBlock(blocknum, b) == true) ;
	CPPUNIT_ASSERT(b.getNumBytes() == 10) ;
	CPPUNIT_ASSERT(b.getBlockNum() == blocknum) ;
}

void
NetworkBlockFileUnitTest::testZeroAndFreeBlocks()
{
	Block b ;
	//cout << "NetworkBlockFileUnitTest::testZeroAndFreeBlocks()" << endl ;

	CPPUNIT_ASSERT(bf->open() == true) ;
	uint64 blocknum = bf->getFreeBlock() ;
	CPPUNIT_ASSERT(blocknum != 0) ;
	CPPUNIT_ASSERT(bf->readBlock(blocknum, b) == true) ;
	CPPUNIT_ASSERT(b.getNumBytes() == 0) ;
	CPPUNIT_ASSERT(bf->zeroBlock(blocknum) == true) ;
	CPPUNIT_ASSERT(bf->readBlock(blocknum, b) == true) ;
	CPPUNIT_ASSERT(b.getNumBytes() == 0) ;
	// CPPUNIT_ASSERT(b.isFree() == false) ;
	CPPUNIT_ASSERT(bf->releaseBlock(blocknum) == true) ;
	CPPUNIT_ASSERT(bf->readBlock(1, b) == false) ;
	//CPPUNIT_ASSERT(b.isFree() == true) ;
}

void
NetworkBlockFileUnitTest::testFreeBlockListMethods()
{
	CPPUNIT_ASSERT(bf->open() == true) ;
	CPPUNIT_ASSERT(bf->getNumFreeBlocks() == 100) ;
	int64 bn = bf->getFreeBlock() ;
	CPPUNIT_ASSERT(bf->getNumFreeBlocks() == 99) ;
	CPPUNIT_ASSERT(bf->releaseBlock(bn) == true) ;
	CPPUNIT_ASSERT(bf->getNumFreeBlocks() == 100) ;
	bn = bf->getFreeBlock() ;
	CPPUNIT_ASSERT(bf->getNumFreeBlocks() == 99) ;
	bf->close() ;
	CPPUNIT_ASSERT(bf->open() == true) ;
	CPPUNIT_ASSERT(bf->getNumFreeBlocks() == 99) ;
	CPPUNIT_ASSERT(bf->flushFreeBlockList() == true) ;
}
