#include "NetworkVirtualBlockFileUnitTest.hpp"
#include "SocketBase.hpp"
#include "Timer.hpp"
#include "VirtualBlockFile.hpp"
#include "BlockFile.hpp"
#include "NetworkBlockFile.hpp"
#include "Block.hpp"
#include <stdlib.h>
#include <iostream>

using namespace std ;
using namespace SynerEdge ;
using SynerEdge::VirtualBlockFile ;

// CPPUNIT_TEST_SUITE_REGISTRATION(NetworkVirtualBlockFileUnitTest) ;

static SynerEdge::BlockFile *bf1 = NULL;
static SynerEdge::NetworkBlockFile *bf2 = NULL;
static SynerEdge::BlockFile *bf3 = NULL;
static SynerEdge::TCPServerSocket *socket  = NULL;
static SynerEdge::ClientSocket *cli  = NULL;


void
NetworkVirtualBlockFileUnitTest::setUp()
{
	cout << "NetworkVirtualBlockFileUnitTest::setUp()" << endl ;
	// Start from a clean system

	system("rm -f /tmp/bf1*");
	system("rm -f /tmp/bf3*");
	system("rm -f /tmp/mybfe1*") ;
	system("rm -f /tmp/myvbf*") ;

	bf1 = new BlockFile("/tmp/bf1") ;
	bf3 = new BlockFile("/tmp/bf3") ;

	// setup up a network block file

	system("$SYGHOME/code/utest/setupstorprovtest.sh START") ;
	system("syg_storprovider > /dev/null 2>&1 &") ;
	Timer::sleep(1000) ;
	//cout << "Setting up socket" << endl ;
	Protocol tcp(L"tcp");
	Host hst(L"localhost", false);
	Service serv(L"sygsrv", tcp);

	//std::wcout << L"tcpserversocket make: " << std::endl;
	socket = new TCPServerSocket(serv, false, 10);
	socket->setNoDelay(true) ;
	socket->setReuseAddress(true);
	socket->listenSocket();
	cli = socket->acceptSocket();
	cli->setTimeout(2000);
	bf2 = new NetworkBlockFile((*cli)) ;
	
	CPPUNIT_ASSERT(bf1->create(1, 100, 1024)) ;
	bf1->close() ;

	CPPUNIT_ASSERT(bf3->create(1, 100, 1024));
	bf3->close() ;


	VirtualBlockFile::BlockFileList bflist;
	bflist.push_back(bf1) ;
	bflist.push_back(bf2) ;
	bflist.push_back(bf3) ;

	vbf = new VirtualBlockFile("vbf", bflist, 1024) ;


	// The rest of the code assumes that everything is allocated. Allocate all the blocks.

	CPPUNIT_ASSERT(vbf->open());
	for (int i = 0; i < 300; i++) {
		CPPUNIT_ASSERT(vbf->getFreeBlock() != 0);
	}
	CPPUNIT_ASSERT(vbf->getFreeBlock() == 0);
		
	
	//tb1 = new Block() ;
	//tb2 = new Block() ;
//	exit(0) ;
}

void
NetworkVirtualBlockFileUnitTest::tearDown()
{
//	cout << "NetworkVirtualBlockFileUnitTest::tearDown()" << endl ;
	delete vbf;
	delete socket;
	delete cli;

	if (bf1) {delete bf1; bf1 = NULL;}
	if (bf2) {delete bf2; bf2 = NULL;}
	if (bf3) {delete bf3; bf2 = NULL;} 
}

void
NetworkVirtualBlockFileUnitTest::testConstructor()
{
//	cout << "NetworkVirtualBlockFileUnitTest::testConstructor()" << endl ;

	VirtualBlockFile::BlockFileList bflist ;
	BlockFile bf("/tmp/mybfe1");
	bflist.push_back(&bf) ;
	VirtualBlockFile myvbf("myvbf", bflist, 1024) ;
	VirtualBlockFile *newvbf = new VirtualBlockFile("newvbf", bflist, 1024) ;
	CPPUNIT_ASSERT(newvbf != NULL);
	if (newvbf) delete newvbf;
}

void
NetworkVirtualBlockFileUnitTest::testCreateFile()
{
//	cout << "NetworkVirtualBlockFileUnitTest::testCreateFile()" << endl ;
	CPPUNIT_ASSERT(vbf->open() == true) ;
	// Test that 300 blocks were written
	Block tb ;
	// tests broken up to see behavior around disk boundaries
	for (int i = 1 ; i <= 99 ; i++) {
		CPPUNIT_ASSERT(vbf->readBlock(i, tb) == true) ;
		CPPUNIT_ASSERT(tb.getBlockNum() == i) ;
	}
	CPPUNIT_ASSERT(vbf->readBlock(100, tb) == true) ;
	CPPUNIT_ASSERT(tb.getBlockNum() == 100) ;

	for (int i = 101 ; i <= 200 ; i++) {
		CPPUNIT_ASSERT(vbf->readBlock(i, tb) == true) ;
		CPPUNIT_ASSERT(tb.getBlockNum() == i) ;
	}
	for (int i = 201 ; i <= 300 ; i++) {
		CPPUNIT_ASSERT(vbf->readBlock(i, tb) == true) ;
		CPPUNIT_ASSERT(tb.getBlockNum() == i) ;
	}
	// out-of-bound check
	CPPUNIT_ASSERT(vbf->readBlock(301, tb) == false);
	CPPUNIT_ASSERT(vbf->readBlock(0, tb) == false);
	// CPPUNIT_ASSERT(vbf->readBlock(-1, tb) == false);
}

void NetworkVirtualBlockFileUnitTest::testOpenFile()
{
	//cout << "NetworkVirtualBlockFileUnitTest::testOpenFile()" << endl ;
	// setUp has already run
	CPPUNIT_ASSERT(vbf->open() == true) ;
	CPPUNIT_ASSERT(vbf->getNumBlocks() == 300) ;
	CPPUNIT_ASSERT(vbf->getNumFreeBlocks() == 0) ;
	CPPUNIT_ASSERT(vbf->getBlockSize() == 1024) ;
	//CPPUNIT_ASSERT(vbf->getStartingBlockNum() == 1) ;

	// Test open of non-existent file
	VirtualBlockFile::BlockFileList bflist ;
	BlockFile bf("/tmp/doesnotexist");
	bflist.push_back(&bf) ;
	VirtualBlockFile nvbf("myvbf2", bflist, 1024) ;
	CPPUNIT_ASSERT(nvbf.open() == false) ;
}

void NetworkVirtualBlockFileUnitTest::testReadWriteBlocks()
{
	char *buf = "DEF456123" ;
	//cout << "NetworkVirtualBlockFileUnitTest::testReadWriteBlocks()" << endl ;
	CPPUNIT_ASSERT(vbf->open() == true) ;
	CPPUNIT_ASSERT(vbf->writeBlock(1, 0, 10, buf) == true) ;
	Block b ;
	CPPUNIT_ASSERT(vbf->readBlock(1, b) == true) ;
	CPPUNIT_ASSERT(b.getNumBytes() == 10) ;
	CPPUNIT_ASSERT(vbf->writeBlock(2, 0, 10, buf) == true) ;
	CPPUNIT_ASSERT(vbf->readBlock(2, b) == true) ;
	CPPUNIT_ASSERT(b.getNumBytes() == 10) ;
	CPPUNIT_ASSERT(b.getBlockNum() == 2) ;
	b.setBlockNum(3) ;
	CPPUNIT_ASSERT(vbf->writeBlock(b) == true) ;
	CPPUNIT_ASSERT(vbf->readBlock(3, b) == true) ;
	CPPUNIT_ASSERT(b.getNumBytes() == 10) ;
	CPPUNIT_ASSERT(b.getBlockNum() == 3) ;
}

void
NetworkVirtualBlockFileUnitTest::testZeroAndFreeBlocks()
{
	Block b ;
	//cout << "NetworkVirtualBlockFileUnitTest::testZeroAndFreeBlocks()" << endl ;

	char *buf = "DEF456123" ;
	CPPUNIT_ASSERT(vbf->open() == true) ;
	CPPUNIT_ASSERT(vbf->writeBlock(1, 0, 10, buf) == true) ;
	CPPUNIT_ASSERT(vbf->readBlock(1, b) == true) ;
	CPPUNIT_ASSERT(b.getNumBytes() != 0) ;
	CPPUNIT_ASSERT(vbf->zeroBlock(1) == true) ;
	CPPUNIT_ASSERT(vbf->readBlock(1, b) == true) ;
	CPPUNIT_ASSERT(b.getNumBytes() == 0) ;
	CPPUNIT_ASSERT(vbf->releaseBlock(1) == true) ;
	CPPUNIT_ASSERT(vbf->readBlock(1, b) == false) ;
}


void
NetworkVirtualBlockFileUnitTest::testFreeBlockListMethods()
{
	CPPUNIT_ASSERT(vbf->open() == true) ;

	// Everything was allocated previously in setup.

	CPPUNIT_ASSERT(vbf->releaseBlock(0) == false);
	CPPUNIT_ASSERT(vbf->releaseBlock(301) == false);

	for (int i = 1; i <= 300; i++) {
		CPPUNIT_ASSERT(vbf->releaseBlock(i) == true);
	}

	int64 f = vbf->getNumFreeBlocks() ;
	CPPUNIT_ASSERT(vbf->getNumFreeBlocks() == 300) ;
	int64 bn = vbf->getFreeBlock() ;
	CPPUNIT_ASSERT(vbf->getNumFreeBlocks() == 299) ;
	CPPUNIT_ASSERT(vbf->releaseBlock(bn) == true) ;
	CPPUNIT_ASSERT(vbf->getNumFreeBlocks() == 300) ;
	bn = vbf->getFreeBlock() ;
	CPPUNIT_ASSERT(vbf->getNumFreeBlocks() == 299) ;
	vbf->close() ;
	CPPUNIT_ASSERT(vbf->open() == true) ;
	CPPUNIT_ASSERT(vbf->getNumFreeBlocks() == 299) ;
	CPPUNIT_ASSERT(vbf->flushFreeBlockList() == true) ;

	system("rm -f /tmp/bf*") ;
}


