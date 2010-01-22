// Unit Test the syg_storprovider server.
#include "sygstorproviderUnitTest.hpp"
#include "StorProvClntInterface.hpp"
#include "Timer.hpp"

// CPPUNIT_TEST_SUITE_REGISTRATION(sygstorproviderUnitTest) ;

using namespace std ;

bool socketsetup = false ;

void
sygstorproviderUnitTest::setupSocket()
{
	if (! ::socketsetup) {
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
//		socket->listenSocket();
//		cli = socket->acceptSocket();
//		cli->setTimeout(10000);
		::socketsetup = true ;
	}
}

void 
sygstorproviderUnitTest::setUp()
{
	setupSocket() ;
}

void 
sygstorproviderUnitTest::tearDown()
{
}

void 
sygstorproviderUnitTest::testAll()
{
	socket->listenSocket();
	cli = socket->acceptSocket();
	cli->setTimeout(10000);
	spi = new StorProvClntInterface((*cli)) ;

	// Test get functions
	int blocksize = spi->getBlockSize() ;
	CPPUNIT_ASSERT(blocksize == 1024) ;

	int64 numblocks = spi->getNumBlocks() ;
	CPPUNIT_ASSERT(numblocks == 100) ;

	int64 numfreeblocks = spi->getNumFreeBlocks() ;
	CPPUNIT_ASSERT(numfreeblocks == 100) ;

	// Test getting and releasing a new block
	int64 newblock = spi->getFreeBlock() ;
	CPPUNIT_ASSERT(newblock != 0) ;
	CPPUNIT_ASSERT(spi->getNumFreeBlocks() == 99) ;
	CPPUNIT_ASSERT(spi->releaseBlock(newblock) == true) ;
	CPPUNIT_ASSERT(spi->getNumFreeBlocks() == 100) ;
	// Try release the same block a second time and make sure count doesn't change
	spi->releaseBlock(newblock) ;
	CPPUNIT_ASSERT(spi->getNumFreeBlocks() == 100) ;

	// Test writing & reading a block
	string data = "abcd" ;
	newblock = spi->getFreeBlock() ;
	CPPUNIT_ASSERT(newblock != 0) ;
	CPPUNIT_ASSERT(spi->writeBlock(newblock, 0, 4, data) == true) ;

	// Read it back and check that its correct
	Block b(1024) ;
	CPPUNIT_ASSERT(spi->readBlock(newblock, b) == true) ;

	CPPUNIT_ASSERT(b.getNumBytes() == 4) ;
	string data2(b.getBytes(), b.getNumBytes()) ;
	CPPUNIT_ASSERT(data2 == data) ;

	// Zero a block, and then try reading it back.
	CPPUNIT_ASSERT(spi->zeroBlock(newblock) == true) ;
	CPPUNIT_ASSERT(spi->readBlock(newblock, b)) ;
	CPPUNIT_ASSERT(b.getNumBytes() == 0) ;

	// Test flush
	CPPUNIT_ASSERT(spi->flushFreeBlockList() == true) ;
	system("$SYGHOME/code/utest/setupstorprovtest.sh STOP") ;
}

