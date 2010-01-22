#include "StorProvSvrInterfaceUnitTest.hpp"
#include "StorProvSvrInterface.hpp"
#include <iostream>
using namespace std ;
using namespace SynerEdge ;

// CPPUNIT_TEST_SUITE_REGISTRATION(StorProvSvrInterfaceUnitTest) ;

void
StorProvSvrInterfaceUnitTest::setUp()
{
	bf = new BlockFile("/tmp/bf") ;

	if (! bf->open()) {
		bf->create(1, 100, 1024) ;
		CPPUNIT_ASSERT(bf->open() == true) ;
	}
}

void
StorProvSvrInterfaceUnitTest::tearDown()
{
	delete bf ;
}

void 
StorProvSvrInterfaceUnitTest::testConstructor()
{
	StorProvSvrInterface spi((*bf)) ;
	Context ctx ;
	XdrStream xd ;
	int64 numblocks ;
	XdrStream *xdresult = spi.getNumBlocks(ctx, xd) ;
	(*xdresult) >> numblocks ;
	CPPUNIT_ASSERT(numblocks == 100) ;
}

void 
StorProvSvrInterfaceUnitTest::testGets()
{
	StorProvSvrInterface spi((*bf)) ;
	Context ctx ;
	XdrStream xd ;
	int64 numblocks ;

	XdrStream *xdresult = spi.getNumBlocks(ctx, xd) ;
	(*xdresult) >> numblocks ;
	CPPUNIT_ASSERT(numblocks == 100) ;
	delete xdresult ;

	xdresult = spi.getBlockSize(ctx, xd) ;
	int blocksize ;
	(*xdresult) >> blocksize ;
	CPPUNIT_ASSERT(blocksize == 1024) ;
	delete xdresult ;

	xdresult = spi.getNumFreeBlocks(ctx, xd) ;
	(*xdresult) >> numblocks ;
	CPPUNIT_ASSERT(numblocks == 100) ;
	delete xdresult ;

	xdresult = spi.getFreeBlock(ctx, xd) ;
	int64 blocknum ;
	(*xdresult) >> blocknum ;
	CPPUNIT_ASSERT(blocknum == 1) ;
	XdrStream *xdresult2 = spi.getNumFreeBlocks(ctx, xd) ;
	(*xdresult2) >> numblocks ;
	CPPUNIT_ASSERT(numblocks == 99) ;
	delete xdresult ;
	delete xdresult2 ;

	InvokeRequest ir ;
	ir.parameters << blocknum ;
	XdrStream xd2 ;
	xd2 << ir ;
	InvokeRequest ir2 ;
	xd2 >> ir2 ;

	xdresult = spi.releaseBlock(ctx, ir2.parameters) ;
	bool rc ;
	(*xdresult) >> rc ;
	CPPUNIT_ASSERT(rc == true) ;
	delete xdresult ;

	xdresult = spi.getNumFreeBlocks(ctx, xd) ;
	(*xdresult) >> numblocks ;
	CPPUNIT_ASSERT(numblocks == 100) ;
	delete xdresult ;
}

void
StorProvSvrInterfaceUnitTest::testReadWrite()
{
	StorProvSvrInterface spi((*bf)) ;
	Context ctx ;
	XdrStream xd ;
	InvokeRequest ir; // Need to build this as this is how we will
			// receive requests in the server.
	XdrStream *xdresult = spi.getFreeBlock(ctx, xd) ;
	uint64 blocknum ;
	(*xdresult) >> blocknum ;
	CPPUNIT_ASSERT(blocknum != 0) ;
	delete xdresult ;

	int offset = 0 ;
	string data = "abc123" ;
	int size = data.size() ;
	ir.parameters << blocknum << offset << size << data ;
	XdrStream xd2 ;
	xd2 << ir ;
	InvokeRequest ir2 ;
	xd2 >> ir2 ;
	xdresult = spi.writeBlock(ctx, ir2.parameters) ;
	bool rc ;
	(*xdresult) >> rc ;
	CPPUNIT_ASSERT(rc == true) ;
	delete xdresult ;

	InvokeRequest ir3 ;
	ir3.parameters << blocknum ;
	XdrStream xd3 ;
	xd3 << ir3 ;
	xd3 >> ir2 ;
	xdresult = spi.readBlock(ctx, ir2.parameters) ;
	(*xdresult) >> rc ;
	uint64 blocknumback ;
	(*xdresult) >> blocknumback ;
	int numbytes ;
        (*xdresult) >> numbytes ;
        (*xdresult) >> size ;
	string data2 ;
        (*xdresult) >> data2 ;
	CPPUNIT_ASSERT(data2 == "abc123") ;
	CPPUNIT_ASSERT(blocknumback == blocknum) ;
	CPPUNIT_ASSERT(rc == true) ;

	InvokeRequest newir ;
	newir.parameters << blocknum ;
	InvokeRequest ir4 ;
	XdrStream xd4 ;
	xd4 << newir ;
	xd4 >> ir4 ;
	xdresult = spi.zeroBlock(ctx, ir4.parameters) ;
	(*xdresult) >> rc ;
	CPPUNIT_ASSERT(rc == true) ;
	delete xdresult ;

	InvokeRequest anewir ;
	anewir.parameters << blocknum ;
	InvokeRequest ir5 ;
	xd2 << anewir ;
	xd2 >> ir5 ;
	xdresult = spi.readBlock(ctx, ir5.parameters) ;
	(*xdresult) >> rc ;
	(*xdresult) >> blocknum ;
        (*xdresult) >> numbytes ;
        (*xdresult) >> size ;
        (*xdresult) >> data2 ;
	CPPUNIT_ASSERT(numbytes == 0) ;
	CPPUNIT_ASSERT(size == 1024) ;
}

void
StorProvSvrInterfaceUnitTest::testOther()
{
	StorProvSvrInterface spi((*bf)) ;
	Context ctx ;
	XdrStream xd ;
	InvokeRequest ir; // Need to build this as this is how we will
			// receive requests in the server.
	
	bool rc ;
	XdrStream *xdresult = spi.flushFreeBlockList(ctx, xd) ;
	(*xdresult) >> rc ;
	CPPUNIT_ASSERT(rc == true) ;
	delete xdresult ;

	system("rm -f /tmp/bf*") ;
}
