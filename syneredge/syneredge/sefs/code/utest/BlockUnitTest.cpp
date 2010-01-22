#include "BlockUnitTest.hpp"
#include "Block.hpp"
#include "seerror.hpp"
#include <iostream>
#include <fstream>
using namespace std ;
using namespace SynerEdge ;



// This is specified in testOrder.
// CPPUNIT_TEST_SUITE_REGISTRATION(BlockUnitTest) ;

void
BlockUnitTest::setUp()
{
	//tb1 = new Block() ;
	//tb2 = new Block() ;
}

void
BlockUnitTest::tearDown()
{
//	delete tb1 ;
//	delete tb2 ;
}

void
BlockUnitTest::testConstructor()
{
//	cout << "BlockUnitTest::testConstructor()" << endl ;
	Block tb(1048) ;
	CPPUNIT_ASSERT(tb.getBlockNum() == 0) ;
	CPPUNIT_ASSERT(tb.getSize() == 1048) ;

	Block tb2(2, 1048) ;
	CPPUNIT_ASSERT(tb2.getSize() == 1048) ;
	CPPUNIT_ASSERT(tb2.getBlockNum() == 2) ;

	// Test boundary conditions
	try {
		Block tooBig(9000) ;
		CPPUNIT_FAIL("Allowed creation of block that is too big") ;
	} catch (seerror &exception) { }

	try {
		Block tooSmall(1) ;
		CPPUNIT_FAIL("Allowed creation of block that is too small") ;
	} catch (seerror &exception) { }

	try {
		Block negativeBlocknum(-1, 5000) ;
		CPPUNIT_FAIL("Allowed creation of block with negative blocknum") ;
	} catch (seerror &exception) { }
	
}

void
BlockUnitTest::testSetGet()
{
	//cout << "BlockUnitTest::testSetGet()" << endl ;

	Block b;  // empty block of size 0
	CPPUNIT_ASSERT(b.getSize() == 0);
	CPPUNIT_ASSERT(b.setSize(1024) == true);
	CPPUNIT_ASSERT(b.getSize() == 1024);

	Block tb(1024) ;
	char *buf = "abc123" ;

	char largebuf[2000];
	init_buffer(largebuf, 2000);
	
	CPPUNIT_ASSERT(tb.getBlockNum() == 0) ;
	CPPUNIT_ASSERT(tb.setBlockNum(-1) == false) ;
	CPPUNIT_ASSERT(tb.getBlockNum() == 0) ;
	CPPUNIT_ASSERT(tb.setBlockNum(12) == true) ;
	CPPUNIT_ASSERT(tb.getBlockNum() == 12) ;
	CPPUNIT_ASSERT(tb.getNumBytes() == 0) ;
	CPPUNIT_ASSERT(tb.setBytes(0,buf,7) == 7) ;
	CPPUNIT_ASSERT(tb.getNumBytes() == 7) ;
	CPPUNIT_ASSERT(tb.setNumBytes(4) == true) ;
	CPPUNIT_ASSERT(tb.getNumBytes() == 4) ;
	CPPUNIT_ASSERT(tb.setNumBytes(8) == false) ;
	CPPUNIT_ASSERT(tb.getNumBytes() == 4) ;

	// check for off-by-one errors
	CPPUNIT_ASSERT(tb.setBytes(0,largebuf, 1024) == 1024) ;
	CPPUNIT_ASSERT(tb.setBytes(0,largebuf, 1023) == 1023) ;
	CPPUNIT_ASSERT(tb.setBytes(0,largebuf, 1025) == 0) ;
	CPPUNIT_ASSERT(tb.setBytes(1024,largebuf,1) == 0) ;
	CPPUNIT_ASSERT(tb.setBytes(1024, largebuf, 0) == 0) ;
	CPPUNIT_ASSERT(tb.setBytes(0, largebuf, 0) == 0) ;
	CPPUNIT_ASSERT(tb.setBytes(-1, largebuf, 0) == 0) ;
	CPPUNIT_ASSERT(tb.setBytes(1023, largebuf, 1) == 1) ;
	CPPUNIT_ASSERT(tb.setBytes(1023, largebuf, INT_MAX - 1) == 0) ;

	// Now check for reads for off-by-one errors

	CPPUNIT_ASSERT(tb.getBytes() != NULL);
	CPPUNIT_ASSERT(tb.getBytes(0) != NULL);
	CPPUNIT_ASSERT(tb.getBytes(1023) != NULL);
	CPPUNIT_ASSERT(tb.getBytes(1024) == NULL);
	CPPUNIT_ASSERT(tb.getBytes(1025) == NULL);
	CPPUNIT_ASSERT(tb.getBytes(-1) == NULL);
     





	CPPUNIT_ASSERT(tb.zeroBytes(-1, 500) == false) ;
	// Check for off-by-one errors
	CPPUNIT_ASSERT(tb.zeroBytes(0, 1024) == true) ;
	CPPUNIT_ASSERT(tb.zeroBytes(0, 1023) == true) ;
	CPPUNIT_ASSERT(tb.zeroBytes(0, 1025) == false) ;
	CPPUNIT_ASSERT(tb.zeroBytes(1, 1023) == true) ;
	CPPUNIT_ASSERT(tb.zeroBytes(1, 1024) == false) ;


	CPPUNIT_ASSERT(tb.zeroBytes(1, 100) == true) ;
}

void BlockUnitTest::iotest()
{
	Block tb(2048) ;
	fstream file;
	char filename[] = "/tmp/iotest";
	tb.setBytes(0, "hello there", 12);
	char *data = tb.getBytes();
	// cout << "BlockUnitTest: iotest. Written bytes are " << tb.getBytes() << endl;
	file.open(filename, ios::in | ios::out | ios::binary) ;
	if (! file.is_open()) {
		CPPUNIT_ASSERT(false);
	}
	file.seekg(0, ios::beg) ;
	file << tb; // write the buffer to the file.
	file.close();

	Block outblock(2048);
	file.open(filename, ios::in | ios::out | ios::binary) ;
	if (! file.is_open()) {
		CPPUNIT_ASSERT(false);
	}
	file.seekg(0, ios::beg) ;
	file >> outblock; // write the buffer to the file.
	file.close();
	char *outblockdata = outblock.getBytes();
	// cout << "BlockUnitTest: iotest. Read Bytes are " << outblockdata << endl;
	CPPUNIT_ASSERT(!strcmp(data, outblockdata));   // should be equal
}


void
BlockUnitTest::init_buffer(char *buf, int length)
{
	int i, j ;
	char *abc="abcdefghijklmnopqrstuvwxy" ; /* 25 in length */

	j = 0 ;
	for(i = 0 ; i < length; i++) {
		if (j == 25) {
			j = 0 ;
		}
		buf[i] = abc[j] ;
		j++ ;
	}
}
