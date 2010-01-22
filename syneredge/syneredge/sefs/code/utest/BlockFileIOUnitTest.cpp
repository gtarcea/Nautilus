#include "BlockFileIOUnitTest.hpp"
#include "BlockFileIO.hpp"
#include "seerror.hpp"
#include <iostream>
using namespace std ;
using namespace SynerEdge ;

bool BlockFileIOUnitTest::blockfilecreated = false ;
// CPPUNIT_TEST_SUITE_REGISTRATION(BlockFileIOUnitTest) ;

void
BlockFileIOUnitTest::setUp()
{
	if (! blockfilecreated) {
		system("rm -f /tmp/blockfileiotest") ;
		bf = new BlockFile("/tmp/blockfileiotest") ;
		bf->create(1, 100, 1024) ;
		blockfilecreated = true ;
	} else {
		bf = new BlockFile("/tmp/blockfileiotest") ;
	}

	bfio = new BlockFileIO((*bf)) ;
}

void
BlockFileIOUnitTest::tearDown()
{
	delete bfio ;
//	delete tb1 ;
//	delete tb2 ;
}

void
BlockFileIOUnitTest::testConstructor()
{
//	cout << "BlockFileIOUnitTest::testConstructor()" << endl ;
	BlockFileIO bfiotc((*bf)) ;
}

void
BlockFileIOUnitTest::testOpenRelease()
{
//	cout << "BlockFileIOUnitTest::testOpenRelease()" << endl ;
	CPPUNIT_ASSERT(bfio->open("/etc/syneredge/fbmap/bfiotest.file", 0) == true) ;
	CPPUNIT_ASSERT(bfio->release() == true) ;
	CPPUNIT_ASSERT(bfio->release() == false) ;
	CPPUNIT_ASSERT(bfio->open("/etc/syneredge/fbmap/bfioreleasetest.file", 0) == true) ;
	CPPUNIT_ASSERT(bfio->release() == true) ;
}

void
BlockFileIOUnitTest::testSimpleReadWrite()
{
	//cout << "BlockFileIOUnitTest::testSimpleReadWrite()" << endl ;

	// Write one block
	char *buf = "ABC123" ;
	int amountWritten ;
	amountWritten = bfio->write(0, buf, 7) ;
	CPPUNIT_ASSERT(amountWritten == 7) ;

	// Read back block we just wrote
	char buf2[100] ;
	int amountRead ;
	amountRead = bfio->read(0, buf2, 7) ;
	CPPUNIT_ASSERT(amountRead == 7) ;
	string b = buf ;
	string b2 = buf2 ;
	CPPUNIT_ASSERT(b2 == b) ;

	// Test offset write and then read
	amountWritten = bfio->write(1, buf, 7) ;
	CPPUNIT_ASSERT(amountWritten == 7) ;
	amountRead = bfio->read(0, buf2, 8) ;
	CPPUNIT_ASSERT(amountRead == 8) ;
	b2 = buf2 ;
	CPPUNIT_ASSERT(b2 == "AABC123") ;

	// Test read more data than in file
	amountRead = bfio->read(0, buf2, 10) ;
	CPPUNIT_ASSERT(amountRead == 8) ;
	b2 = buf2 ;
	CPPUNIT_ASSERT(b2 == "AABC123") ;

	// Test read less data than in file
	amountRead = bfio->read(0, buf2, 4) ;
	CPPUNIT_ASSERT(amountRead == 4) ;
	buf2[4] = 0 ;
	b2 = buf2 ;
	CPPUNIT_ASSERT(b2 == "AABC") ;
}

void
BlockFileIOUnitTest::init_buffer(char *buf, int length)
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

void
BlockFileIOUnitTest::testMultiBlockReadWrite()
{
	char buf[2048] ;
	char buf2[2048] ;
	string b ;
	string b2 ;

	init_buffer(buf, 1059) ;
	// Write one large buffer bigger than our block size (which has been set at
	// 1024 bytes in these tests (see setUp) and read it back.
	buf[1059] = 0 ;
	int amountWritten = bfio->write(0, buf, 1060) ;
	CPPUNIT_ASSERT(amountWritten == 1060) ;

	int amountRead = bfio->read(0, buf2, 1060) ;
	CPPUNIT_ASSERT(amountRead == 1060) ;
	b = buf ;
	b2 = buf2 ;
	CPPUNIT_ASSERT(b == b2) ;

	// Now do some offset writing to see what we get.
	amountWritten = bfio->write(1, buf, 1060) ;
	CPPUNIT_ASSERT(amountWritten == 1060) ;
	amountRead = bfio->read(1, buf2, 1060) ;
	CPPUNIT_ASSERT(amountRead == 1060) ;
	b = buf ;
	b2 = buf2 ;
	CPPUNIT_ASSERT(b == b2) ;

	// Read 1061 and make sure writes were correct
	amountRead = bfio->read(0, buf2, 1061) ;
	init_buffer(buf+1, 1059) ;
	b = buf ;
	b2 = buf2 ;
	CPPUNIT_ASSERT(b == b2) ;

	// set buf back to expected value
	init_buffer(buf, 1059) ;

	// Test extreme reads (read where there is no data)
	amountRead = bfio->read(5000, buf2, 2) ;
	CPPUNIT_ASSERT(amountRead == 0) ;

	// Test extreme writes (begin write beyond where data is)
	amountWritten = bfio->write(5000, buf, 1000) ;
	CPPUNIT_ASSERT(amountWritten == 0) ;

	// What other tests do we need to do?
	//CPPUNIT_FAIL("still testing testMultiBlockReadWrite()") ;
}

void
BlockFileIOUnitTest::testReadWrite()
{
	//cout << "BlockFileIOUnitTest::testReadWrite()" << endl ;

	system("rm -f /etc/syneredge/fbmap/bfiotest.file") ;

	CPPUNIT_ASSERT(bfio->open("/etc/syneredge/fbmap/bfiotest.file", 0) == true) ;

	testSimpleReadWrite() ;
	testMultiBlockReadWrite() ;
}

void
BlockFileIOUnitTest::testTruncateFile()
{
	char buf[2048] ;
	char buf2[2048] ;
	string b ;
	string b2 ;
	int64 amountRead ;

	init_buffer(buf, 200) ;
	buf[200] = 0 ;

	// Put file in known good state, and make sure it worked.
	CPPUNIT_ASSERT(bfio->write(0, buf, 200) == 200) ;
	CPPUNIT_ASSERT(bfio->read(0, buf2, 200) == 200) ;
	buf2[200] = 0 ;
	b = buf ;
	b2 = buf2 ;
	CPPUNIT_ASSERT(b == b2) ;
	
	// Now truncate file
	CPPUNIT_ASSERT(bfio->truncate(50) == true) ;
	amountRead = bfio->read(0, buf2, 200) ;
	CPPUNIT_ASSERT(amountRead == 50) ;

	// Truncate file with a negative value
	CPPUNIT_ASSERT(bfio->truncate(-1) == false) ;
	CPPUNIT_ASSERT(bfio->getSize() == 50) ;
}

int
BlockFileIOUnitTest::getBlockCount()
{
	string filename = "/etc/syneredge/fbmap/bfiotest.file" ;
	FileBlockMap f(filename) ;
	return f.getBlockCount() ;
}

void
BlockFileIOUnitTest::testExtendFile()
{
	char buf[2048] ;
	char buf2[2048] ;
	string b ;
	string b2 ;
	int64 amountRead ;

	CPPUNIT_ASSERT(bfio->truncate(100) == true) ;
	CPPUNIT_ASSERT(bfio->getSize() == 100) ;
	CPPUNIT_ASSERT(getBlockCount() == 1) ;
	CPPUNIT_ASSERT(bfio->truncate(120) == true) ;
	CPPUNIT_ASSERT(bfio->getSize() == 120) ;
	CPPUNIT_ASSERT(getBlockCount() == 1) ;
	CPPUNIT_ASSERT(bfio->truncate(1024) == true) ;
	CPPUNIT_ASSERT(getBlockCount() == 1) ;
	CPPUNIT_ASSERT(bfio->truncate(2048) == true) ;
	CPPUNIT_ASSERT(getBlockCount() == 2) ;
	CPPUNIT_ASSERT(bfio->truncate(2049) == true) ;
	CPPUNIT_ASSERT(getBlockCount() == 3) ;
	
	CPPUNIT_ASSERT(bfio->truncate(5000) == true) ;
	CPPUNIT_ASSERT(bfio->getSize() == 5000) ;
	CPPUNIT_ASSERT(getBlockCount() == 5) ;

	// Read buffer. The extended portion should be all zero. From the truncate tests
	// the first 50 characters should be ok. Lets test that assumption.
	init_buffer(buf, 50) ;
	buf[50] = 0 ;
	amountRead = bfio->read(0, buf2, 200) ;
	CPPUNIT_ASSERT(amountRead == 200) ;
	b = buf ;
	//cout << endl << endl << "b = " << b << endl << endl ;
	b2 = buf2 ;
	//cout << endl << endl << "b2 = " << b2 << endl << endl ;
	//cout << endl << endl << "amountRead = " << amountRead << endl << endl ;
	CPPUNIT_ASSERT(b == b2) ;
}

void
BlockFileIOUnitTest::testTruncate()
{
	//cout << "BlockFileIOUnitTest::testTruncate()" << endl ;

	CPPUNIT_ASSERT(bfio->open("/etc/syneredge/fbmap/bfiotest.file", 0) == true) ;
	testTruncateFile() ;
	testExtendFile() ;
}

void
BlockFileIOUnitTest::testOtherMethods()
{
	//cout << "BlockFileIOUnitTest::testOtherMethods()" << endl ;
	CPPUNIT_ASSERT(bfio->blockSize() == 1024) ;
	CPPUNIT_ASSERT(bfio->getBlockFileNumBlocks() == 100) ;
	CPPUNIT_ASSERT(bfio->getBlockFileNumFreeBlocks() > 0) ;
}
