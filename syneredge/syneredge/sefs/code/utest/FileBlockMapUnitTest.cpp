#include "FileBlockMapUnitTest.hpp"
#include "FileBlockMap.hpp"
#include <iostream>
using namespace std ;
using namespace SynerEdge ;

// CPPUNIT_TEST_SUITE_REGISTRATION(FileBlockMapUnitTest) ;

void
FileBlockMapUnitTest::setUp()
{
}

void
FileBlockMapUnitTest::tearDown()
{
}

void
FileBlockMapUnitTest::testConstructor()
{
//	cout << "FileBlockMapUnitTest::testConstructor()" << endl ;
	system("rm -f /etc/syneredge/fbmap/passwd") ;
	FileBlockMap fb("/etc/syneredge/fbmap/passwd") ;
	FileBlockMap *fbp = new FileBlockMap("/etc/syneredge/fbmap/passwd2") ;
}

void
FileBlockMapUnitTest::testBlockMethods()
{
	//cout << "FileBlockMapUnitTest::testBlockMethods()" << endl ;
	FileBlockMap fb("/etc/syneredge/fbmap/passwd") ;
	CPPUNIT_ASSERT(fb.getBlockCount() == 0) ;
	CPPUNIT_ASSERT(fb.addBlock(100, 100) == true) ;
	CPPUNIT_ASSERT(fb.getBlockByteCount(100) == 100) ;
	CPPUNIT_ASSERT(fb.blockExists(100) == true) ;
	CPPUNIT_ASSERT(fb.blockExists(101) == false) ;
	CPPUNIT_ASSERT(fb.getBlockCount() == 1) ;
	CPPUNIT_ASSERT(fb.removeBlock(101) == false) ;
	CPPUNIT_ASSERT(fb.getBlockCount() == 1) ;
	CPPUNIT_ASSERT(fb.removeBlock(100) == true) ;
	CPPUNIT_ASSERT(fb.blockExists(100) == false) ;
	CPPUNIT_ASSERT(fb.getBlockByteCount(100) == -1) ;
	CPPUNIT_ASSERT(fb.getBlockCount() == 0) ;
	CPPUNIT_ASSERT(fb.updateBlock(100, 4) == false) ;
	CPPUNIT_ASSERT(fb.getBlockByteCount(100) == -1) ;
	CPPUNIT_ASSERT(fb.blockExists(100) == false) ;
	CPPUNIT_ASSERT(fb.getBlockCount() == 0) ;
	CPPUNIT_ASSERT(fb.addBlock(100, 100) == true) ;
	CPPUNIT_ASSERT(fb.getByteCount() == 100) ;
	CPPUNIT_ASSERT(fb.updateBlock(100, 4) == true) ;
	CPPUNIT_ASSERT(fb.getBlockByteCount(100) == 4) ;
	CPPUNIT_ASSERT(fb.blockExists(100) == true) ;
	CPPUNIT_ASSERT(fb.getBlockCount() == 1) ;
	CPPUNIT_ASSERT(fb.getByteCount() == 4) ;
	CPPUNIT_ASSERT(fb.getBlockNumAt(1) == 100) ;
	CPPUNIT_ASSERT(fb.getBlockNumAt(2) == 0) ;
}

void
FileBlockMapUnitTest::testHostMethods()
{
	//cout << "FileBlockMapUnitTest::testHostMethods()" << endl ;

	FileBlockMap fb("/etc/syneredge/fbmap/passwd") ;

	CPPUNIT_ASSERT(fb.hostExists("nosuchhost") == false) ;
	CPPUNIT_ASSERT(fb.addHost("spelljammer") == true) ;
	CPPUNIT_ASSERT(fb.hostExists("spelljammer") == true) ;
	CPPUNIT_ASSERT(fb.hostExists("nosuchhost") == false) ;
	CPPUNIT_ASSERT(fb.removeHost("spelljammer") == true) ;
	CPPUNIT_ASSERT(fb.hostExists("spelljammer") == false) ;
	CPPUNIT_ASSERT(fb.removeHost("nosuchhost") == false) ;
}

void
FileBlockMapUnitTest::testOtherMethods()
{
	//cout << "FileBlockMapUnitTest::testOtherMethods()" << endl ;

	CPPUNIT_ASSERT(FileBlockMap::exists("/tmp/nosuchfile") == false) ;
	CPPUNIT_ASSERT(FileBlockMap::exists("/etc/passwd") == true) ;
}
