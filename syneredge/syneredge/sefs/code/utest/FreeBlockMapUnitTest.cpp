#include "FreeBlockMapUnitTest.hpp"
#include "FreeBlockMap.hpp"
#include "SynerEdge.hpp"
#include "seerror.hpp"
#include <iostream>
#include <fstream>
using namespace std ;
using namespace SynerEdge ;

// CPPUNIT_TEST_SUITE_REGISTRATION(FreeBlockMapUnitTest) ;

void
FreeBlockMapUnitTest::setUp()
{
}

void
FreeBlockMapUnitTest::tearDown()
{
}

void
FreeBlockMapUnitTest::testConstructor()
{
//	cout << "FreeBlockMapUnitTest::testConstructor()" << endl ;

	FreeBlockMap fbmap(1024, 400) ;

	try {
		FreeBlockMap toosmallstarting(1024,0) ;
		CPPUNIT_FAIL("Created map with too small of a starting block num") ;
	} catch (seerror &exception) {}

	//try {
	//	FreeBlockMap negativestartingat(1024,-1) ;
	//	CPPUNIT_FAIL("Created map with a negative starting block num") ;
//	} catch (seerror &exception) {}

	try {
		FreeBlockMap toofewbits(0,100) ;
		CPPUNIT_FAIL("Created map with 0 bits") ;
	} catch (seerror &exception) {}

	//try {
	//	FreeBlockMap negativebits(-1,100) ;
	//	CPPUNIT_FAIL("Created map with negative bits") ;
	//} catch (seerror &exception) {}

}

void
FreeBlockMapUnitTest::testAllocateFreeMethods()
{
	//cout << "FreeBlockMapUnitTest::testAllocateFreeMethods()" << endl ;

	int64 blocknum ;
	FreeBlockMap t(100, 100) ;

	CPPUNIT_ASSERT(t.getFreeBlockCount() == 100) ;
	blocknum = t.allocateBlock() ;
	CPPUNIT_ASSERT(blocknum >= 100) ;
	CPPUNIT_ASSERT(t.getFreeBlockCount() == 99) ;
	CPPUNIT_ASSERT(t.freeBlock(blocknum) == true) ;
	CPPUNIT_ASSERT(t.getFreeBlockCount() == 100) ;
}

void
FreeBlockMapUnitTest::testOperators()
{
	FreeBlockMap t(200, 200) ;
	FreeBlockMap t2 ;
	fstream file ;
	fstream file2 ;
	/*
	** For some reason reopening the stream with file rather than
	** file2 causes a 0 byte file to be written.
	*/

	system("rm -f /tmp/FreeBlockMapUnitTest.out") ;
	system("rm -f /tmp/FreeBlockMapUnitTest2.out") ;
	file.open("/tmp/FreeBlockMapUnitTest.out", ios::out) ;
	file << t ;
	file.close() ;
	file.open("/tmp/FreeBlockMapUnitTest.out", ios::in) ;
	file >> t2 ;
	CPPUNIT_ASSERT(t2.getFreeBlockCount() == 200) ;
	t.allocateBlock() ;
	CPPUNIT_ASSERT(t.getFreeBlockCount() == 199) ;
	file.flush() ;
	file.close() ;
	file2.open("/tmp/FreeBlockMapUnitTest2.out", ios::out) ;
	file2 << t ;
	file2.flush() ;
	file2.close() ;
	file2.open("/tmp/FreeBlockMapUnitTest2.out", ios::in) ;
	file2 >> t2 ;
	CPPUNIT_ASSERT(t2.getFreeBlockCount() == 199) ;
}
