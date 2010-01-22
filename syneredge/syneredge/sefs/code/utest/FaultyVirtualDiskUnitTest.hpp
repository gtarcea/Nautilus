
#include <cppunit/extensions/HelperMacros.h>
#include "VirtualDisk.hpp"

class FaultyVirtualDiskUnitTest : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE(FaultyVirtualDiskUnitTest) ;
	CPPUNIT_TEST(testConstructor) ;
	CPPUNIT_TEST(testCreateFile) ;
	CPPUNIT_TEST(testCreateFileWithFailures) ;
	CPPUNIT_TEST(testOpenFile) ;
	CPPUNIT_TEST(testReadWriteBlocks) ;
	CPPUNIT_TEST(testZeroAndFreeBlocks) ;
	CPPUNIT_TEST(testFreeBlockListMethods) ;
	CPPUNIT_TEST_SUITE_END() ;
public:
	void setUp() ;
	void tearDown() ;
	void testConstructor() ;
	void testCreateFile() ;
	void testCreateFileWithFailures() ;
	void testOpenFile() ;
	void testReadWriteBlocks() ;
	void testZeroAndFreeBlocks() ;
	void testFreeBlockListMethods() ;

private:
	SynerEdge::VirtualDisk *vd ;
	bool diskcreated ;
} ;
