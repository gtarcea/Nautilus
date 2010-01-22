
#include <cppunit/extensions/HelperMacros.h>
#include "NetworkBlockFile.hpp"

class NetworkBlockFileUnitTest : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE(NetworkBlockFileUnitTest) ;
	CPPUNIT_TEST(testOpenFile) ;
	CPPUNIT_TEST(testReadWriteBlocks) ;
	CPPUNIT_TEST(testZeroAndFreeBlocks) ;
	CPPUNIT_TEST(testFreeBlockListMethods) ;
	CPPUNIT_TEST_SUITE_END() ;
public:
	void setUp() ;
	void tearDown() ;
	void testOpenFile() ;
	void testReadWriteBlocks() ;
	void testZeroAndFreeBlocks() ;
	void testFreeBlockListMethods() ;

private:
	SynerEdge::NetworkBlockFile *bf ;
	SynerEdge::TCPServerSocket *socket ;
	SynerEdge::ClientSocket *cli ;
} ;
