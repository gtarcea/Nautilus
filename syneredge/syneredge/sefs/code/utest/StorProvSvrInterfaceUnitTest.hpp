
#include <cppunit/extensions/HelperMacros.h>
#include "BlockFile.hpp"

using namespace SynerEdge ;

class StorProvSvrInterfaceUnitTest : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE(StorProvSvrInterfaceUnitTest) ;
	CPPUNIT_TEST(testConstructor) ;
	CPPUNIT_TEST(testGets) ;
	CPPUNIT_TEST(testReadWrite) ;
	CPPUNIT_TEST(testOther) ;
	CPPUNIT_TEST_SUITE_END() ;
public:
	void setUp() ;
	void tearDown() ;
	void testConstructor() ;
	void testGets() ;
	void testReadWrite() ;
	void testOther() ;

private:
	BlockFile *bf ;
} ;
