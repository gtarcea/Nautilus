
#include <cppunit/extensions/HelperMacros.h>
class FreeBlockMapUnitTest : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE(FreeBlockMapUnitTest) ;
	CPPUNIT_TEST(testConstructor) ;
	CPPUNIT_TEST(testAllocateFreeMethods) ;
	CPPUNIT_TEST(testOperators) ;
	CPPUNIT_TEST_SUITE_END() ;
public:
	void setUp() ;
	void tearDown() ;
	void testConstructor() ;
	void testAllocateFreeMethods() ;
	void testOperators() ;
} ;
