
#include <cppunit/extensions/HelperMacros.h>
class FileBlockMapUnitTest : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE(FileBlockMapUnitTest) ;
	CPPUNIT_TEST(testConstructor) ;
	CPPUNIT_TEST(testBlockMethods) ;
	CPPUNIT_TEST(testHostMethods) ;
	CPPUNIT_TEST(testOtherMethods) ;
	CPPUNIT_TEST_SUITE_END() ;
public:
	void setUp() ;
	void tearDown() ;
	void testConstructor() ;
	void testBlockMethods() ;
	void testHostMethods() ;
	void testOtherMethods() ;
} ;
