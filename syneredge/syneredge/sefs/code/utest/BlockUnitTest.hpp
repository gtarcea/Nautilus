
#include <cppunit/extensions/HelperMacros.h>
class BlockUnitTest : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE(BlockUnitTest) ;
	CPPUNIT_TEST(testConstructor) ;
	CPPUNIT_TEST(testSetGet) ;
	CPPUNIT_TEST(iotest);
	CPPUNIT_TEST_SUITE_END() ;
public:
	void setUp() ;
	void tearDown() ;
	void testConstructor() ;
	void testSetGet() ;
	void iotest();
	static void init_buffer(char *buf, int length);
} ;
