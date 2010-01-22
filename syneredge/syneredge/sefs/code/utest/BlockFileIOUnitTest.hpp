
#include <cppunit/extensions/HelperMacros.h>
#include "BlockFileIO.hpp"
#include "BlockFile.hpp"
using namespace SynerEdge ;
class BlockFileIOUnitTest : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE(BlockFileIOUnitTest) ;
	CPPUNIT_TEST(testConstructor) ;
	CPPUNIT_TEST(testOpenRelease) ;
	CPPUNIT_TEST(testReadWrite) ;
	CPPUNIT_TEST(testTruncate) ;
	CPPUNIT_TEST(testOtherMethods) ;
	CPPUNIT_TEST_SUITE_END() ;
public:
	void setUp() ;
	void tearDown() ;
	void testConstructor() ;
	void testOpenRelease() ;
	void testReadWrite() ;
	void testTruncate() ;
	void testOtherMethods() ;

private:
	void testSimpleReadWrite() ;
	void testMultiBlockReadWrite() ;
	void testTruncateFile() ;
	void testExtendFile() ;
	void init_buffer(char *buf, int length) ;
	int getBlockCount() ;

	static bool blockfilecreated ;
	BlockFile *bf ;
	BlockFileIO *bfio ;
} ;
