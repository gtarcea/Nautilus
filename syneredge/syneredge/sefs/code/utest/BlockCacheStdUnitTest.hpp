
#include <cppunit/extensions/HelperMacros.h>
#include "BlockCacheStd.hpp"
#include "VirtualDisk.hpp"

class BlockCacheStdUnitTest : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE(BlockCacheStdUnitTest) ;
	CPPUNIT_TEST(testzeroblock) ;
	CPPUNIT_TEST(vdisktestzeroblock) ;
	CPPUNIT_TEST(testallocatefree);
	CPPUNIT_TEST(testwrites) ;
	CPPUNIT_TEST_SUITE_END() ;
public:
	void setUp() ;
	void tearDown() ;
	void testwrites() ;
	void testzeroblock() ;
	void vdisktestzeroblock() ;
	void testallocatefree();

private:
	SynerEdge::BlockCacheStd *bcache;
	SynerEdge::VirtualDisk *vd;
};
