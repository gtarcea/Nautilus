// Unit Test the syg_storprovider server.

#include <cppunit/extensions/HelperMacros.h>
#include "SocketBase.hpp"
#include "StorProvClntInterface.hpp"

using namespace SynerEdge ;

class sygstorproviderUnitTest : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE(sygstorproviderUnitTest) ;
	CPPUNIT_TEST(testAll) ;
	CPPUNIT_TEST_SUITE_END() ;
public:
	void setUp() ;
	void tearDown() ;
	void testAll() ;
private:
	void setupSocket() ;
	TCPServerSocket *socket ;
	ClientSocket *cli ;
	StorProvClntInterface *spi ;
	static bool socketsetup ;
} ;

