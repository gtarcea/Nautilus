
#include <cppunit/extensions/HelperMacros.h>

class Class_Name_Here : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE(Class_Name_Here) ;
	CPPUNIT_TEST(Unit_Test_Here) ;
	CPPUNIT_TEST_SUITE_END() ;
public:
	void setUp() ;
	void tearDown() ;
	void Unit_Test_Here() ;

private:
} ;
