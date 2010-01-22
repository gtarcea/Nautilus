#include <cppunit/extensions/TestFactoryRegistry.h>
#include <cppunit/ui/text/TestRunner.h>
#include "Testutils.hpp"

int
main()
{
	CppUnit::TextUi::TestRunner runner ;
	CppUnit::TestFactoryRegistry &registry = CppUnit::TestFactoryRegistry::getRegistry() ;
	Testutils::deleteTestVirtualBlockFiles();
	runner.addTest(registry.makeTest()) ;
	bool wasSuccessful = runner.run() ;
	return ! wasSuccessful ;
}
