#include "BlockFile.hpp"
#include "stdlib.h"

using namespace SynerEdge ;
main()
{
	system("rm -f /tmp/syneredgeblocks1*") ;
	system("rm -f /etc/syneredge/fbmap/*") ;
	BlockFile bf("/tmp/syneredgeblocks1") ;
	bool rc = bf.open() ;
	if (! rc) {
		// Doesn't exist so create
		//cout << "Calling bf.create()" << endl ;
		rc = bf.create(1, 10000, 1024) ;
	}

	if (! rc) {
		return false ;
	}

	bf.close() ;
}
