#include "BlockFile.hpp"
#include "VirtualBlockFile.hpp"

using namespace SynerEdge;


class Testutils {
	public:
	static SynerEdge::BlockFile *makeTestBlockFile(SynerEdge::uint64 blockcount, int blocksize);
	static  SynerEdge::VirtualBlockFile *makeTestVirtualBlockFile(char *name, int bfcount, 
												SynerEdge::uint64 blockcount, 
												 int blocksize);

/***************
 * Delete all virtual block files used for testing
 ***************/
	static void deleteTestBlockFiles();
	static void deleteTestVirtualBlockFiles();
};
