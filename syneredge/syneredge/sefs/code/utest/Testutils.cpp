#include "BlockFile.hpp"
#include "VirtualBlockFile.hpp"
#include "Testutils.hpp"

/*******************
 * Initialize a virtual blockfile using tmp files, consisting of
 * bfcount blockfiles, each with blockcount blocks, with each 
 * block of blocksize bytes
 *******************/

using namespace SynerEdge;



static char *nameprefix = "/tmp/vbftestfiles";      // prefix for all test file names
static int namesuffix = 1;

SynerEdge::BlockFile*
Testutils::makeTestBlockFile(SynerEdge::uint64 blockcount, int blocksize)
{
	char blockfilename[32];
	memset(blockfilename, 0, sizeof(blockfilename));
	int i = 0;
	for (i = 0; i < 1000;  i++)  {  // Find an availble filename
		sprintf(blockfilename, "%s%d", nameprefix, namesuffix);
		if (SynerEdge::BlockFile::exists(blockfilename)) {
			namesuffix++;
		} else {
			break;
		}
	}

	if (i == 1000) return NULL;
	
	SynerEdge::BlockFile *bf = new SynerEdge::BlockFile(blockfilename) ;
	if (!bf->create(1, blockcount, blocksize)) {
		delete bf; 
		return NULL;
	}
	bf->close();
	namesuffix++;
	return bf;
}


void Testutils::deleteTestBlockFiles()
{
	char command[32];
	sprintf(command, "rm -f %s*", nameprefix);
	cout << "deleteTestBlockFiles executing command: " << command << endl;
	system(command); 
}




VirtualBlockFile *Testutils::makeTestVirtualBlockFile(char *name, int bfcount, uint64 blockcount, int blocksize)
{
	// Allocate an array of BlockFile pointers

	BlockFile *bf;
	VirtualBlockFile::BlockFileList bflist;

	for (int i = 0; i < bfcount; i++) {
		bf = makeTestBlockFile(blockcount, blocksize);
		if (bf == NULL) {
			return NULL;
		}
		bflist.push_back(bf);
	}

	VirtualBlockFile *vbf = new VirtualBlockFile(name, bflist, blocksize);
	

	return vbf;
}


/***************
 * Delete all virtual block files used for testing
 ***************/

void Testutils::deleteTestVirtualBlockFiles()
{
	deleteTestBlockFiles();
}

