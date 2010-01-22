#include "BlockCacheStdUnitTest.hpp"
#include "VirtualDisk.hpp"
#include "Block.hpp"
#include "Testutils.hpp"
#include <stdlib.h>
#include <iostream>
using namespace std ;
using namespace SynerEdge ;

// CPPUNIT_TEST_SUITE_REGISTRATION(BlockCacheStdUnitTest) ;


void BlockCacheStdUnitTest::setUp()
{

	VirtualBlockFile *vbf1 = NULL;
	VirtualBlockFile *vbf2 = NULL;

	vbf1  = Testutils::makeTestVirtualBlockFile("vbf1", 2, 100, 1024);  // Make a virtual block file of 100 nodes each
	vbf2  = Testutils::makeTestVirtualBlockFile("vbf2", 2, 100, 1024);  // Make a virtual block file of 100 nodes each
	CPPUNIT_ASSERT(vbf1);
	CPPUNIT_ASSERT(vbf2);


	// VirtualBlockFileList *vbflistp = NULL;

	VirtualDisk::VirtualBlockFileList  *vbflistp = new VirtualDisk::VirtualBlockFileList;
	vbflistp->push_back(vbf1) ;
	vbflistp->push_back(vbf2) ;




	vd = new VirtualDisk("vd", 1024, (*vbflistp)) ;

	delete vbflistp;
	vd->open();
	// Most tests assume that the disk can be read/written. Allocate all the blocks

	for (int i = 0; i < 200; i++) {
		CPPUNIT_ASSERT(vd->getFreeBlock() != 0);
	}

	CPPUNIT_ASSERT(vd->getFreeBlock() == 0);   // disk had only 200 blocks.
	bcache = vd->getCache();


}

void BlockCacheStdUnitTest::tearDown()
{
	vd->close();
	delete vd;
	Testutils::deleteTestVirtualBlockFiles() ;
}


void BlockCacheStdUnitTest::testzeroblock()
{
	CPPUNIT_ASSERT(bcache->zeroBlock(1));
	CPPUNIT_ASSERT(bcache->zeroBlock(2));
	Block b;
	CPPUNIT_ASSERT(bcache->readBlock(1, b));
}

void BlockCacheStdUnitTest::vdisktestzeroblock()
{
	for (int i = 1; i < 200; i++) 
		CPPUNIT_ASSERT(vd->zeroBlock(i));
	Block b;
	for (int i = 1; i < 200; i++)
		CPPUNIT_ASSERT(vd->readBlock(i, b));
}


void BlockCacheStdUnitTest::testallocatefree()
{
	int count = vd->getNumFreeBlocks();
	CPPUNIT_ASSERT(count == 0);   // Blocks were allocated in setUp.
	for (int i = 1; i <= 200; i++) {
		vd->releaseBlock(i);
	}

	count = vd->getNumFreeBlocks();
	CPPUNIT_ASSERT(count == 200);
	for (int i = 1; i < 200; i++) {
		int blocknum = vd->getFreeBlock();
		CPPUNIT_ASSERT(vd->zeroBlock(blocknum));
		vd->releaseBlock(blocknum); // always returns true.
		Block b;
		CPPUNIT_ASSERT(vd->readBlock(blocknum, b) == false);
	}
}


void BlockCacheStdUnitTest::testwrites()
{
	char data[] = "Hello there";
	char buf[12];

	// all blocks were allocated in setup
	CPPUNIT_ASSERT(bcache->releaseBlock(2) == true);
	CPPUNIT_ASSERT(bcache->writeBlock(2, 0, 12, data) == false); // block not allocated
	int numbytesread = bcache->readBytes(2, 0, 12, buf) ;
	CPPUNIT_ASSERT(numbytesread == -1);    // block not allocated
	CPPUNIT_ASSERT(bcache->writeBlock(3, 0, 12, data)); // block allocated in setup
	numbytesread = bcache->readBytes(3, 0, 12, buf) ;
	CPPUNIT_ASSERT(numbytesread == 12);
	CPPUNIT_ASSERT(memcmp(data, buf, 12) == 0);
}

