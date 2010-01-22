#include "VirtualDiskUnitTest.hpp"
#include "VirtualBlockFile.hpp"
#include "Block.hpp"
#include <stdlib.h>
#include <iostream>
#include "Testutils.hpp"
using namespace std ;
using namespace SynerEdge ;

void
VirtualDiskUnitTest::setUp()
{
	cout << "VirtualDiskUnitTest::setUp()" << endl ;


	// deleteTestVirtualBlockFiles();
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
	CPPUNIT_ASSERT(vd->getNumFreeBlocks() == 200) ;
	// Most tests assume that the disk can be read/written. Allocate all the blocks

	for (int i = 0; i < 200; i++) {
		CPPUNIT_ASSERT(vd->getFreeBlock() != 0);
	}

	CPPUNIT_ASSERT(vd->getFreeBlock() == 0);   // disk had only 200 blocks.

	// cout << "VirtualDiskUnitTest end setup" << endl;

	//tb1 = new Block() ;
	//tb2 = new Block() ;
}

void
VirtualDiskUnitTest::tearDown()
{
	cout << "VirtualDiskUnitTest::tearDown()" << endl ;
	// delete vd ;
	Testutils::deleteTestVirtualBlockFiles() ;
}

void
VirtualDiskUnitTest::testConstructor()
{
	cout << "VirtualDiskUnitTest::testConstructor()" << endl ;
     // Make a virtual block file of 5 blocks with 0 blocks each
	VirtualBlockFile *tmpvbf1 = Testutils::makeTestVirtualBlockFile("tmpvbf1", 5, 
											   0, 1024);  

	CPPUNIT_ASSERT(tmpvbf1 == NULL);
	// Make 5 block files, each with 10,000 blocks of 1K each, 
	tmpvbf1 = Testutils::makeTestVirtualBlockFile("tmpvbf2",  5, 10000, 1024);  // Make a virtual block file of 100 nodes eac
	
	CPPUNIT_ASSERT(tmpvbf1 != NULL);
	delete tmpvbf1;
}

void
VirtualDiskUnitTest::testCreateFile()
{
	cout << "VirtualDiskUnitTest::testCreateFile()" << endl ;
	// Test that 300 blocks were written
	Block tb ;
	for (int i = 1 ; i < 201 ; i++) {
		CPPUNIT_ASSERT(vd->readBlock(i, tb) == true) ;
		CPPUNIT_ASSERT(tb.getBlockNum() == i) ;
	}
	// Do boundary checks
	CPPUNIT_ASSERT(vd->readBlock(201, tb) == false) ;
	CPPUNIT_ASSERT(vd->readBlock(0, tb) == false) ;
}

void VirtualDiskUnitTest::testOpenFile()
{
	
	cout << "VirtualDiskUnitTest::testOpenFile()" << endl ;
	// setUp has already run
	CPPUNIT_ASSERT(vd->open() == true) ;
	//int64 numblocks = vd->getNumBlocks() ;
	//cout << "Numblocks = " << numblocks << endl ;
	CPPUNIT_ASSERT(vd->getNumBlocks() == 200) ;

	// Disk was completely allocated in setup
	CPPUNIT_ASSERT(vd->getNumFreeBlocks() == 0) ;


	CPPUNIT_ASSERT(vd->getBlockSize() == 1024) ;
	//CPPUNIT_ASSERT(vbf->getStartingBlockNum() == 1) ;

	// Test open of non-existent file
	VirtualBlockFile::BlockFileList bflist ;
	BlockFile *bf = new BlockFile("/tmp/doesnotexist");
	bflist.push_back(bf) ;
	VirtualBlockFile *nvbf = new VirtualBlockFile("myvbf2", bflist, 1024) ;
	CPPUNIT_ASSERT(nvbf->open() == false) ;
	VirtualDisk::VirtualBlockFileList vbflist ;
	vbflist.push_back(nvbf) ;
	VirtualDisk newvd("newvd", 1024, vbflist) ;
	CPPUNIT_ASSERT(newvd.open() == false) ;
	delete bf;
	delete nvbf;
}

void VirtualDiskUnitTest::testReadWriteBlocks()
{
	
	char *buf = "DEF456123" ;
	//cout << "VirtualDiskUnitTest::testReadWriteBlocks()" << endl ;
	CPPUNIT_ASSERT(vd->open() == true) ;
	CPPUNIT_ASSERT(vd->disk_writeBlock(1, 0, 10, buf) == true) ;
	Block b ;
	CPPUNIT_ASSERT(vd->disk_readBlock(1, b) == true) ;
	CPPUNIT_ASSERT(b.getNumBytes() == 10) ;
	CPPUNIT_ASSERT(vd->disk_writeBlock(2, 0, 10, buf) == true) ;
	CPPUNIT_ASSERT(vd->disk_readBlock(2, b) == true) ;
	CPPUNIT_ASSERT(b.getNumBytes() == 10) ;
	CPPUNIT_ASSERT(b.getBlockNum() == 2) ;
	b.setBlockNum(3) ;
	CPPUNIT_ASSERT(vd->disk_writeBlock(b) == true) ;
	CPPUNIT_ASSERT(vd->disk_readBlock(3, b) == true) ;
	CPPUNIT_ASSERT(b.getNumBytes() == 10) ;
	CPPUNIT_ASSERT(b.getBlockNum() == 3) ;
	
}

void
VirtualDiskUnitTest::testZeroAndFreeBlocks()
{
	
	Block b ;
	//cout << "VirtualDiskUnitTest::testZeroAndFreeBlocks()" << endl ;
	char *buf = "DEF456123" ;

	CPPUNIT_ASSERT(vd->open() == true) ;
	CPPUNIT_ASSERT(vd->disk_writeBlock(1, 0, 10, buf) == true) ;
	CPPUNIT_ASSERT(vd->disk_readBlock(1, b) == true) ;
	CPPUNIT_ASSERT(b.getNumBytes() != 0) ;
	CPPUNIT_ASSERT(vd->disk_zeroBlock(1) == true) ;
	CPPUNIT_ASSERT(vd->disk_readBlock(1, b) == true) ;
	CPPUNIT_ASSERT(b.getNumBytes() == 0) ;
	// CPPUNIT_ASSERT(b.isFree() == false) ;
	CPPUNIT_ASSERT(vd->disk_releaseBlock(1) == true) ;
	CPPUNIT_ASSERT(vd->disk_readBlock(1, b) == false) ;
	// CPPUNIT_ASSERT(b.isFree() == true) ;
}

void
VirtualDiskUnitTest::testFreeBlockListMethods()
{
	
	CPPUNIT_ASSERT(vd->open() == true) ;
	int64 f = vd->getNumFreeBlocks() ;
	//cout << "number of free blocks =  " << f << endl;
	CPPUNIT_ASSERT(vd->getNumFreeBlocks() == 0) ;


	// Everything was allocated previously in setup.

	CPPUNIT_ASSERT(vd->releaseBlock(0) == false);
	CPPUNIT_ASSERT(vd->releaseBlock(201) == false);

	for (int i = 1; i <= 200; i++) {
		CPPUNIT_ASSERT(vd->releaseBlock(i) == true);
	}

	int64 bn = vd->getFreeBlock() ;
	CPPUNIT_ASSERT(vd->getNumFreeBlocks() == 199) ;
	CPPUNIT_ASSERT(vd->releaseBlock(bn) == true) ;
	CPPUNIT_ASSERT(vd->getNumFreeBlocks() == 200) ;
	bn = vd->getFreeBlock() ;
	CPPUNIT_ASSERT(vd->getNumFreeBlocks() == 199) ;
	vd->close() ;
	CPPUNIT_ASSERT(vd->open() == true) ;
	CPPUNIT_ASSERT(vd->getNumFreeBlocks() == 199) ;
	CPPUNIT_ASSERT(vd->flushFreeBlockList() == true) ;

	system("rm -f /tmp/bf*") ;
}

