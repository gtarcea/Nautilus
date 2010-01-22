#include "VirtualBlockFileUnitTest.hpp"
#include "VirtualBlockFile.hpp"
#include "BlockFile.hpp"
#include "Block.hpp"
#include <stdlib.h>
#include <iostream>
#include "Testutils.hpp"
using namespace std ;
using namespace SynerEdge ;
using SynerEdge::VirtualBlockFile ;

// CPPUNIT_TEST_SUITE_REGISTRATION(VirtualBlockFileUnitTest) ;

static BlockFile *bf1 = NULL;
static BlockFile *bf2 = NULL;
static BlockFile *bf3 = NULL;


void
VirtualBlockFileUnitTest::setUp()
{
//	cout << "VirtualBlockFileUnitTest::setUp()" << endl ;
	// Start from a clean system


	system("rm -f /tmp/bf1*");
	system("rm -f /tmp/bf2*");
	system("rm -f /tmp/bf3*");
	system("rm -f /tmp/mybfe1*") ;
	system("rm -f /tmp/myvbf*") ;


	bf1 = new BlockFile("/tmp/bf1") ;
	bf2 = new BlockFile("/tmp/bf2") ;
	bf3 = new BlockFile("/tmp/bf3") ;

	
	CPPUNIT_ASSERT(bf1->create(1, 100, 1024)) ;
	bf1->close() ;

	CPPUNIT_ASSERT(bf2->create(1, 100, 1024)) ;
	bf2->close() ;
	CPPUNIT_ASSERT(bf3->create(1, 100, 1024));
	bf3->close() ;

	VirtualBlockFile::BlockFileList bflist ;
	bflist.push_back(bf1) ;
	bflist.push_back(bf2) ;
	bflist.push_back(bf3) ;

	// vbf = new VirtualBlockFile("vbf", bflist, 1024) ;
	vbf = Testutils::makeTestVirtualBlockFile("vbf", 3, 100, 1024);

	// The rest of the code assumes that everything is allocated. Allocate all the blocks.

	CPPUNIT_ASSERT(vbf->open());
	for (int i = 0; i < 300; i++) {
		CPPUNIT_ASSERT(vbf->getFreeBlock() != 0);
	}
	CPPUNIT_ASSERT(vbf->getFreeBlock() == 0);
		
	
	//tb1 = new Block() ;
	//tb2 = new Block() ;
}

void
VirtualBlockFileUnitTest::tearDown()
{
//	cout << "VirtualBlockFileUnitTest::tearDown()" << endl ;
	if (vbf) {delete vbf ; vbf = NULL;}

	if (bf1) {delete bf1; bf1 = NULL;}
	if (bf2) {delete bf2; bf2 = NULL;}
	if (bf3) {delete bf3; bf3 = NULL;}
}

void
VirtualBlockFileUnitTest::testConstructor()
{
//	cout << "VirtualBlockFileUnitTest::testConstructor()" << endl ;

	VirtualBlockFile::BlockFileList bflist ;
	BlockFile bf("/tmp/mybfe1");
	bflist.push_back(&bf) ;
	VirtualBlockFile myvbf("myvbf", bflist, 1024) ;
	VirtualBlockFile *newvbf = new VirtualBlockFile("newvbf", bflist, 1024) ;
	CPPUNIT_ASSERT(newvbf != NULL);
	if (newvbf) delete newvbf;


}

void
VirtualBlockFileUnitTest::testCreateFile()
{
//	cout << "VirtualBlockFileUnitTest::testCreateFile()" << endl ;
	CPPUNIT_ASSERT(vbf->open() == true) ;
	// Test that 300 blocks were written
	Block tb ;
	// tests broken up to see behavior around disk boundaries
	for (int i = 1 ; i <= 99 ; i++) {
		CPPUNIT_ASSERT(vbf->readBlock(i, tb) == true) ;
		CPPUNIT_ASSERT(tb.getBlockNum() == i) ;
	}
	CPPUNIT_ASSERT(vbf->readBlock(100, tb) == true) ;
	CPPUNIT_ASSERT(tb.getBlockNum() == 100) ;

	for (int i = 101 ; i <= 200 ; i++) {
		CPPUNIT_ASSERT(vbf->readBlock(i, tb) == true) ;
		CPPUNIT_ASSERT(tb.getBlockNum() == i) ;
	}
	for (int i = 201 ; i <= 300 ; i++) {
		CPPUNIT_ASSERT(vbf->readBlock(i, tb) == true) ;
		CPPUNIT_ASSERT(tb.getBlockNum() == i) ;
	}
	// out-of-bound check
	CPPUNIT_ASSERT(vbf->readBlock(301, tb) == false);
	CPPUNIT_ASSERT(vbf->readBlock(0, tb) == false);
	// CPPUNIT_ASSERT(vbf->readBlock(-1, tb) == false); // Test not needed. unsigned val is passed
}

void VirtualBlockFileUnitTest::testOpenFile()
{
	//cout << "VirtualBlockFileUnitTest::testOpenFile()" << endl ;
	// setUp has already run
	CPPUNIT_ASSERT(vbf->open() == true) ;
	CPPUNIT_ASSERT(vbf->getNumBlocks() == 300) ;
	CPPUNIT_ASSERT(vbf->getNumFreeBlocks() == 0) ;
	CPPUNIT_ASSERT(vbf->getBlockSize() == 1024) ;
	//CPPUNIT_ASSERT(vbf->getStartingBlockNum() == 1) ;

	// Test open of non-existent file
	VirtualBlockFile::BlockFileList bflist ;
	BlockFile bf("/tmp/doesnotexist");
	bflist.push_back(&bf) ;
	VirtualBlockFile nvbf("myvbf2", bflist, 1024) ;
	CPPUNIT_ASSERT(nvbf.open() == false) ;
}

void VirtualBlockFileUnitTest::testReadWriteBlocks()
{
	char *buf = "DEF456123" ;
	//cout << "VirtualBlockFileUnitTest::testReadWriteBlocks()" << endl ;
	CPPUNIT_ASSERT(vbf->open() == true) ;
	CPPUNIT_ASSERT(vbf->writeBlock(1, 0, 10, buf) == true) ;
	Block b ;
	CPPUNIT_ASSERT(vbf->readBlock(1, b) == true) ;
	CPPUNIT_ASSERT(b.getNumBytes() == 10) ;
	CPPUNIT_ASSERT(vbf->writeBlock(2, 0, 10, buf) == true) ;
	CPPUNIT_ASSERT(vbf->readBlock(2, b) == true) ;
	CPPUNIT_ASSERT(b.getNumBytes() == 10) ;
	CPPUNIT_ASSERT(b.getBlockNum() == 2) ;
	b.setBlockNum(3) ;
	CPPUNIT_ASSERT(vbf->writeBlock(b) == true) ;
	CPPUNIT_ASSERT(vbf->readBlock(3, b) == true) ;
	CPPUNIT_ASSERT(b.getNumBytes() == 10) ;
	CPPUNIT_ASSERT(b.getBlockNum() == 3) ;
}

void
VirtualBlockFileUnitTest::testZeroAndFreeBlocks()
{
	Block b ;
	//cout << "VirtualBlockFileUnitTest::testZeroAndFreeBlocks()" << endl ;

	char *buf = "DEF456123" ;
	CPPUNIT_ASSERT(vbf->open() == true) ;
	CPPUNIT_ASSERT(vbf->writeBlock(1, 0, 10, buf) == true) ;
	CPPUNIT_ASSERT(vbf->readBlock(1, b) == true) ;
	CPPUNIT_ASSERT(b.getNumBytes() != 0) ;
	CPPUNIT_ASSERT(vbf->zeroBlock(1) == true) ;
	CPPUNIT_ASSERT(vbf->readBlock(1, b) == true) ;
	CPPUNIT_ASSERT(b.getNumBytes() == 0) ;
	CPPUNIT_ASSERT(vbf->releaseBlock(1) == true) ;
	CPPUNIT_ASSERT(vbf->readBlock(1, b) == false) ;
}


void
VirtualBlockFileUnitTest::testFreeBlockListMethods()
{
	CPPUNIT_ASSERT(vbf->open() == true) ;

	// Everything was allocated previously in setup.

	CPPUNIT_ASSERT(vbf->releaseBlock(0) == false);
	CPPUNIT_ASSERT(vbf->releaseBlock(301) == false);

	for (int i = 1; i <= 300; i++) {
		CPPUNIT_ASSERT(vbf->releaseBlock(i) == true);
	}

	int64 f = vbf->getNumFreeBlocks() ;
	CPPUNIT_ASSERT(vbf->getNumFreeBlocks() == 300) ;
	int64 bn = vbf->getFreeBlock() ;
	CPPUNIT_ASSERT(vbf->getNumFreeBlocks() == 299) ;
	CPPUNIT_ASSERT(vbf->releaseBlock(bn) == true) ;
	CPPUNIT_ASSERT(vbf->getNumFreeBlocks() == 300) ;
	bn = vbf->getFreeBlock() ;
	CPPUNIT_ASSERT(vbf->getNumFreeBlocks() == 299) ;
	vbf->close() ;
	CPPUNIT_ASSERT(vbf->open() == true) ;
	CPPUNIT_ASSERT(vbf->getNumFreeBlocks() == 299) ;
	CPPUNIT_ASSERT(vbf->flushFreeBlockList() == true) ;

	system("rm -f /tmp/bf*") ;
}


