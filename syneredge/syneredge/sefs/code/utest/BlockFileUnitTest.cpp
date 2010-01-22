#include "BlockFileUnitTest.hpp"
#include "BlockFile.hpp"
#include "Block.hpp"
#include "BlockUnitTest.hpp"
#include <stdlib.h>
#include <iostream>

using namespace std ;
using namespace SynerEdge ;

// CPPUNIT_TEST_SUITE_REGISTRATION(BlockFileUnitTest) ;

void
BlockFileUnitTest::setUp()
{
//	cout << "BlockFileUnitTest::setUp()" << endl ;
	system("rm -f /tmp/newblockfile*") ; // Important. Otherwise state is left between tests.
	bf = new BlockFile("/tmp/newblockfile") ;
	assert(bf->create(1, 10, 1024));

	//tb1 = new Block() ;
	//tb2 = new Block() ;
}

void
BlockFileUnitTest::tearDown()
{
//	cout << "BlockFileUnitTest::tearDown()" << endl ;
	delete bf ;
//	delete tb1 ;
//	delete tb2 ;
}

void
BlockFileUnitTest::testConstructor()
{
	//cout << "BlockFileUnitTest::testConstructor()" << endl ;
	BlockFile nbf("/tmp/newblockfile") ;

	CPPUNIT_ASSERT(nbf.getNumBlocks() == 0) ;
	CPPUNIT_ASSERT(nbf.getStartingBlockNum() == 0) ;
	CPPUNIT_ASSERT(nbf.getBlockSize() == 1024) ;
	//cout << "Leaving BlockFileUnitTest::testConstructor" << endl ;
}

void
BlockFileUnitTest::testCreateFile()
{
	//cout << "BlockFileUnitTest::testCreateFile()" << endl ;
	system("rm -f /tmp/newblockfile*") ;
	BlockFile nbf("/tmp/newblockfile") ;
	CPPUNIT_ASSERT(nbf.create(1, 10, 1024) == true) ;
	//nbf.printStuff() ;
	// Test that 10 blocks were written
	Block tb ;
	for (int i = 1 ; i < 12 ; i++) {
		CPPUNIT_ASSERT(nbf.readBlock(i, tb) == false); // blocks are not allocated.
		CPPUNIT_ASSERT(nbf.zeroBlock(i) == false); // Can't zero an unallocated block
		CPPUNIT_ASSERT(nbf.writeBlock(i, 0, 2, "a") == false); // Can't write an unallocated block
	}

	int alloclist[10];
	for (int i = 0 ; i < 10 ; i++) {
		alloclist[i] = nbf.getFreeBlock();
		CPPUNIT_ASSERT(nbf.readBlock(alloclist[i], tb)); // should work. block is Allocated
		CPPUNIT_ASSERT(nbf.zeroBlock(alloclist[i]) == true); 
		CPPUNIT_ASSERT(nbf.writeBlock(alloclist[i], 0, 2, "a") == true);
		// cout << "tb.getBlockNum() = " << tb.getBlockNum() << " and allocated block = " << alloclist[i] << endl;
		CPPUNIT_ASSERT(tb.getBlockNum() == alloclist[i]) ;
	}
	// Check the out-of-bound case
	int extraalloc = nbf.getFreeBlock();
	CPPUNIT_ASSERT(extraalloc == 0);
	
	// Do more out-of-bound checks. Block numbers start at 1. 0 is invalid
	CPPUNIT_ASSERT(nbf.readBlock(0, tb) == false); 
	CPPUNIT_ASSERT(nbf.zeroBlock(0) == false); 
	CPPUNIT_ASSERT(nbf.writeBlock(0, 0, 2, "a") == false); 


	

	// Free up all the blocks
	for (int i = 0; i < 10; i++) {
		CPPUNIT_ASSERT(nbf.releaseBlock(alloclist[i]));
	}
	system("rm -f /tmp/newblockfile*") ;

}

void BlockFileUnitTest::testOpenFile()
{
	//cout << "BlockFileUnitTest::testOpenFile()" << endl ;
	// testCreateFile() should have already run.
	
	CPPUNIT_ASSERT(bf->open() == true) ;
	CPPUNIT_ASSERT(bf->getNumBlocks() == 10) ;
	// cout << "BlackFileUnitTest::testOpenFile: number of free blocks = " << bf->getNumFreeBlocks() << endl;
	CPPUNIT_ASSERT(bf->getNumFreeBlocks() == 10) ;
	CPPUNIT_ASSERT(bf->getBlockSize() == 1024) ;
	CPPUNIT_ASSERT(bf->getStartingBlockNum() == 1) ;

	// Test open of non-existent file
	BlockFile nbf("/tmp/doesnotexist") ;
	CPPUNIT_ASSERT(nbf.open() == false) ;

	system("rm -f /tmp/newblockfile*") ;
}

void BlockFileUnitTest::testReadWriteBlocks()
{
	char *buf = "DEF456123" ;
	//cout << "BlockFileUnitTest::testReadWriteBlocks()" << endl ;
	CPPUNIT_ASSERT(bf->open() == true) ;

	int first = bf->getFreeBlock();
	int second = bf->getFreeBlock();
	int third  = bf->getFreeBlock();
	CPPUNIT_ASSERT(bf->writeBlock(first, 0, 10, buf) == true) ;
	Block b ;
	CPPUNIT_ASSERT(bf->readBlock(first, b) == true) ;
	CPPUNIT_ASSERT(b.getNumBytes() == 10) ;
	CPPUNIT_ASSERT(bf->writeBlock(second, 0, 10, buf) == true) ;
	CPPUNIT_ASSERT(bf->readBlock(second, b) == true) ;
	CPPUNIT_ASSERT(b.getNumBytes() == 10) ;
	CPPUNIT_ASSERT(b.getBlockNum() == 2) ;
	b.setBlockNum(third) ;
	CPPUNIT_ASSERT(bf->writeBlock(b) == true) ;
	CPPUNIT_ASSERT(bf->readBlock(third, b) == true) ;
	CPPUNIT_ASSERT(b.getNumBytes() == 10) ;
	CPPUNIT_ASSERT(b.getBlockNum() == 3) ;

	// Do some boundary checks on read/writes
	char data[4096];
	BlockUnitTest::init_buffer(data, 4096);
	CPPUNIT_ASSERT(bf->writeBlock(first, 0,  bf->getBlockSize(), data) == true);
	CPPUNIT_ASSERT(bf->writeBlock(first, 0,  bf->getBlockSize()+1, data) == false);
	CPPUNIT_ASSERT(bf->writeBlock(first, 1023, 1, data) == true);
	CPPUNIT_ASSERT(bf->writeBlock(first, 1024, 1, data) == false);
	CPPUNIT_ASSERT(bf->writeBlock(first, 1023, -1, data) == false);
	CPPUNIT_ASSERT(bf->writeBlock(first, -1, 10, data) == false);


	CPPUNIT_ASSERT(bf->releaseBlock(first));
	CPPUNIT_ASSERT(bf->releaseBlock(second));
	CPPUNIT_ASSERT(bf->releaseBlock(third));

	// Boundary check
	CPPUNIT_ASSERT(bf->releaseBlock(third+1) == false);  // block not allocated previously.

	system("rm -f /tmp/newblockfile*") ;
}

void
BlockFileUnitTest::testZeroAndFreeBlocks()
{
	Block b ;
	//cout << "BlockFileUnitTest::testZeroAndFreeBlocks()" << endl ;

	CPPUNIT_ASSERT(bf->open() == true) ;

	int first = bf->getFreeBlock();

	CPPUNIT_ASSERT(bf->readBlock(first, b) == true) ;
	// The assertion below has been changed.
	CPPUNIT_ASSERT(b.getNumBytes() == 0) ; 
	CPPUNIT_ASSERT(bf->zeroBlock(first) == true) ;
	CPPUNIT_ASSERT(bf->readBlock(first, b) == true) ;
	CPPUNIT_ASSERT(b.getNumBytes() == 0) ;
	// CPPUNIT_ASSERT(b.isFree() == false) ;

	CPPUNIT_ASSERT(bf->releaseBlock(first) == true) ;
	// CPPUNIT_ASSERT(bf->readBlock(first, b) == false) ;  uncomment later.
	system("rm -f /tmp/newblockfile*") ;
}

void
BlockFileUnitTest::testFreeBlockListMethods()
{

	//cout << "BlockFileUnitTest::testFreeBlockListMethods()" << endl ;
	CPPUNIT_ASSERT(bf->open() == true) ;

	CPPUNIT_ASSERT(bf->getNumFreeBlocks() == 10) ;
	int64 bn = bf->getFreeBlock() ;
	CPPUNIT_ASSERT(bf->getNumFreeBlocks() == 9) ;
	CPPUNIT_ASSERT(bf->releaseBlock(bn) == true) ;
	CPPUNIT_ASSERT(bf->getNumFreeBlocks() == 10) ;
	bn = bf->getFreeBlock() ;
	CPPUNIT_ASSERT(bf->getNumFreeBlocks() == 9) ;
	bf->close() ;
	CPPUNIT_ASSERT(bf->open() == true) ;
	CPPUNIT_ASSERT(bf->getNumFreeBlocks() == 9) ;
	CPPUNIT_ASSERT(bf->flushFreeBlockList() == true) ;

	system("rm -f /tmp/newblockfile*") ;
}

