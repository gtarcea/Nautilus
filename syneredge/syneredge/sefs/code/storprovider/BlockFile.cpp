#include <iostream>
#include "BlockFile.hpp"
#include "Block.hpp"
#include "SynerEdge.hpp"
#include "seerror.hpp"
#include "PT.hpp"
#include "boost/filesystem/operations.hpp"
/*#include "blockcache.h"*/
#include <string>
#include <cstdlib>

using namespace std ;

namespace SynerEdge {

//static const headersize = sizeof(BlockFile::numblocks) ;

BlockFile::BlockFile(string filename) : numblocks(0), numfreeblocks(0),
		startingblocknum(0), blocksize(1024), filename(filename)
{
	freeblockmap = NULL ;
}


BlockFile::~BlockFile()
{
	//cout << "~BlockFile()" << endl ;
	close();
}

bool
BlockFile::close()
{
	//cout << "close()" << endl ;
	boost::mutex::scoped_lock lk(_mtx, true) ;
	if (file.is_open()) {
		file.close() ;
		flushFreeBlockList(false) ;
	}
	if (freeblockmapfile.is_open()) freeblockmapfile.close() ;
	if (freeblockmap) delete freeblockmap ;
	freeblockmap = NULL ;
	return true ;
}

bool
BlockFile::exists(string blockfilename)
{
	if (boost::filesystem::exists(blockfilename)) {
		return true ;
	}

	return false ;
}

bool
BlockFile::open()
{

	fstream ifile ;

//	cout << "open()" << endl ;
	boost::mutex::scoped_lock lk(_mtx, true) ;
	if (file.is_open()) {
		return true ;
	}

	string freeblockmapname = filename + ".freeblockmap" ;
	if (exists(filename) && exists(freeblockmapname)) {
		ifile.open(freeblockmapname.c_str(), ios::in) ;
		file.open(filename.c_str(), ios::in | ios::out | ios::binary) ;
		if (! file.is_open() || ! ifile.is_open()) {
			file.close();
			ifile.close();
			return false ;
		}
	} else {
		return false ;
	}



	// Error check should be done.

	//
	// Make sure we are at beginning of file
	//
	file.seekg(0, ios::beg) ;

	//
	// Read in header and fill in object structures.
	//
	file.read(reinterpret_cast<char *>(&numfreeblocks), sizeof(uint64)) ;
	file.read(reinterpret_cast<char *>(&numblocks), sizeof(uint64)) ;
	file.read(reinterpret_cast<char *>(&startingblocknum), sizeof(uint64)) ;
	file.read(reinterpret_cast<char *>(&blocksize), sizeof(int)) ;

//	cout << "numblocks = " << numblocks << endl ;
//	cout << "numfreeblocks = " << numfreeblocks << endl ;
//	cout << "startingblocknum = " << startingblocknum << endl ;
//	cout << "blocksize = " << blocksize << endl ;

	//
	// Open freeblockmap file, read in, and then reopen for write.
	//

	freeblockmap = new FreeBlockMap() ;
	ifile >> (*freeblockmap) ;
	ifile.close() ;
	freeblockmapfile.open(freeblockmapname.c_str(), ios::out) ;

	// Write current bitmap to file.
	flushFreeBlockList(false) ;

	return true ;

}

bool
BlockFile::create(uint64 startingblocknum, uint64 numblocks, int blocksize)
{
//	cout << "create()" << endl ;
	boost::mutex::scoped_lock lk(_mtx, true) ;
	
	if (numblocks < 1) {
//		cout << "numblocks < 1" << endl ;
		return false ;
	}

	if (startingblocknum < 0) {
//		cout << "startingblocknum < 0" << endl ;
		return false ;
	}

	if (file.is_open()) {
//		cout << "file already open" << endl ;
		return false ;
	}

	if (exists(filename)) {
//		cout << "file exists" << endl ;
		return false ;
	}

	if (! Block::validBlockSize(blocksize)) {
//		cout << "invalid blocksize " << blocksize << endl ;
		return false ;
	}

	file.open(filename.c_str(), ios::out | ios::binary) ;
	if (! file.is_open()) {
//		cout << "couldn't create file" << endl ;
		return false ;
	}

	//
	// Setup internal data structures
	BlockFile::numblocks = numblocks ;
	BlockFile::numfreeblocks = numblocks ;
	BlockFile::startingblocknum = startingblocknum ;
	BlockFile::blocksize = blocksize ;

	//
	// Write the header, and then write numblocks nullblocks.
	//
//	cout << "Writing header" << endl ;
	// Header
	//file << numblocks ;
	//file << numfreeblocks ;
	//file << startingblocknum ;
	//file << blocksize
	// We write numfreeblocks first to make seeking to and updates
	// an easy calculation.
	file.write(reinterpret_cast<char *>(&numfreeblocks), sizeof(uint64)) ;
	file.write(reinterpret_cast<char *>(&numblocks), sizeof(uint64)) ;
	file.write(reinterpret_cast<char *>(&startingblocknum), sizeof(uint64)) ;
	file.write(reinterpret_cast<char *>(&blocksize), sizeof(int)) ;

	// Fill in numblocks nullblocks 
//	cout << "in Blockfile blocksize = " << blocksize << endl ;
	Block nullblock(startingblocknum, blocksize) ;
	uint64 bn = startingblocknum ;

//	cout << "Sizeof block = " << nullblock.getObjectSize(blocksize) << endl ;

	for(int i = 0 ; i < numblocks ; i++) {
		nullblock.setBlockNum(bn) ;
//		cout << "Blocknum is " << nullblock.getBlockNum() << endl ;
		file << nullblock ;
		bn++ ;
	}

	file.close() ;
	file.open(filename.c_str(), ios::out | ios::in | ios::binary) ;

	if (! file.is_open()) {
//		cout << "couldn't reopen file" << endl ;
		return false ;
	}

	string freeblockmapname = filename + ".freeblockmap" ;
	freeblockmapfile.open(freeblockmapname.c_str(), ios::out) ;
	assert(freeblockmap == NULL);
	freeblockmap = new FreeBlockMap(numblocks, startingblocknum) ;
	freeblockmapfile << (*freeblockmap) ;
	freeblockmapfile.flush() ;

//	cout << "before return Blockfile blocksize = " << blocksize << endl ;
	return true ;
}

bool
BlockFile::writeBlock(uint64 blocknum, int offset, int size, char *data)
{
	PT("writeBlock(blocknum)") ;
	int bsize = Block::getObjectSize(blocksize) ;
	int wrote ;



	if (! validateAccessAndCheckNotFree(blocknum))  {
		return false ;
	}

	if (offset < 0 || offset + size <= offset || offset + size > blocksize) {
		return false ;
	}

	// Seek to where the block is
	int seekto = headersize + (blocknum - startingblocknum) * bsize ;

	boost::mutex::scoped_lock lk(_mtx, true) ;

	int where = file.tellg() ;
	if (where != seekto) {
		PT("\tseekg()") ;
		file.seekg(seekto, ios::beg) ;
		PT("\tLeaving seekg()") ;
	}
//	cout << "Current position = " << file.tellg() << endl ;

	if (offset == 0 && size == blocksize) {
		// We can optimize access here. Since we are replacing all the
		// blocks data, we don't have to read it in.
		Block b(blocksize, data) ;

		b.setBlockNum(blocknum) ;
		// b.setIsFree(false) ;
		PT("\toptimized file << b") ;
		file << b ;
		PT("\tLeaving optimized file << b") ;
	} else {
		PT("\tunoptimized") ;
		Block b(blocksize) ;
		// We'll read the block in, since we have to update block fields.
		file >> b ;
	//	cout << "Blocknum = " << b.getBlockNum() << endl ;

		// Update fields.
		wrote = b.setBytes(offset, data, size) ;
	//	cout << "Tried to write " << size << " wrote " << wrote << endl ;
	//	cout << "b.getNumBytes() = " << b.getNumBytes() << endl ;

		// seek backwards one block, so we can write this block out.
		file.seekg(-bsize, ios::cur) ;
		file << b ;
		PT("\tLeaving unoptimized") ;
	}

	PT("Leaving writeBlock(blocknum)") ;
	return true ;
}

bool
BlockFile::writeBlock(Block &block)
{
	PT("BlockFile::writeBlock(Block &block)") ;
	int bsize = Block::getObjectSize(blocksize) ;
	int blocknum = block.getBlockNum() ;



	if (! validateAccessAndCheckNotFree(blocknum)) {
		return false ;
	}

	// Make sure the block is big enough
	if (block.getSize() != blocksize) {
		return false ;
	}

	boost::mutex::scoped_lock lk(_mtx, true) ;

	// Seek to where the block is
	int seekto = headersize + (blocknum - startingblocknum) * bsize ;
	int where = file.tellg() ;
	if (where != seekto) {
		file.seekg(seekto, ios::beg) ;
	}

	// write block
	file << block ;

	PT("Leaving BlockFile::writeBlock(Block &block)") ;
	return true ;
}

bool
BlockFile::readBlock(uint64 blocknum, Block &block)
{

	if (!validateAccessAndCheckNotFree(blocknum)) {
		return false;
	}

	// block.setSize(blocksize) ;

	int seekto = headersize + ((blocknum - startingblocknum) * block.getObjectSize(blocksize)) ;
	boost::mutex::scoped_lock lk(_mtx, true) ;

	file.seekg(seekto, ios::beg) ;
	file >> block ;
	return true ;
}

bool
BlockFile::zeroBlock(uint64 blocknum)
{
	int bsize = Block::getObjectSize(blocksize) ;
	Block b(blocksize) ;

	if (!validateAccessAndCheckNotFree(blocknum)) {
		return false;
	}
	b.setNumBytes(0) ;
	b.setBlockNum(blocknum);
	return writeBlock(b);
}


bool
BlockFile::validateAccessAndCheckNotFree(uint64 blocknum)
{
	/* This is the correct code. But it breaks the tests. */
	if (( ! validateAccess(blocknum))  || freeblockmap->isFree(blocknum) ) {
		return false ;
	}
	else return true;
	
}


/*
* bool
* BlockFile::freeBlock(uint64 blocknum)
* {
* 	int bsize = Block::getObjectSize(blocksize) ;
* 	Block b(blocksize) ;
* 
* 	if(! readBlock(blocknum, b)) {
* 		return false ;
* 	}
* 
* 	if (b.isFree()) {
* 		// Block is already has 0 length for data, but is allocated
* 		return true ;
* 	} else {
* 		b.setNumBytes(0) ;
* 		b.setIsFree(true) ;
* 		boost::mutex::scoped_lock lk(_mtx, true) ;
* 		file.seekg(-bsize, ios::cur) ;
* 		file << b ;
* 	}
* 
* 	return true ;
* }
*/

uint64
BlockFile::getNumBlocks()
{
	return numblocks ;
}

uint64
BlockFile::getStartingBlockNum() const
{
	return startingblocknum ;
}

uint64
BlockFile::getNumFreeBlocks()
{
	// File should be open for this to be called.
	assert(freeblockmap != NULL);
	return freeblockmap->getFreeBlockCount() ;
}

int
BlockFile::getBlockSize() const
{
	return blocksize ;
}

bool
BlockFile::validateAccess(uint64 blocknum)
{
	if (! file.is_open()) {
		//cout << "File isn't open" << endl ;
		return false ;
	}

	if (blocknum < startingblocknum) {
		//cout << "blocknum < startingblocknum " << blocknum << "<" << startingblocknum << endl ;
		return false ;
	}

	if (blocknum >= startingblocknum + numblocks) {
		//cout << "blocknum > startingblocknum + numblocks" << blocknum << ">" << startingblocknum << "+" << numblocks << endl ;
		return false ;
	}

	return true ;
}

uint64
BlockFile::getFreeBlock()
{
	assert(freeblockmap);
	boost::mutex::scoped_lock lk(_mtx, true) ;
	PT("BlockFile::getFreeBlock()") ;
	uint64 freeblock = freeblockmap->allocateBlock() ;
	PT("Leaving BlockFile::getFreeBlock()") ;
	return freeblock ;
}

string
BlockFile::getFilename() const
{
	return filename ;
}

bool
BlockFile::releaseBlock(uint64 blocknum)
{
	assert(freeblockmap);
	return freeblockmap->freeBlock(blocknum) ;
}

bool
BlockFile::flushFreeBlockList()
{
	return flushFreeBlockList(true) ;
}

bool
BlockFile::flushFreeBlockList(bool lock)
{
	assert(freeblockmap);
	if (lock) {
		boost::mutex::scoped_lock lk(_mtx, true) ;
		freeblockmapfile.seekp(0, ios::beg) ;
		freeblockmapfile << (*freeblockmap) ;
		freeblockmapfile.flush() ;
	} else {
		freeblockmapfile.seekp(0, ios::beg) ;
		freeblockmapfile << (*freeblockmap) ;
		freeblockmapfile.flush() ;
	}

	return true ;
}

/*
void
BlockFile::printStuff()
{
	cout << "======= PRINT BLOCK DATA ========" << endl ;
	cout << "numblocks = " << numblocks << endl ;
	cout << "numfreeblocks = " << numfreeblocks << endl ;
	cout << "startingblocknum = " << startingblocknum << endl ;
	cout << "blocksize = " << blocksize << endl ;
	cout << "filename = " << filename << endl ;
	cout << "======= END  ========" << endl ;
}
*/
} //namespace SynerEdge
