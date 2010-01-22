#include <string.h>
#include <iostream>
#include "seerror.hpp"
#include "Block.hpp"

/* Author: Glenn Tarcea
 * Modifications: Atul Prakash.
 */

using namespace std ;

namespace SynerEdge {

Block::Block(int64 bnum, int size) : blocknum(bnum), numbytes(0), size(size), allocatedbytes(true)
{
	//
	// Why doesn't it compile if I replace the numbytes<1024... check below with
	// if (! validBlockSize(numbytes)) {
	//	throw seerror("Block must be between 1024 and 8192 bytes in size") ;
	// }
	if ((size < 1024) || (size > 8192)) {
		throw seerror("Block must be between 1024 and 8192 bytes in size") ;
	}

	if (blocknum < 0) {
		throw seerror("Blocknum must be a positive number") ;
	}

//	cout << "blocknum = " << blocknum << endl ;

	bytes = new char[size] ;
	memset(bytes, 0, size) ;
	memset(reserved, 0, 8) ;


	/*cout << "sizeof individual elements:\n" ;
	cout << "nextblock: " << sizeof(nextblock) << endl ;
	cout << "prevblock: " << sizeof(prevblock) << endl ;
	cout << "reserved: " << sizeof(reserved) << endl ;
	cout << "size: " << sizeof(size) << endl ;
	cout << "freechunks: " << sizeof(freechunks) << endl ;
	cout << "bytes: " << sizeof(bytes) << endl ;*/
}

Block::Block(int size) : blocknum(0), numbytes(0), size(size), allocatedbytes(true)
{
	if (! validBlockSize(size)) {
		throw seerror("Block must be between 1024 and 8192 bytes in size") ;
	}

	bytes = new char[size] ;
	memset(bytes, 0, size) ;
	memset(reserved, 0, 8) ;
}

Block::Block() : blocknum(0), numbytes(0), size(0), allocatedbytes(false)
{
	bytes = NULL;
}


/*
This is a special case for performance optimization. Only BlockFile.cpp
uses this function to speed up the writes to the blockfile to avoid
an extra copy. The "size" in this case is same as the size of the data
and the block.
*/
Block::Block(int size, char *data) : blocknum(0), numbytes(size), size(size), bytes(data), allocatedbytes(false)
{
	if (! validBlockSize(size)) {
		throw seerror("Block must be between 1024 and 8192 bytes in size") ;
	}
}

Block::~Block()
{
	if (allocatedbytes) {
		delete [] bytes ;
		bytes = NULL;
		allocatedbytes = false;
	}
}
ostream &operator<<(ostream &output, const Block &block)
{

	// assert(block.size > 0);
	// assert(block.numBytes > 0);
	//cout << "write blocknum = " << block.blocknum << endl ;
	output.write(reinterpret_cast<const char *>(&block.blocknum), 
		sizeof(int64)) ;
//	cout << "write isfree = " << block.isfree << endl ;
	output.write(reinterpret_cast<const char *>(&block.isfree),
			   sizeof(int)) ;
	output.write(reinterpret_cast<const char *>(block.reserved), 
		sizeof(char)*8) ;
//	cout << "write block.numbytes = " << block.bytes << endl ;
	output.write(reinterpret_cast<const char *>(&block.numbytes), 
		sizeof(int)) ;
//	cout << "write block.size = " << block.size << endl ;
	output.write(reinterpret_cast<const char *>(&block.size), 
		sizeof(short)) ;
//	cout << "write block.bytes = " << block.bytes << endl ;
	if (block.size > 0) output.write(block.bytes, sizeof(char) * block.size) ;
//	cout << "Writing blocknum : " << block.blocknum << endl ;
	return output ;
}

istream &operator>>(istream &input, Block &block)
{
//	cout << "block.size before starting read = " << block.size << endl ;
	input.read(reinterpret_cast<char *>(&block.blocknum), 
			 sizeof(int64)) ;
//	cout << "read blocknum = " << block.blocknum << endl ;
	input.read(reinterpret_cast<char *>(&block.isfree),
			 sizeof(int));
	input.read(reinterpret_cast<char *>(block.reserved), 
		sizeof(char)*8) ;
//	cout << "read reserved = " << block.reserved << endl ;
	input.read(reinterpret_cast<char *>(&block.numbytes), 
		sizeof(int)) ;
//	cout << "read numbytes = " << block.numbytes << endl ;
	short newblocksize;
	input.read(reinterpret_cast<char *>(&newblocksize), 
		sizeof(short)) ;
	assert(newblocksize >= 0);
//	cout << "read size = " << block.size << endl ;
	if (newblocksize > 0) {
		if (newblocksize == block.size) {
			input.read(reinterpret_cast<char *>(block.bytes), sizeof(char) * block.size) ;
		} else {
			if (block.allocatedbytes) {delete []block.bytes;}
			block.size = newblocksize;
			block.bytes = new char[block.size];
			input.read(reinterpret_cast<char *>(block.bytes), sizeof(char) * block.size) ;
			block.allocatedbytes = true;
		}
	} else { // newblocksize = 0
		if (block.allocatedbytes) delete []block.bytes;
		block.allocatedbytes = false;
		block.size = 0;
		block.bytes = NULL;
	}
	return input ;
}

bool
Block::zeroBytes(int offset, int length)
{
	if (offset >= size || offset < 0) {
		return false ;
	}

	if (length > size || length < 0 || offset + length > size) {
		return false ;
	}
	assert(bytes);
	memset(bytes+offset, 0, length) ;

	return true ;
}

/*
// No copy if length == size

int
Block::setBytesPointer(char *data, int length)
{
	if (length == size) {
		// No copy, just set the pointer.
		if (allocatedbytes) {
			delete [] bytes ;
			bytes = NULL;
			allocatedbytes = false ;
		}
		bytes = data ;
		return length ;
	} else {
		return setBytes(0, data, length) ;
	}
}

*/

int
Block::setBytes(int offset, char *data, int length)
{
	if (offset >= size || offset < 0) {
		return 0 ;
	}

	if (length > size || length < 0 || offset + length > size) {
		return 0 ;
	}

	memcpy(bytes+offset, data, length) ;
	if (numbytes < offset+length) {
		numbytes = offset+length ;
	}

	//	isfree = 0 ;

	return length ;
}

/* 
 * This is used to truncate a block
 */

bool
Block::setNumBytes(int length)
{
	if (length < 0 || length > numbytes) {
		return false ;
	}

	numbytes = length ;

	return true ;
}

int
Block::getNumBytes()
{
	return numbytes ;
}

int
Block::getSize(void)
{
	return size ;
}

bool
Block::setBlockNum(int64 bnum)
{
	if (bnum < 0) {
		return false ;
	}

	blocknum = bnum ;
	return true ;
}

int64
Block::getBlockNum()
{
	return blocknum ;
}


/******************
 * What does this do?
 * Does it also truncate the block?
 * Modify numBytes to be the size of the block?
 ******************/
bool
Block::setSize(int newsize)
{
	if (! validBlockSize(newsize)) {
		return false ;
	}

	if (size != 0 && size != newsize) {
		return false;		  // not allowed to change size.
	} else if (size == 0) {           // size must be 0 or size = newsize
		// No allocation, so allocate new
		size = newsize ;
		allocatedbytes = true ;
		bytes = new char[size] ;
		memset(bytes, 0, size) ;
	} else {
		// Possibly do nothing. New size is same as old size
		//cout << "possibly redundant operation memset" << endl;
		memset(bytes, 0, size) ;
	}
	return true ;
}

bool
Block::validBlockSize(int sizetocheck)
{
	if ((sizetocheck < 1024) || (sizetocheck > 8192)) {
		return false ;
	}

	return true ;
}

int
Block::getObjectSize(int size)
{
	return headersize + size ;
}

int
Block::getHeaderSize()
{
	return headersize ;
}

char *
Block::getBytes()
{
	return bytes ;
}

char *
Block::getBytes(int offset)
{
	if (offset < 0 || offset >= numbytes) {
		return NULL ;
	}

	return bytes+offset ;

}


/*
* void
* Block::setIsFree(bool freeblock)
* {
* 	if (freeblock) {
* 		isfree = 1 ;
* 	} else {
* 		isfree = 0 ;
* 	}
* }
* 
* bool
* Block::isFree()
* {
* 	if (isfree == 1) {
* 		return true ;
* 	} else {
* 		return false ;
* 	}
* }
*/

} // namespace SynerEdge
