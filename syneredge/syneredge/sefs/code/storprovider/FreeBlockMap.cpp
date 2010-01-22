
#include "FreeBlockMap.hpp"
#include "seerror.hpp"
#include <iostream>

using namespace std ;

/*
** Blocks are kept in a bitmap. If a bit is set to 0 then it is allocated,
** a 1 indicates this is free. This may seem counter-intuitive, but it is
** done this way because the bitset class has a function to find first
** set bit, but not the first free bit. So we reverse the meaning of having
** a bit set to free. The init() virtual function must observe this behavior
** when setting up a bit array.
*/

namespace SynerEdge {

FreeBlockMap::FreeBlockMap(uint64 numblocks, uint64 startingblocknum)
		: startingblocknum(startingblocknum)
{
	if (numblocks < 1) {
		throw seerror("numblocks must be greater than 0") ;
	}

	if (startingblocknum < 1) {
		throw seerror("startingblocknum must be greater than 0") ;
	}

	blockmap = new boost::dynamic_bitset<>(numblocks) ;

	blockmap->set() ;
	// cout << "this = " << this << ". FreeBlockMap constructor: numblocks = " << numblocks << " , blockmap freelist size is " << blockmap->count() << endl;
}

FreeBlockMap::FreeBlockMap() : startingblocknum(1)
{
	blockmap = new boost::dynamic_bitset<>(10) ;
	blockmap->set() ;
	// cout << "this = " << this << ". FreeBlockMap constructor: blockmap freelist size is " << blockmap->count() << endl;
}

uint64 
FreeBlockMap::allocateBlock()
{
	uint64 bitnum ;

	// cout << "this = " << this << ". Before allocateBlock: blockmap free size is " << blockmap->count() << endl;
	bitnum = blockmap->find_first() ;

	if (bitnum == blockmap->npos) {
		return 0 ;
	}

	blockmap->set(bitnum, false) ;

	// cout << "this = "<< this << ". After allocateBlock: blockmap free size is " << blockmap->count() << endl;
	return bitnum + startingblocknum ;
}

bool
FreeBlockMap::freeBlock(uint64 blocknum)
{

	uint64 index = blocknum - startingblocknum ;

	if ((index < 0 ) || (index >= blockmap->npos)) {
		return false ;
	}
	// cout << "this = " << this << ". Before freeBlock: blockmap free size is " << blockmap->count() << endl;
	if ((*blockmap)[index]) {
		//std::cerr << "FreeBlockMap::freeBlock:  attempt to free an already free block" << endl;
		return false;  // block is already free
	}
	(*blockmap)[index] = true ;
	// cout << "this = " << this << ". Aafter freeBlock: blockmap free size is " << blockmap->count() << endl;
	return true ;
}

bool
FreeBlockMap::isFree(uint64 blocknum)
{
	uint64 index = blocknum - startingblocknum;
	if ((index < 0 ) || (index >= blockmap->npos)) {
		assert(false);
	}
	if ((*blockmap)[index]) return true;
	else return false;
}


uint64
FreeBlockMap::getFreeBlockCount()
{
	
	uint64 returnval =  blockmap->count() ;
	// cout << "this = " << this << ". getFreeBlockCount: return value is " << returnval << endl;
	return returnval;
}

istream &operator>>(istream &input, FreeBlockMap &bmap)
{
	input >> bmap.startingblocknum ;
	input >> (*bmap.blockmap) ;
	return input ;
}

ostream &operator<<(ostream &output, const FreeBlockMap &bmap)
{
	output << bmap.startingblocknum << endl ;
	output << (*bmap.blockmap) ;
	return output ;
}

} // namespace SynerEdge
