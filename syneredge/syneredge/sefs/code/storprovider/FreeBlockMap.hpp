
#ifndef __FreeBlockMap_INCLUDE_
#define __FreeBlockMap_INCLUDE_

#include "boost/dynamic_bitset.hpp"
#include "SynerEdge.hpp"
#include <iostream>

using namespace std ;

namespace SynerEdge {

class FreeBlockMap {

public:
	FreeBlockMap(uint64 numblocks, uint64 startingblocknum) ;
	FreeBlockMap() ;

	uint64 allocateBlock() ; // Allocates block
	bool isFree(uint64 blocknum); /* Returns true if block is free. Returns false if blocknum 
							  out of range or allocated */
	bool freeBlock(uint64 blocknum) ; // Returns block to free list

	uint64 getFreeBlockCount() ;

	friend istream &operator>>(istream &input, FreeBlockMap &bmap) ;
	friend ostream &operator<<(ostream &output, const FreeBlockMap &bmap) ;


private:
	boost::dynamic_bitset<> *blockmap ;
	uint64 startingblocknum ;

} ; // class FreeBlockMap

} ; // namespace SynerEdge
#endif // __FreeBlockMap_INCLUDE_
