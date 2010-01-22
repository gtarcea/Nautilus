
#ifndef __BLOCK_INCLUDE_
#define __BLOCK_INCLUDE_

#include "SynerEdge.hpp"
#include <iostream>
using std::ostream ;
using std::istream ;

namespace SynerEdge {

class Block {

     public:
	     Block(int64 bnum, int size) ;
		Block(int size) ;
		Block(int size, char *data) ;
		explicit Block() ;
		~Block() ;

		friend ostream & operator << (ostream &, const Block &) ;
		friend istream & operator >> (istream &, Block &) ;

		int setBytes(int offset, char *data, int length) ;

		// No copy if length == size
		// int setBytesPointer(char *data, int length) ;
 
		bool setNumBytes(int length) ;
		bool zeroBytes(int offset, int length) ;
		int getNumBytes() ;

		bool setSize(int newsize) ;
		int getSize(void) ;

		bool setBlockNum(int64 blocknum) ;
		int64 getBlockNum() ;

		static int getObjectSize(int size) ;
		static int getHeaderSize() ;
		char *getBytes() ;
		char *getBytes(int offset) ;

	     void setIsFree(bool freeblock) {
			    std::cout << "Block::setIsFree: This is DEPRICATED. Do not use. value set is fake." << std::endl;
			    isfree = freeblock?1:0;
		    };
	    bool isFree() {
		    std::cout << "Block::isFree: This is DEPRICATED. Do not use. Return value is fake." << std::endl;
		    return isfree?true:false;
		};

		static bool validBlockSize(int numbytes) ;

	private:
		static const int headersize = 
		sizeof(int64)+sizeof(int)+sizeof(char)*8+sizeof(int)+sizeof(short);
	     int64 blocknum ;
	     int isfree ;
		char reserved[8] ;
	     int numbytes ; // These are the number of valid bytes. 
	                    // size indicates how big the bytes array is.
		short size ;
		char *bytes ;

		// Not part of header, just internal state
		bool allocatedbytes ;
} ;

} ; /* namespace SynerEdge */

#endif /* __BLOCK_INCLUDE_*/
