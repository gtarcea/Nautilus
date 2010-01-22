
#ifndef __FileBlockMap_INCLUDE_
#define __FileBlockMap_INCLUDE_

#include "fileblockmap.h"
#include <string>
#include <ext/hash_map>

using std::string ;
using namespace __gnu_cxx ;

namespace SynerEdge {

class FileBlockMapHashVal {
public:
	FileBlockMapHashVal(FILEBLOCKP fblock) : fblock(fblock) {}
	FileBlockMapHashVal() {}
	inline int getBlockNum() const { return fileblock_getblocknum(fblock); }
	inline FILEBLOCKP getFileBlock() const { return fblock ; }
private:
	FILEBLOCKP fblock ;
} ;

class FileBlockMap {

public:
	FileBlockMap(string filename) ;
	~FileBlockMap() ;

	static bool exists(string filename) ;
	bool deleteMap() ;
	bool addBlock(int blocknum, int bytes) ;
	bool removeBlock(int blocknum) ;
	bool updateBlock(int blocknum, int bytes) ;
	bool save() ;
	int getBlockByteCount(int blocknum) ;
	bool blockExists(int blocknum) ;
	int getBlockCount() ;
	bool addHost(string host) ;
	bool removeHost(string host) ;
	bool hostExists(string host) ;
	int getByteCount() ;
	int getBlockNumAt(int index) ;

private:
	FileBlockMap() ;

	bool blocknumExists(int blocknum) ;
	bool indexExists(int index) ;


	struct eqint {
		bool operator()(const int a, const int b) const
		{
			return a == b ;
		}
	} ;

	FILEBLOCKMAPP fbmap ;
	string fbname ;
	bool deleted ;
	int bytecount ;
	int currentindex ;
	bool rebuildbyindex ;
	hash_map<int, FileBlockMapHashVal, hash<int>, eqint> blocksetByBlocknum ;
	hash_map<int, FileBlockMapHashVal, hash<int>, eqint> blocksetByIndex ;
//	hash_map<int, int, hash<int>, eqint> blocksetByBlocknum ;
//	hash_map<int, int, hash<int>, eqint> blocksetByIndex ;

	hash_map<int, FileBlockMapHashVal, hash<int>, eqint>::iterator blocksetIterator ;
} ; // class FileBlockMap

} ; // namespace SynerEdge

#endif // __FileBlockMap_INCLUDE_
