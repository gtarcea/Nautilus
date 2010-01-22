
#ifndef __BlockFileIO_INCLUDE_
#define __BlockFileIO_INCLUDE_

#include "SynerEdge.hpp"
#include "iFileIO.hpp"
#include "iBlockIO.hpp"
#include "FileBlockMap.hpp"

namespace SynerEdge {

class BlockFileIO : public iFileIO {

public:
	BlockFileIO(iBlockIO &blockfile) ;
	virtual ~BlockFileIO() ;

	bool open(string filename, int flags) ;
	bool release() ;
	int read(int64 offset, char *buf, int length) ;
	int write(int64 offset, const char *buf, int length) ;
	int64 getSize() ;
	bool truncate(int64 at) ;
	bool isAccessible(string filename, int flags) ;
	int blockSize() ;
	bool flush() ;
	int64 getBlockFileNumFreeBlocks() ;
	int64 getBlockFileNumBlocks() ;

private:
	// Helper functions
	static int blocksin(int what, int by) ;
	int getBlockNumAtOffset(int64 offset) ;
	bool updateBlockInfo(int64 blocknum, int offsetinblock, int amountwritten) ;
	int64 getLastBlock(int blocksize, int filebytes) ;
	bool extendFile(int64 at, int64 filebytes) ;
	bool truncateFile(int64 at) ;
	bool freeAllBlocksAfter(int64 offset) ;


	// Data members
	iBlockIO &bf ;
	FileBlockMap *map ;
	string filename ;
	bool fileisopen ;
	

} ; // class BlockFileIO

} ; // namespace SynerEdge

#endif // __BlockFileIO_INCLUDE_
