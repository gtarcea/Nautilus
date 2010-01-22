
#ifndef __VirtualDisk_INCLUDE_
#define __VirtualDisk_INCLUDE_

#include <string>
#include <list>
#include "VirtualBlockFile.hpp"
#include "BlockCacheStd.hpp"

using namespace std ;

namespace SynerEdge {

	const int cacheSize = 100;       // Size in blocks


class VirtualDisk : public iBlockIO, private boost::noncopyable {
	friend class BlockCacheBase;
public:
	// These are the mirrors for this disk
	typedef std::list<VirtualBlockFile*> VirtualBlockFileList ;
	typedef std::list<VirtualBlockFile*>::iterator VirtualBlockFileListIterator;

	VirtualDisk(string name, int blocksize, VirtualBlockFileList &vbfilelist) ;
	~VirtualDisk() ;

	bool open() ;
	bool close() ;
	uint64 getNumBlocks() ;
	uint64 getNumFreeBlocks() ;
	uint64 getFreeBlock() ;
	bool releaseBlock(uint64 blocknum) ;
	bool flushFreeBlockList() ;
	int getBlockSize() const ;
	bool writeBlock(uint64 blocknum, int offset, int size, char *data) ;
	bool writeBlock(Block &block) ;
	bool readBlock(uint64 blocknum, Block &block) ;
	bool zeroBlock(uint64 blocknum) ;
	// bool freeBlock(uint64 blocknum) ;
	void setMirrorBlocking(bool blockonwrite) ;
	bool getMirrorBlocking() ;
	bool repair() ;
	BlockCacheStd *getCache() {return blockcache;}

	// Should be private. To help with testing, they are made public.
	bool disk_writeBlock(uint64 blocknum, int offset, int size, char *data) ;
	bool disk_writeBlock(Block &block) ;
	bool disk_readBlock(uint64 blocknum, Block &block) ;
	bool disk_zeroBlock(uint64 blocknum) ;
	bool disk_releaseBlock(uint64 blocknum) ;


private:
	bool validateAccess(uint64 blocknum);
	bool useCache;             // use or not use cache. For debugging.
	string name ;
	int blocksize ;
	bool isopen ;
	bool blockonwrite ;
	VirtualBlockFileList vbflist ;
	BlockCacheStd *blockcache;        // Cache. Can be constructed using a factory
	static const unsigned int cachesize = 10;
} ;

} ; // namespace SynerEdge
#endif // __VirtualDisk_INCLUDE_
