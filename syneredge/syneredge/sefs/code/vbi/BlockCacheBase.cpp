#include "BlockCacheBase.hpp"
#include "VirtualDisk.hpp"
#include "Block.hpp"

namespace SynerEdge {
bool BlockCacheBase::disk_writeBlock(uint64 blockID, unsigned int offset, 
							  unsigned int size, char *data)
{
	return disk->disk_writeBlock(blockID, offset, size, data);
}


bool BlockCacheBase::disk_writeBlock(Block &block)
{
	return disk->disk_writeBlock(block);

}
bool BlockCacheBase::disk_readBlock(uint64 blockID, Block &block)
{
	return disk->disk_readBlock(blockID, block);
}
bool BlockCacheBase::disk_zeroBlock(uint64 blocknum)
{
	return disk->disk_zeroBlock(blocknum);
}
bool BlockCacheBase::disk_releaseBlock(uint64 blocknum)
{
	return disk->disk_releaseBlock(blocknum);
}
} // namespace Syneredge
