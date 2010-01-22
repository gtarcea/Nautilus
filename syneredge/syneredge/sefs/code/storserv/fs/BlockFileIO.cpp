
#include "BlockFileIO.hpp"
#include "seerror.hpp"
#include "PT.hpp"
//#include <iostream>

using namespace std ;

namespace SynerEdge {

BlockFileIO::BlockFileIO(iBlockIO &blockfile) : bf(blockfile), fileisopen(false), map(NULL)
{
	if (! bf.open()) {
		throw seerror("No such blockfile") ;
	}
}

BlockFileIO::~BlockFileIO()
{
	//delete bf ;
	bf.flushFreeBlockList() ;
	delete map ;
}

bool
BlockFileIO::open(string filename, int flags)
{
	PT("BlockFileIO::open()") ;
	map = new FileBlockMap(filename) ;
	fileisopen = true ;
	PT("Leaving BlockFileIO::open()") ;
	return true ;
}

bool
BlockFileIO::flush()
{
	bf.flushFreeBlockList() ;

	if (! fileisopen) {
		return false ;
	}

	map->save() ;

	return true ;
}

bool
BlockFileIO::release()
{
	bf.flushFreeBlockList() ;

	if (! fileisopen) {
		return false ;
	}

	if (map) {
		delete map ;
	}

	map = NULL ;
	fileisopen = false ;
	return true ;
}

int
BlockFileIO::blocksin(int what, int by)
{
	int dividebyblocks ;
	int modin ;
	int blocks ;
	/*
	** We want to start from 1, what is based on 0-by-1 so add one to
	** what to get the correct calculation.
	*/
	what++ ;
	dividebyblocks = what/by ;
	if (! dividebyblocks) {
		//printf("    blocksin = 1\n") ;
		return 1 ;
	} else {
		blocks = ++dividebyblocks ;
		modin = what%by ;
		if (!modin) {
			blocks-- ;
		}
	}
	//printf("    blocksin = %d\n", blocks) ;
	return blocks ;
}

int
BlockFileIO::getBlockNumAtOffset(int64 offset)
{
	PT("BlockFileIO::getBlockNumAtOffset") ;
	int what = offset ;
	int by = bf.getBlockSize() ;
	int bindex ;
	int rc ;

	// Convert offset into blockcount. We'll start
	// at 1 as the first block and work from there.

	bindex = blocksin(what, by) ;

	rc = map->getBlockNumAt(bindex) ;

	PT("Leaving BlockFileIO::getBlockNumAtOffset") ;

	return rc ;
}

int
BlockFileIO::read(int64 offset, char *buf, int length)
{
	int totalBytesToRead = length ;
	int64 startingat = offset ;
	char *data ;
	int amountToCopy ;
	int amountRead = 0 ;
	int blocknum ;
	int offsetInBlockToStartFrom ;

	if (! fileisopen) {
		return -1 ;
	}

	int blocksize = bf.getBlockSize() ;
	Block b(blocksize) ;

	// We may have been asked to read more than exists,
	// if that is the case then adjust totalBytesToRead
	// appropriately.
	int filebytes = map->getByteCount() ;

	//cout <<
	//	"blocksize = " << blocksize << endl <<
	//	"filebytes = " << filebytes << endl <<
	//	"totalBytesToRead = " << totalBytesToRead << endl <<
	//	"offset = " << offset << endl ;
	if (filebytes < totalBytesToRead+offset) {
		// Ok, given the offset and length request, we've been
		// asked to read too much. So, now we need to figure
		// out how much we can read, and check for the special case
		// where the request makes no sense.
		//
		totalBytesToRead = filebytes - offset ;
		if (totalBytesToRead <= 0) {
	//		cout << "totalBytesToRead <= 0 " << totalBytesToRead << endl ;
			return 0 ;
		}
	}

	//
	// Loop through reading blocks of data to fulfill the request.
	//
	data = buf ;
	while(1) {
		// Get blocknum to read from
		blocknum = getBlockNumAtOffset(startingat) ;
		if (blocknum == 0) {
			// No more blocks to read
			break ;
		}
		// Figure out offset in block to write to (zero based)
		offsetInBlockToStartFrom = startingat % blocksize ;
		// Figure out how many bytes we are writing
		amountToCopy = blocksize - offsetInBlockToStartFrom ;
		// Check if amountToCopy is greater than amount to read,
		// if so, then adjust accordingly.
		if (amountToCopy > totalBytesToRead) {
			amountToCopy = totalBytesToRead ;
		}
		// If a request was sent to read more than in block (length or offset in)
		// then we'll have a negative value in amountToCopy. There is nothing
		// to do in that case except make sure we exit the loop by
		// setting totalBytesToRead to 0, and amountToCopy to 0.
	//	cout << "amountToCopy = " << amountToCopy << endl ;
	//	cout << "blocknum = " << blocknum << endl ;
		if (amountToCopy > 0) {
			// Read block
			bool result = bf.readBlock(blocknum, b) ;
	//		cout << "Result of readBlock = " << result << endl ;
			// Copy bytes starting at offset in block
			memcpy(data, b.getBytes(offsetInBlockToStartFrom), amountToCopy) ;
			// Move data pointer
			data+= amountToCopy ;
		} else {
			amountToCopy = 0 ;
			totalBytesToRead = 0 ;
		}
		// We now have less data to read
		totalBytesToRead -= amountToCopy ;

		amountRead += amountToCopy ;
		// Are we done?
		if (totalBytesToRead <= 0) {
			break ;
		}
		// Adjust for next set of calculations
		startingat = startingat+amountToCopy ;
	}
	return amountRead ;
}

bool
BlockFileIO::updateBlockInfo(int64 blocknum, int offsetinblock, 
		int amountwritten)
{
	PT("BlockFileIO::updateBlockInfo") ;

	//cout << "updating block " << blocknum << " at offsetinblock " <<
	//	offsetinblock << " with amountwritten " << amountwritten << endl ;
	if (! map->blockExists(blocknum)) {
		// Block wasn't in the list in the past,
		// update map.
		map->addBlock(blocknum, amountwritten) ;
	} else {
		int blockbytes = map->getBlockByteCount(blocknum) ;
		
		if (amountwritten > blockbytes) {
			// We wrote more than was in block before.
			map->updateBlock(blocknum, amountwritten) ;
		} else if ((amountwritten+offsetinblock) > blockbytes) {
			// The offset in the block, plus the amount written
			// extended what was originally there.
			map->updateBlock(blocknum, amountwritten+offsetinblock) ;
		} /* else nothing to update */
	}

	PT("Leaving BlockFileIO::updateBlockInfo") ;
}


int
BlockFileIO::write(int64 offset, const char *buf, int length)
{
	int totalBytesToWrite = length ;
	int64 startingat = offset ;
	char *data ;
	int amountToWrite ;
	int amountWritten = 0 ;
	int blocknum ;
	int offsetInBlockToStartFrom ;
	PT("BlockFileIO::write()") ;

	if (! fileisopen) {
		return -1 ;
	}

	// Make sure request is sane before we try and do it. The length
	// must be a positive number, and offset must be in the range of
	// 0 to filelength -1.
	if (length <= 0) {
		return 0 ;
	}

	int filebytes = map->getByteCount() ;
	if ((offset < 0) || (offset > filebytes)) {
		return 0 ;
	}

	int blocksize = bf.getBlockSize() ;

	data = const_cast<char *>(buf) ;
	while(1) {
		// Get blocknum to write to
		//cout << endl << endl << "starting at = " << startingat << endl << endl ;
		blocknum = getBlockNumAtOffset(startingat) ;
		if (blocknum == 0) {
			// Need to allocate a new block
			blocknum = bf.getFreeBlock() ;
			if (blocknum == 0) {
				// No more free blocks
				break ;
			}
		}

		// Figure out offseting block to write to
		offsetInBlockToStartFrom = startingat % blocksize ;
		// Figure out how many bytes we are writing
		amountToWrite = blocksize - offsetInBlockToStartFrom ;
		// Check to see if there is that much left to write,
		// otherwise adjust accordingly.
		if (amountToWrite > totalBytesToWrite) {
			amountToWrite = totalBytesToWrite ;
		}
		// Write data (make sure to set the offset in data correctly
		//cout << "writing to blocknum " << blocknum << endl ;
		if (bf.writeBlock(blocknum, offsetInBlockToStartFrom,
			amountToWrite, data+amountWritten) == false) {
			// write failed.
			return amountWritten;
		};
		// Update block information
		updateBlockInfo(blocknum, offsetInBlockToStartFrom,
				amountToWrite) ;
		// We now have less data to write
		totalBytesToWrite -= amountToWrite ;

		amountWritten += amountToWrite ;
		// Are we done?
		if (totalBytesToWrite <= 0) {
			break ;
		}
		// Adjust for next set of calculations
		startingat = startingat+amountToWrite ;
	}

	//map->save() ;

	PT("Leaving BlockFileIO::write()") ;
	return amountWritten ;
}
		
int64 
BlockFileIO::getSize()
{
	if (! fileisopen) {
		return -1 ;
	}

	return map->getByteCount() ;
}

int64
BlockFileIO::getLastBlock(int blocksize, int filebytes)
{
	int bin = blocksin(filebytes, blocksize) ;

	return map->getBlockNumAt(bin) ;
}

bool
BlockFileIO::extendFile(int64 at, int64 filebytes)
{
	//cout << "In extendFile at = " << at << " filebytes = " << filebytes << endl ;
	int blocksize = bf.getBlockSize() ;
	int64 totalAmountToExtend = at - filebytes ;
	int64 blocknum ;
	int64 amountToExtendBy ;
	Block b(blocksize) ; // We are guaranteed data is all zeros.
	bool newblock = false ;

	// Figure out how much to extend current last block by
	
	blocknum = getLastBlock(blocksize, filebytes) ;
	//cout << "After getLastBlock() blocknum = " << blocknum << endl ;
	if (blocknum == 0) {
		blocknum = bf.getFreeBlock() ;
		map->addBlock(blocknum, 0) ;
		newblock = true ;
	}
	int blockbytes = map->getBlockByteCount(blocknum) ;
	int amountleftinblock = blocksize - blockbytes ;

	if (amountleftinblock < 0) {
		amountToExtendBy = blockbytes - at%blocksize ;
	} else {
		amountToExtendBy = amountleftinblock ;
	}

	if (amountToExtendBy > totalAmountToExtend) {
		amountToExtendBy = totalAmountToExtend ;
	}
	//cout << endl << endl <<
	//	"amountToExtendBy = " << amountToExtendBy << endl <<
	//	"totalAmountToExtend = " << totalAmountToExtend << endl <<
	//	"amountleftinblock = " << amountleftinblock << endl <<
	//	"blockbytes = " << blockbytes << endl <<
	//	"filebytes = " << filebytes << endl <<
	//	"blocknum = " << blocknum << endl <<
	//	"at = " << at << endl << endl ;
	map->updateBlock(blocknum, amountToExtendBy+blockbytes) ;

	if (newblock) {
	//	cout << "newblock" << endl ;
		b.setBlockNum(blocknum) ;
		b.setNumBytes(amountToExtendBy) ;
	} else {
		// Existing block, read in to update.
	//	cout << "existing block" << endl ;
		bf.readBlock(blocknum, b) ;
		b.setNumBytes(amountToExtendBy+blockbytes) ;
		b.zeroBytes(blockbytes, amountToExtendBy) ;
	}
	//cout << "Writing to block " << blocknum << endl ;
	bf.writeBlock(b) ;
	totalAmountToExtend -= amountToExtendBy ;

	// Are we done extending?

	if (totalAmountToExtend) {
		// We still have work to do
		
		while (1) {
			amountToExtendBy = blocksize ;
			if (amountToExtendBy > totalAmountToExtend) {
				amountToExtendBy = totalAmountToExtend ;
			}
			// Get new block
			blocknum = bf.getFreeBlock() ;
			if (blocknum == 0) {
				// No more free blocks
				// What should we do?
				break ;
			}
			// Set blocknum and size, then write
			b.setBlockNum(blocknum) ;
			b.setNumBytes(amountToExtendBy) ;
	//		cout << "bf.writeBlock() blocknum = " << blocknum << endl ;
			bf.writeBlock(b) ;
			// Update file meta data
			updateBlockInfo(blocknum, 0, amountToExtendBy) ;

			// Book keeping, and check if we are done
			totalAmountToExtend -= amountToExtendBy ;
			if (totalAmountToExtend <= 0) {
				break ;
			}
		}
	}

	map->save() ;
	return true ;
}

bool
BlockFileIO::freeAllBlocksAfter(int64 offset)
{
	// The offset will give us the last block that is
	// to be a part of the file. All we needed to do
	// is continue adding blocksize to this until we
	// have no more blocks that are a part of the file
	// and we are done.
	int blocksize = bf.getBlockSize() ;
	int64 nextoffset = offset+blocksize ;

	while (1) {
		int blocknum = getBlockNumAtOffset(nextoffset) ;
		if (blocknum == 0) {
			// We are done!
			break ;
		}
		map->removeBlock(blocknum) ;
		bf.releaseBlock(blocknum) ;
		nextoffset += blocksize ;
	}

	return true ;
}

bool
BlockFileIO::truncateFile(int64 at)
{
	int blocksize = bf.getBlockSize() ;
	int64 blocknum = getBlockNumAtOffset(at) ;
	int bytesinblocktoleave = at%blocksize ;
	Block b(blocksize) ;

	map->updateBlock(blocknum, bytesinblocktoleave) ;
	// Read in block and to zero.
	bf.readBlock(blocknum, b) ;
	b.zeroBytes(bytesinblocktoleave, blocksize-bytesinblocktoleave) ;
	b.setNumBytes(bytesinblocktoleave) ;
	bf.writeBlock(b) ;
	freeAllBlocksAfter(at) ;
	map->save() ;
	return true ;
}

bool
BlockFileIO::truncate(int64 at)
{
	if (! fileisopen) {
	//	cout << "file is not open" << endl ;
		return false ;
	}

	// Check if truncate makes sense
	if ( at < 0) {
	//	cout << " at < 0 " << at << endl ;
		return false ;
	}

	// Are we extending or truncating the file?
	int filebytes = map->getByteCount() ;
	if (at > filebytes) {
	//	cout << "extendFile at/filebytes " << at << "/" << filebytes << endl ;
		return extendFile(at, filebytes) ;
	} else {
	//	cout << "truncateFile at " << at << endl ;
		return truncateFile(at) ;
	}
}

bool
BlockFileIO::isAccessible(string filename, int flags)
{
	// No flags to check at the moment.
	return true ;
}

int
BlockFileIO::blockSize()
{
	return bf.getBlockSize() ;
}

int64
BlockFileIO::getBlockFileNumFreeBlocks()
{
	return bf.getNumFreeBlocks() ;
}

int64
BlockFileIO::getBlockFileNumBlocks()
{
	return bf.getNumBlocks() ;
}

} // namespace SynerEdge
