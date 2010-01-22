#include "SharedMemoryPool.hpp"
#include "TraceThread.hpp"

#include "boost/format.hpp"

namespace SynerEdge
{

SharedMemoryPool::SharedMemoryPool(size_t size, key name, GlobalSemaphore::Cleanup cleanup) 
: SharedMemory(roundUpSize(size), name, cleanup), control(0)
{
	control = static_cast<Control *>(getAddress());
	initializeControlSegment();

	dumpControlSegment();
	dumpFreeList();
}

SharedMemoryPool::~SharedMemoryPool()
{
}

void *SharedMemoryPool::memalloc(size_t size)
{
	TRACE_SETUP(L"utils");

	void *result = NULL;
	syg_ulong_ptr offset = 0;
	Freelist *prevfl = NULL;
	Freelist *nextfl = NULL;
	Freelist *thisfl = NULL;
	Freelist *usefl = NULL;

	size_t additionalSize = sizeof(syg_ulong_ptr);
	size_t mallocSize = size + additionalSize;

	// round of malloc size to "additionalSize" boundary.
	if ((mallocSize % additionalSize) != 0)
	{
		mallocSize = ((mallocSize + additionalSize) / additionalSize)
				 * additionalSize;
	}

	lock();

	thisfl = normalizeToFreelist(control->freeList);
	nextfl = (thisfl != NULL) ? normalizeToFreelist(thisfl->next) : NULL;
	while (thisfl != NULL)
	{
		if (thisfl->size >= mallocSize)
		{
			if (usefl == NULL)
			{
				usefl = thisfl;
			}
			else
			{
				if (usefl->size > thisfl->size)
				{
					usefl = thisfl;
				}
			}
		}
	
		prevfl = thisfl;
		thisfl = nextfl;
		nextfl = (thisfl != NULL) ? 
			 normalizeToFreelist(thisfl->next) : NULL;
	}

	if (usefl != NULL)
	{
		usefl->size -= mallocSize;
		offset = usefl->address + usefl->size;
		result = normalize(offset);

		// handle the case where this bucket is consumed.
		if (usefl->size == 0)
		{
			if (usefl->prev == 0)
			{
				control->freeList = usefl->next;
			}
			else
			{
				Freelist *prevfl = normalizeToFreelist(usefl->prev);
				prevfl->next = usefl->next;
			}

			if (usefl->next != 0)
			{
				Freelist *nextfl = normalizeToFreelist(usefl->next);
				nextfl->prev = usefl->prev;
			}

			// mark bucket as unused.
			usefl->address = 0;
			usefl->size = 0;
		}

		if (result != NULL)
		{
			size_t *sizeptr = reinterpret_cast<size_t *>(result);
			*sizeptr = mallocSize;
			sizeptr++;

			result = reinterpret_cast<void *>(sizeptr);

			boost::wformat fmt(L"Allocated memory from shared pool: %ld bytes at %ld (address: %lx).  Requested size was: %ld");
			fmt % mallocSize 
			    % offset
			    % reinterpret_cast<syg_ulong_ptr>(result) 
			    % size;
			TRACE(TraceData::info, fmt.str());
		}
	}

	unlock();

	return result;
}

void SharedMemoryPool::memfree(void *address)
{
	TRACE_SETUP(L"utils");

	if ((address < normalize(control->address)) || 
	     address > normalize(control->address + control->size))
	{
		TRACE(TraceData::info, L"Cannot free - address out of range");
		return;
	}
	
	size_t additionalSize = sizeof(size_t);
	void *realaddr = reinterpret_cast<void *>
		( reinterpret_cast<char *>(address) - additionalSize);
	size_t *sizeptr = reinterpret_cast<size_t *>(realaddr);
	size_t realsize = *sizeptr;

	Freelist *prevfl = NULL;
	Freelist *nextfl = NULL;
	Freelist *thisfl = NULL;

	thisfl = normalizeToFreelist(control->freeList);
	nextfl = (thisfl != NULL) ? normalizeToFreelist(thisfl->next) : NULL;

	while (thisfl != NULL)
	{
		if (realaddr <= normalize(thisfl->address + thisfl->size))
			break;

		prevfl = thisfl;
		thisfl = nextfl;
		nextfl = (thisfl != NULL) ? 
			normalizeToFreelist(thisfl->next) : NULL;
	}

	if (thisfl == NULL)
	{
		// add to end of freelist
		insertFreelistEntry(realaddr, realsize, prevfl, thisfl);
	}
	else
	{
		void *addrAfter = reinterpret_cast<void *>
		   (reinterpret_cast<char *>(realaddr) + realsize);

		if (realaddr == normalize(thisfl->address + thisfl->size))
		{
			if ((nextfl != NULL) && 
			    (addrAfter == normalize(nextfl->address)))
			{
				// this bucket needs to merge with
				// the next bucket.
				thisfl->size += realsize + nextfl->size;
				thisfl->next = nextfl->next;

				if (nextfl->next != 0)
				{
					Freelist *fl = normalizeToFreelist(nextfl->next);
					fl->prev = denormalizeFromFreelist(thisfl);
				}

				nextfl->address = 0;
				nextfl->size = 0;
			}
			else
			{
				// fits at end of current bucket.
				thisfl->size += realsize;
			}
		}
		else if (realaddr == normalize(thisfl->address))
		{
			if ((prevfl != NULL) &&
			    (realaddr == normalize(prevfl->address + prevfl->size)))
			{
				// this bucket needs to merge with
				// the previous bucket
				prevfl->size += realsize + thisfl->size;
				prevfl->next = denormalizeFromFreelist(nextfl);

				if (nextfl != NULL)
				{
					nextfl->prev = denormalizeFromFreelist(prevfl);
				}

				thisfl->address = 0;
				thisfl->size = 0;
			}
			else
			{
				// fits at begining of current bucket
				thisfl->address = denormalize(realaddr);
				thisfl->size += realsize;
			}
		}
		else
		{
			// insert before current bucket
			insertFreelistEntry(realaddr, realsize, prevfl, thisfl);
			
		}
	}
}

syg_ulong_ptr SharedMemoryPool::getFixedAsULong(unsigned short pos) const
{
	syg_ulong_ptr result = 0;

	if (pos <= 3)
	{
		syg_ulong_ptr *addr = static_cast<syg_ulong_ptr *>
			(normalize(control->address - sizeof(Control)));
		addr += pos;
		result = *addr;
	}

	return result;
}

void *SharedMemoryPool::getFixedAsAddress(unsigned short pos) const
{
	syg_ulong_ptr res = getFixedAsULong(pos);
	return normalize(res);
}

void SharedMemoryPool::setFixed(unsigned short pos, syg_ulong_ptr value) const
{
	if (pos <= 3)
	{
		syg_ulong_ptr *addr = static_cast<syg_ulong_ptr *>
			(normalize(control->address - sizeof(Control)));
		addr += pos;
		*addr = value;
	}
}

void SharedMemoryPool::setFixed(unsigned short pos, void *address) const
{
	setFixed(pos, denormalize(address));
}

SharedMemoryPool::Freelist *
SharedMemoryPool::normalizeToFreelist(syg_ulong_ptr address) const
{
	return reinterpret_cast<Freelist *>(normalize(address));
}

syg_ulong_ptr SharedMemoryPool::denormalizeFromFreelist(Freelist *ptr) const
{
	return denormalize(reinterpret_cast<void *>(ptr));
}


void *SharedMemoryPool::normalize(syg_ulong_ptr address) const
{
	void *result = 0;
	if (address != 0) 
	{
		result = reinterpret_cast<void *>
			((reinterpret_cast<char * const>(control) + address));
	}

	return result;
}

syg_ulong_ptr SharedMemoryPool::denormalize(void *address) const
{
	syg_ulong_ptr result = 0;
	if (address != 0)
	{
		result = (reinterpret_cast<char * const>(address) - 
			  reinterpret_cast<char * const>(control));
	}

	return result;
}

size_t SharedMemoryPool::roundUpSize(size_t size)
{
	// round off so size is evenly divisible by 4 * sizeof(Control).
	// first 1/4 is for control information
	// last 3/4 is for data
	size_t result = size;
	size_t roundSize = sizeof(struct Control) * 4;
	if (size % roundSize != 0)
	{
		result = ((size + roundSize) / roundSize) * roundSize;
	}

	return result;
}

syg_ulong_ptr SharedMemoryPool::startOfSegment() const
{
	return (getSize() / 4);
}

size_t SharedMemoryPool::sizeOfSegment() const
{
	return (3 * getSize()) / 4;
}

size_t SharedMemoryPool::numberOfFreelistBuckets() const
{
	size_t result = ((startOfSegment() - control->freeList) 
			/ sizeof(Freelist));
	result--;  // last freelist bucket holds "fixed position" ptrs.
	return result;
}

SharedMemoryPool::Freelist *
SharedMemoryPool::findUnusedFreelistBucket() const
{
	Freelist *result = normalizeToFreelist(control->freeList);

	for (size_t i = 0; 
	     (result != 0) && (i < numberOfFreelistBuckets()); 
	     i++, result++)
	{
		if (result->address == 0) 
		{
			break;
		}
	}

	return result;
}

void SharedMemoryPool::insertFreelistEntry(void *realaddr, size_t realsize, Freelist *prevfl, Freelist *nextfl)
{
	Freelist *result = findUnusedFreelistBucket();
	if (result != 0)
	{
		result->address = denormalize(realaddr);
		result->size = realsize;
		result->prev = denormalizeFromFreelist(prevfl);
		result->next = denormalizeFromFreelist(nextfl);

		if (prevfl)
		{
			prevfl->next = denormalizeFromFreelist(result);
		}
		else
		{
			control->freeList = denormalizeFromFreelist(result);
		}

		if (nextfl)
		{
			nextfl->prev = denormalizeFromFreelist(result);
		}
	}
	else
	{
		throw SharedMemoryPoolException(
		L"SharedMemoryPool too fragmented - cannot add freelist item!");
	}
}

void SharedMemoryPool::initializeControlSegment()
{
	control = static_cast<Control *>(getAddress());
	if ((willCleanup()) && (control != 0))
	{
		control->address = startOfSegment();
		control->freeList = 0;
		control->size = sizeOfSegment();
		control->numBuckets = numberOfFreelistBuckets();

		Freelist *wholespace = 0;
		wholespace = reinterpret_cast<Freelist *>
		    (reinterpret_cast<char *>(control) + sizeof(Control));	

		wholespace->address = control->address;
		wholespace->size = control->size;
		wholespace->prev = 0;
		wholespace->next = 0;

		control->freeList = reinterpret_cast<char *>(wholespace) - 
				    reinterpret_cast<char *>(control);
	}	
}


void SharedMemoryPool::dumpControlSegment()
{
	TRACE_SETUP(L"utils");

	boost::wformat fmt(L"control addr: %lx segment addr: %ld, freelist: %ld, size: %ld, numBuckets: %d");
	fmt % (syg_ulong_ptr) control
	    % (syg_ulong_ptr) control->address
	    % (syg_ulong_ptr) control->freeList
	    % (syg_ulong_ptr) control->size
	    % (syg_ulong_ptr) control->numBuckets;

	TRACE(TraceData::info, fmt.str());
}

void SharedMemoryPool::dumpFreeList()
{
	TRACE_SETUP(L"utils");

	boost::wformat fmt(L"start of freelist: %ld");
	fmt % (syg_ulong_ptr) control->freeList;
	TRACE(TraceData::info, fmt.str());

	Freelist *fl = normalizeToFreelist(control->freeList);

	while (fl != NULL)
	{
		boost::wformat fmt(L"address: %ld, size: %ld, prev: %ld, next: %ld");
		fmt % fl->address % fl->size % fl->prev % fl->next;
		TRACE(TraceData::info, fmt.str());


		fl = normalizeToFreelist(fl->next);
	}
}

}

