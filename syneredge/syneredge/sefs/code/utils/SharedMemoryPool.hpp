#ifndef SygSharedMemoryPool_hpp
#define SygSharedMemoryPool_hpp

#include "SynerEdge.hpp"
#include "SharedMemory.hpp"

namespace SynerEdge
{

class SharedMemoryPoolException : public SynerEdgeException
{
public:
	SharedMemoryPoolException(const std::wstring &str)
	: SynerEdgeException(str) {}

	virtual ~SharedMemoryPoolException() {}
};

class SharedMemoryPool : public SharedMemory
{
public:
	SharedMemoryPool(size_t size, key name, GlobalSemaphore::Cleanup cleanup);
	virtual ~SharedMemoryPool();

	void *memalloc(size_t size);
	void memfree(void *address);

	syg_ulong_ptr getFixedAsULong(unsigned short pos) const;
	void *getFixedAsAddress(unsigned short pos) const;
	void setFixed(unsigned short pos, syg_ulong_ptr value) const;
	void setFixed(unsigned short pos, void *address) const;

	// debugging aids
	void dumpControlSegment();
	void dumpFreeList();

protected:
	//virtual void init();

private:
	struct Freelist
	{
		Freelist()
		: address(0), size(0), prev(0), next(0)
		{}

		syg_ulong_ptr address;
		size_t size;
		syg_ulong_ptr prev;
		syg_ulong_ptr next;
	};

	struct Control
	{
		Control() 
		: address(0), size(0), freeList(0), numBuckets(0) 
		{}

		syg_ulong_ptr address;
		size_t size;
		syg_ulong_ptr freeList;
		size_t numBuckets;
	};

	Control *control;

	void *normalize(syg_ulong_ptr address) const;
	syg_ulong_ptr denormalize(void *address) const;
	Freelist *normalizeToFreelist(syg_ulong_ptr address) const;
	syg_ulong_ptr denormalizeFromFreelist(Freelist *address) const;

	// helper functions
	static size_t roundUpSize(size_t size);
	syg_ulong_ptr startOfSegment() const;
	size_t sizeOfSegment() const;
	size_t numberOfFreelistBuckets() const;
	Freelist *findUnusedFreelistBucket() const;
	void insertFreelistEntry(void *realaddr, size_t realsize, Freelist *prevfl, Freelist *nextfl);
	void initializeControlSegment();

	// noncopyable semantics
	SharedMemoryPool(const SharedMemoryPool &pool);
	SharedMemoryPool &operator=(SharedMemoryPool &pool);
};

}

#endif
