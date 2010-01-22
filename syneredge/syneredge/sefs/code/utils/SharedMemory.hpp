#ifndef SygSharedMemory_h
#define SygSharedMemory_h

#include "SynerEdge.hpp"
#include "GlobalSemaphore.hpp"

#include "boost/utility.hpp"

#include <string>

namespace SynerEdge
{

class SharedMemoryException : public SynerEdgeException
{
public:
	SharedMemoryException(const std::wstring &msg) 
	: SynerEdgeException(msg) {}
	virtual ~SharedMemoryException() {}
};

class SharedMemory : boost::noncopyable
{
public:
	typedef int key;

	SharedMemory(size_t size, key name, GlobalSemaphore::Cleanup cleanup);
	virtual ~SharedMemory();

	void lock();
	void unlock();
	
	key getName() const;
	size_t getSize() const;
	void *getAddress() const;

	bool willCleanup();

protected:
	virtual void init();

private:
	key name;
	size_t size;
	void *address;
	int memkey;
	int shmid;

	GlobalSemaphore sem;

	// noncopyable semantics
	SharedMemory(const SharedMemory &);
	SharedMemory &operator=(const SharedMemory &);
};

}

#endif

