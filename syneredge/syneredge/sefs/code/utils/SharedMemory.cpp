#include "SharedMemory.hpp"
#include "StringConversion.hpp"
#include "TraceThread.hpp"

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/stat.h>
#include <errno.h>

#include "boost/filesystem/path.hpp"
#include "boost/format.hpp"

#include <iostream>
#include <fstream>

namespace SynerEdge
{

#define SHM_PERM (S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH)

SharedMemory::SharedMemory(size_t size_, key name_, GlobalSemaphore::Cleanup cleanup)
: size(size_), name(name_), address(0), shmid(-1),
  sem(1, StringConversion::toUTF16(L"sygsem", name_), cleanup)
{
	TRACE_SETUP(L"utils");

	boost::wformat fmt(L"sharedmem_%d.mem");
	fmt % name_;
	boost::filesystem::path pth("/tmp");
	pth /= StringConversion::toUTF8(fmt.str());

	key_t memkey = ftok(pth.native_file_string().c_str(), name_);

	if (memkey == (key) -1)
	{
		std::wofstream fil(pth.native_file_string().c_str(), std::ios::out);
		fil.close();
		memkey = ftok(pth.native_file_string().c_str(), name_);
	}

	int id = -1;
	bool callInit = false;
	if (memkey != (key) -1)
	{
		id = shmget(memkey, size_, IPC_CREAT | IPC_EXCL | SHM_PERM);
		if (id == -1)
		{
			id = shmget(memkey, size_, SHM_PERM);
		}
		else
		{
			callInit = true;
		}
	}


	if (id == -1)
	{
		throw SharedMemoryException(StringConversion::syserr());	
	}
	else
	{
		struct shmid_ds shminfo;
		shmid = id;
		boost::wformat shmidmsg(L"Shared memory shmid %d");
		shmidmsg % shmid;
		TRACE(TraceData::info, shmidmsg.str());
		name = name_;
		size = size_;
		address = shmat(id, 0, 0);
		if (address == 0)
		{
			std::wstring errmsg = StringConversion::syserr();
			if (shmctl(shmid, IPC_STAT, &shminfo) != -1)
			{
				if (shminfo.shm_nattch < 1)
				{
					shmctl(shmid, IPC_RMID, NULL);
				}
			}

			throw SharedMemoryException(errmsg);

		}
		else
		{
			boost::wformat fmt(L"Shared memory for key: %ld mapped to address %lx");
			fmt % name_ % (syg_ulong_ptr) address;
			TRACE(TraceData::info, fmt.str());
			if (callInit)
			{
				TRACE(TraceData::info, L"initializing shared memory block");
				init();
			}
		}
	}
}

SharedMemory::~SharedMemory()
{
	struct shmid_ds shminfo;

	if (shmid != -1)
	{
		shmdt(address);

		if (shmctl(shmid, IPC_STAT, &shminfo) != -1)
		{
			if (shminfo.shm_nattch < 1)
			{
				shmctl(shmid, IPC_RMID, NULL);
			}
		}
		
	}
}

size_t SharedMemory::getSize() const
{
	return size;
}

SharedMemory::key SharedMemory::getName() const
{
	return name;
}

void *SharedMemory::getAddress() const
{
	return address;
}

void SharedMemory::lock()
{
	sem.wait();
}

void SharedMemory::unlock()
{
	sem.post(1);
}

void SharedMemory::init()
{
	memset(address, '\0', size);
}

bool SharedMemory::willCleanup()
{
	return sem.willCleanup();
}

}

