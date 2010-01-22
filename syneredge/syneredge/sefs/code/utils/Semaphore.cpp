#include "Semaphore.hpp"
#include <iostream>

namespace SynerEdge
{

void Semaphore::post(size_t postnum)
{
	boost::mutex::scoped_lock lk(mtx, true);
	count += postnum;
	cnd.notify_one();
}

void Semaphore::wait()
{
	boost::mutex::scoped_lock lk(mtx, true);
	while (count == 0)
		cnd.wait(lk);
	count--;
}

bool Semaphore::operator()()
{
	return (count > 0);
}

};
