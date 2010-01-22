#ifndef SynerEdge_Semaphore_hpp
#define SynerEdge_Semaphore_hpp

#include "boost/utility.hpp"
#include "boost/thread.hpp"
#include "boost/thread/mutex.hpp"
#include "boost/thread/condition.hpp"

namespace SynerEdge
{

class Semaphore : private boost::noncopyable
{
public:
	Semaphore(size_t count_) : count(count_) {}
	~Semaphore() {}

	void post(size_t postnum);
	void wait();
	bool operator()();

private:
	boost::mutex mtx;
	boost::condition cnd;
	size_t count;

	// non-copyable requirement
	Semaphore(const Semaphore &);
	Semaphore &operator=(const Semaphore &);
};

};

#endif
