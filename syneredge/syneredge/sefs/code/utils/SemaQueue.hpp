#ifndef SynerEdge_SemaQueue_hpp
#define SynerEdge_SemaQueue_hpp

#include <queue>
#include "boost/utility.hpp"
#include "boost/thread.hpp"
#include "boost/thread/mutex.hpp"
#include "boost/thread/condition.hpp"

template< class Tp > 
class SemaQueue : private boost::noncopyable
{
public:
	SemaQueue() {}

	void pushAndNotify(const Tp &x)
	{
		boost::mutex::scoped_lock lk(mtx, true);			
		que.push(x);
		cnd.notify_one();	
	}

	Tp waitAndPop()
	{
		boost::mutex::scoped_lock lk(mtx, true);
		while (que.size() == 0)
			cnd.wait(lk);

		Tp result(que.front());
		que.pop();	
		return result;
	}

	size_t currentSize()
	{
		boost::mutex::scoped_lock lk(mtx, true);
		return que.size();
	}

private:
	std::queue<Tp> que;
	boost::condition cnd;
	boost::mutex mtx;

	// non-copyable
	SemaQueue(const SemaQueue &);
	SemaQueue &operator=(const SemaQueue &);
};

#endif
