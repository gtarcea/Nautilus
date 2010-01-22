#ifndef SynerEdge_Timer_h_
#define SynerEdge_Timer_h_

#include "SynerEdge.hpp"
#include "Observer.hpp"
#include "DateTime.hpp"

#include "boost/utility.hpp"
#include "boost/thread/thread.hpp"
#include "boost/thread/mutex.hpp"
#include "boost/thread/condition.hpp"

namespace SynerEdge
{

class TimerException : public SynerEdgeException
{
public:
	TimerException(const std::wstring &msg)
	: SynerEdgeException(msg)
	{}

	virtual ~TimerException() {}

	virtual std::wstring getMsg() const
	{
		return SynerEdgeException::getMsg();
	}
};

class Timer : private boost::noncopyable, public Observable
{
public:
	struct InnerData;
	Timer(unsigned long millis, bool repeat);
	~Timer();

	ObservableEvent<DateTime> timerExpired;

	void start();
	void reset(unsigned long millis, bool repeat);

	static void sleep(unsigned long millis);

private:
	InnerData &innerData;

	// non-copyable semantics
	Timer(const Timer &copy);
	Timer &operator=(const Timer &equal);

};

}

#endif

