#ifndef SygSignalThread
#define SygSignalThread

#include "boost/utility.hpp"
#include "boost/thread.hpp"
#include "boost/thread/once.hpp"
#include "boost/thread/mutex.hpp"
#include "boost/thread/condition.hpp"

#include "SynerEdge.hpp"
#include "Observer.hpp"
#include <signal.h>
#include <map>

namespace SynerEdge
{

class SignalThreadException : public SynerEdgeException
{
public:
	SignalThreadException(const std::wstring &msg)
	: SynerEdgeException(msg)
	{}

	virtual ~SignalThreadException() {}
};

class SignalThread : private boost::noncopyable, public Observable
{
public:
	typedef std::map<int, SignalThread *>::iterator SignalsIterator;
	~SignalThread();

	static SignalThread *instance(int mySignal, bool extraSigaction=false);
	static void blockSignals(const std::list<int> &signals);
	static void unblockSignals(const std::list<int> &signals);

	ObservableEvent<int> raisedSignal;

	void requestStop();

	static SignalsIterator begin();
	static SignalsIterator end();
private:
	class BoostThread;
	
	boost::thread *signalThread;
	boost::mutex mtx;
	boost::condition cnd;
	bool stopRequested;
	int mySignal;
	pthread_t threadId;
	bool extraSigaction;

	// singleton semantics
	SignalThread(int signal, bool extraSigaction);

	// static map of Signal threads and mutex to protect it
	static std::map<int, SignalThread *> _instances;
	static boost::mutex _mtx;

	// noncopyable semantics
	SignalThread(const SignalThread &);
	SignalThread &operator=(const SignalThread &);
};

}

#endif

