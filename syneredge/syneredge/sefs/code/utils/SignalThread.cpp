#include "SignalThread.hpp"
#include "StringConversion.hpp"
#include "DateTime.hpp"

#include <iostream>
#include <signal.h>
#include <pthread.h>
#include <errno.h>

#include "boost/format.hpp"
#include "TraceThread.hpp"

namespace SynerEdge
{

class SignalThread::BoostThread
{
public:
	BoostThread(SignalThread *instance);

	void operator()();

	static void signalHandler(int sig, siginfo_t *extra, void *arg)
	{
		ObservableEvent<DateTime> *event = 
			reinterpret_cast< ObservableEvent<DateTime> *>
			(extra->si_value.sival_ptr);	
		if (event)
		{
			DateTime now;
			(*event)(now);
		}
	}
private:
	SignalThread *_instance;
};

boost::mutex SignalThread::_mtx;
std::map<int, SignalThread *> SignalThread::_instances;

SignalThread::SignalThread(int signo, bool extraSigaction_) 
: stopRequested(false), raisedSignal(this), mySignal(signo), 
  signalThread(0), threadId(0), extraSigaction(extraSigaction_)
{
}

SignalThread::~SignalThread()
{
	delete signalThread;
}

SignalThread *SignalThread::instance(int signo, bool extraSigaction)
{
	TRACE_SETUP(L"utils");

	SignalThread *result = 0;
	boost::mutex::scoped_lock lk(_mtx, true);
	SignalsIterator itor = _instances.find(signo);
	if (itor != _instances.end())
	{
		result = ((*itor).second);
		if (result->extraSigaction != extraSigaction)
		{
			throw new SignalThreadException(L"Signal thread matching signal was found, but it does not match extraSigaction parameter");
		}
	}
	else
	{
		boost::wformat fmt(L"Creating trace thread for signal: %d");
		fmt % signo;
		TRACE(TraceData::info, fmt.str());

		result = new SignalThread(signo, extraSigaction);
		_instances[signo] = result;
		result->signalThread = new boost::thread(BoostThread(result));

		boost::mutex::scoped_lock lk(result->mtx, true);
		while (result->threadId == 0)
		{
			(result->cnd).wait(lk);
		}
	}
	return result;
}

void SignalThread::requestStop()
{
	stopRequested = true;
}

SignalThread::BoostThread::BoostThread(SignalThread *instance)
: _instance(instance)
{
}

void SignalThread::BoostThread::operator()()
{
	TRACE_SETUP(L"utils");

	// set thread id, and notify parent that it is OK to
	// continue.  This way, we know that the thread id
	// stored in the thread object is valid.  This is at the
	// expense of waiting indefinitely for this thread to
	// get a CPU slice.
	_instance->threadId = pthread_self();
	_instance->cnd.notify_one();

	boost::wformat fmt(L"Starting signal thread, listening for signal: %d");
	fmt % _instance->mySignal;

	TRACE(TraceData::info, fmt.str());

	if (! _instance->extraSigaction)
	{
		sigset_t s;
		int sig = _instance->mySignal;

		sigemptyset(&s);
		sigaddset(&s, sig);

		while (! _instance->stopRequested)
		{
			sigwait(&s, &sig);
			if (sig == _instance->mySignal)
			{
				_instance->raisedSignal(sig);
			}
			else
			{
				if (! _instance->stopRequested)
				{
					boost::wformat fmt(L"Got spurious wakeup in signal thread while listening for signal: %d");
					fmt % _instance->mySignal;

					TRACE(TraceData::info, fmt.str());

					boost::xtime xt;
					boost::xtime_get(&xt, boost::TIME_UTC);
					xt.sec += 1;
					boost::thread::sleep(xt);
				}
			}
		}	
	}
	else
	{
		_instance->threadId = pthread_self();
		_instance->cnd.notify_one();

		std::list<int> signalList;
		signalList.push_back(SIGALRM);
		SignalThread::unblockSignals(signalList);

		struct sigaction sig_action;
		sigemptyset(&sig_action.sa_mask);
		sig_action.sa_flags = SA_SIGINFO;
		sig_action.sa_sigaction = 
			SignalThread::BoostThread::signalHandler;
		sigaction(SIGALRM, &sig_action, NULL);

		while (! _instance->stopRequested)
		{
        		pause();
		}
	}
}
	

void SignalThread::blockSignals(const std::list<int> &signals)
{
	sigset_t s;
	sigemptyset(&s);

	std::list<int>::const_iterator itor;
	for (itor = signals.begin();
	     itor != signals.end();
	     itor++)
	{
		sigaddset(&s, (*itor));
	}

	pthread_sigmask(SIG_BLOCK, &s, NULL);
}

void SignalThread::unblockSignals(const std::list<int> &signals)
{
	sigset_t s;
	sigemptyset(&s);

	std::list<int>::const_iterator itor;
	for (itor = signals.begin();
	     itor != signals.end();
	     itor++)
	{
		sigaddset(&s, (*itor));
	}

	pthread_sigmask(SIG_UNBLOCK, &s, NULL);
}

}

