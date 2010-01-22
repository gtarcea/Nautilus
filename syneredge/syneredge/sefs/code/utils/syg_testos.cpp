#include <iostream>
#include <queue>
#include <sstream>

#include "boost/thread/thread.hpp"
#include "boost/thread/xtime.hpp"
#include "boost/format.hpp"

#include "Semaphore.hpp"
#include "SemaQueue.hpp"
#include "TraceThread.hpp"
#include "SignalThread.hpp"
#include "GlobalSemaphore.hpp"
#include "SharedMemory.hpp"
#include "SharedMemoryPool.hpp"

#include <signal.h>

using namespace SynerEdge;

static Semaphore *sem = 0;
static SemaQueue<std::wstring> myque;

struct thrfunc
{
	void operator()() 
	{ 
		for (int i = 0; i < 10; i++)
		{
			std::wcout << L"in thread" << std::endl; 
			boost::xtime xt;
			boost::xtime_get(&xt, boost::TIME_UTC);
			xt.sec += i;
			boost::thread::sleep(xt); 
			
			std::wcout << L"about to post" << std::endl;
			sem->post(1);
		}
	}

};

struct thrfunc2
{
	void operator()()
	{
		for (int i = 0; i < 10; i++)
		{
			std::wcout << L"in thread2" << std::endl;
			std::wcout << L"waiting on semaphore" << std::endl;
			sem->wait();
			std::wcout << L"semaphore wait done" << std::endl;
		}	
	}
};

struct thrfunc3
{
	void operator()()
	{
		for (int i = 0; i < 10; i++)
		{
			boost::xtime xt;
			boost::xtime_get(&xt, boost::TIME_UTC);
			xt.sec += 1;
			boost::thread::sleep(xt); 

			std::wstringstream wss;
			wss << i;
			std::wstring str = wss.str();
			std::wcout << L"pushing: " << str << std::endl;
			myque.pushAndNotify(str);
		}	
	}
};

struct thrfunc4
{
	void operator()()
	{
		std::wstring str;
		while (str != L"9")
		{
			str = myque.waitAndPop();
			std::wcout << "top of queue: " << str << std::endl;
		}
	}
};

class TraceCallback
{
public:
	void tracer(Observable *obs, const TraceData &td)
	{
		std::wcout << "in tracer: " << td.msg << std::endl;
	}
};

class SignalCallback
{
public:
	void signalReceived(Observable *obs, const int &signal)
	{
		std::wcout << L"in SignalCallback: " << signal << std::endl;
		if (signal == SIGHUP)
		{
			std::wcout << L"matches" << std::endl;
		}
	}
	void signalReceived2(Observable *obs, const int &signal)
	{
		std::wcout << L"in SignalCallback2: " << signal << std::endl;
		if (signal == SIGHUP)
		{
			std::wcout << L"matches" << std::endl;
		}
	}
};

static void doNothing(int sig)
{
	std::wcout << L"in doNothing: " << sig << std::endl;
}

int main(int argc, char ** argv)
{
	std::wstring str;

	sem = new Semaphore(0);

	/*
	thrfunc3 f1;
	boost::thread thr(f1);
	thrfunc4 f2;
	boost::thread thr2(f2);

	thr.join();
	thr2.join();
	*/

	TraceCallback tc;

	TRACE_SETUP(L"utils");

	TraceThread::instance()->receivedTrace += new ObserverDelegate<TraceCallback, TraceData>(tc, &TraceCallback::tracer);

	TRACE(TraceData::info, L"My message");

	/*
	while (std::wcin >> msg)
	{
		TRACE(TraceData::info, msg);
	}
	*/

	/*
	std::list<int> siglist;
	siglist.push_back(SIGUSR1);
	siglist.push_back(SIGUSR2);

	SignalThread::blockSignals(siglist);
	SignalCallback sc;

	SignalThread::instance(SIGUSR1)->raisedSignal += new ObserverDelegate<SignalCallback, int>(sc, &SignalCallback::signalReceived);

	SignalThread::instance(SIGUSR2)->raisedSignal += new ObserverDelegate<SignalCallback, int>(sc, &SignalCallback::signalReceived2);

	// pause here..
	std::wstring str;
	std::wcin >> str;

	SignalThread::instance(SIGUSR1)->raisedSignal -= new ObserverDelegate<SignalCallback, int>(sc, &SignalCallback::signalReceived);

	SignalThread::instance(SIGUSR2)->raisedSignal -= new ObserverDelegate<SignalCallback, int>(sc, &SignalCallback::signalReceived2);
	*/

	/*
	{
		GlobalSemaphore gs(0, L"beeblebrox", GlobalSemaphore::iffirst);

		if (argc > 1)
		{
			std::wcout << L"posting" << std::endl;
			gs.post(1);
		}
		else
		{
			std::wcout << L"waiting" << std::endl;
			gs.wait();
		}
	

	}
	

	std::wcin >> str;

	TraceThread::instance()->haltIfRunning();
	*/

	/*
	SharedMemory mem(2048 * 1024, 327);
	if (argc > 1)
	{
		mem.lock();	
		int *i = static_cast<int *>(mem.getAddress());
		*i = 27;
		mem.unlock();
	}
	else
	{
		mem.lock();
		int *i = static_cast<int *>(mem.getAddress());
		int iout = *i;
		mem.unlock();

		boost::wformat fmt(L"iout was: %d");
		fmt % iout;
		TRACE(TraceData::info, fmt.str());
	}
	*/
	
	GlobalSemaphore::Cleanup cleanup;
	if (argc == 1)
	{
		cleanup = GlobalSemaphore::never;
	}
	else
	{
		cleanup = GlobalSemaphore::always;
	}
	try
	{
	SharedMemoryPool smp(1024 * 2048, 327, cleanup);

	TRACE(TraceData::info, L"before memalloc");

	void *addr = smp.memalloc(1024);
	smp.dumpFreeList();

	void *addr2 = smp.memalloc(512);
	smp.dumpFreeList();

	void *addr3 = smp.memalloc(256);
	smp.dumpFreeList();

	smp.memfree(addr2);
	smp.dumpFreeList();
	smp.memfree(addr);
	smp.dumpFreeList();
	smp.memfree(addr3);
	smp.dumpFreeList();
	}
	catch (SynerEdgeException &e)
	{
		std::wcout << e << std::endl;
	}
	catch (...)
	{
		std::wcout << L"caught something..." << std::endl;
	}

	std::wcin >> str;
	TRACE(TraceData::info, L"at end");

	return 0;
}
