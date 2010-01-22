#ifndef SynerEdge_Threads_hpp
#define SynerEdge_Threads_hpp

#include "utils/syneredge.hpp"
#include <map>

namespace Syneredge
{

#ifdef _WIN32
#define stdcall __stdcall
typedef syn_ulong_ptr ThreadHandlerData;
#else
#include "pthread.h"
#define stdcall
typedef int ThreadHandlerData;
#endif

typedef void (stdcall *ThreadSignalFunction)(ThreadHandlerData sig);

class Thread
{
public:
	Thread(bool joinable);
	~Thread();

	bool isStopRequested();
	void requestStop();
	void *getData();
	void setData(void *data);
	syn_ulong_ptr getId();
	void signal(int signo);
	bool isCancelable();
	void setCancelable();

	void cancel();
	void join();

	static Thread *currentThread();

	static void signalBlock(int signo);
	static void signalUnblock(int signo);
	static void signalHandler(int signo, ThreadSignalFunction fn);

protected:
	static void stdcall threadStaticFunction(Thread *thr);

private:
        bool detached;
	void *data;
	static std::map<syn_ulong_ptr, Thread *> threadMap;
};
};

#endif
