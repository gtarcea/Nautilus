#ifndef SygDefaultTracer_hpp
#define SygDefaultTracer_hpp

#include "Observer.hpp"
#include "TraceThread.hpp"
#include "StartupParameters.hpp"

#include "boost/thread.hpp"
#include "boost/thread/mutex.hpp"

namespace SynerEdge
{

class DefaultTracer
{
public:
	DefaultTracer();
	~DefaultTracer();

	void traceRoutine(Observable *obs, const TraceData &td);
	void reparseEvent(Observable *obs, const BaseEventArgs &args);

private:
	std::wstring tracedir;
	enum TraceData::TraceLevel traceAtOrAbove;
	std::wstring subsystems;
	boost::mutex mtx;

	std::wofstream tracestream;

	void configure();
};

}

#endif
