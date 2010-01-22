#include "TraceThread.hpp"
#include <iostream>
#include "DateTime.hpp"
#include "StringConversion.hpp"

namespace SynerEdge
{

boost::once_flag TraceThread::_sentry = BOOST_ONCE_INIT;
boost::mutex TraceThread::_mtx;

TraceThread *TraceThread::_instance = 0;

TraceThread::TraceThread() 
: receivedTrace(this), receivedLog(this), stopRequested(false), tracingThread(0)
{
}

TraceThread::~TraceThread()
{
	delete tracingThread;
}

TraceThread *TraceThread::instance()
{
	boost::call_once(&TraceThread::createInstance, _sentry);
	return _instance;
}

void TraceThread::createInstance()
{
	_instance = new TraceThread();
	_instance->tracingThread = new boost::thread(BoostThread(_instance));
}

TraceThread::BoostThread::BoostThread(TraceThread *instance) 
: _instance(instance)
{
}

void TraceThread::BoostThread::operator()()
{
	while (! _instance->stopRequested)
	{
		_instance->receivedLog(_instance->traceQue.waitAndPop());
	}
}

void TraceThread::requestStop()
{
	TRACE_SETUP(L"utils");
	stopRequested = true;
	TRACE(TraceData::info, L"TraceThread has been requested to stop!");
}

void TraceThread::haltIfRunning()
{
	boost::mutex::scoped_lock lk(_mtx, true);

	if (_instance != 0)
	{
		_instance->requestStop();
		_instance->tracingThread->join();
		delete _instance;
		_instance = 0;
	}
}

void TraceThread::trace(TraceData &td)
{
	td.threadId = static_cast<syg_ulong_ptr>(pthread_self());
	TraceData trx(td);
	instance()->receivedTrace(td);
	traceQue.pushAndNotify(td);
}

TraceData::TraceData(const std::wstring &subsystem_)
: subsystem(subsystem_), traceLevel(info), msg(),
  filename(), lineNumber(0), module(), threadId(0)
{
}

TraceData::TraceData(
		const std::wstring &subsystem_, 
		enum TraceLevel traceLevel_, 
		const std::wstring &msg_,
		const std::string &filename_,
		long lineNumber_,
		const std::string &module_,
		syg_ulong_ptr threadId_)
: subsystem(subsystem_), traceLevel(traceLevel_), msg(msg_),
  filename(StringConversion::toUTF16(filename_)),
  lineNumber(lineNumber_),
  module(StringConversion::toUTF16(module_)),
  threadId(threadId_)
{
}

std::wostream &operator<<(std::wostream &out, const TraceData &in)
{
	DateTime dt;
	DateTimeFields dtf;
	TimeZone tz = TimeZoneFactory::instance()->getLocal();

	dt.toFields(dtf, tz);

	boost::wformat fmt(L"{%ld} %s.%03d %s(%d) in [%s] - %s");
	fmt % in.threadId
	    % dt.toString(tz, L"%F %H:%M:%S", 1024)
	    % dtf.millisecond
	    % in.filename % in.lineNumber % in.module % in.msg;
	out << fmt;
	return out;
}

}
