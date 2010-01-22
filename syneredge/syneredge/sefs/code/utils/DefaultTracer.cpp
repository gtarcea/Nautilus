#include "DefaultTracer.hpp"
#include "StringConversion.hpp"

#include <iostream>

namespace SynerEdge
{

DefaultTracer::DefaultTracer() : traceAtOrAbove(TraceData::info)
{
	configure();

	// hookup TraceThread
	std::wcout << L"hooking receivedTrace" << std::endl;
	TraceThread::instance()->receivedTrace += 
	new ObserverDelegate<DefaultTracer, TraceData>
		(*this, &DefaultTracer::traceRoutine);

	// hooking StartupParameters
	std::wcout << L"hooking startup parameters" << std::endl;
	StartupParameters::instance()->parametersWereReparsed +=
	new ObserverDelegate<DefaultTracer, BaseEventArgs>
		(*this, &DefaultTracer::reparseEvent);

}

void DefaultTracer::configure()
{
	std::wstring level;

	if (StartupParameters::instance()->env_find(L"TraceLevel", level))
	{
		if (level == L"info")
		{
			traceAtOrAbove = TraceData::info;
		}
		else if (level == L"warning")
		{
			traceAtOrAbove = TraceData::warning;
		}
		else if (level == L"exception")
		{
			traceAtOrAbove = TraceData::exception;
		}
		else if (level == L"assert")
		{
			traceAtOrAbove = TraceData::assert;
		}	
	}

	if (StartupParameters::instance()->env_find(L"TraceSubsystems", subsystems))
	{
		subsystems = std::wstring(L",") + subsystems + std::wstring(L",");
	}

	if (StartupParameters::instance()->env_find(L"TraceDir", tracedir))
	{
		tracedir += L"trace.txt";

		tracestream.open(StringConversion::toUTF8(tracedir).c_str(), std::ios::app);
		if (! tracestream)
		{
			throw StartupParametersException(L"Could not open trace file");
		}
	}	
}

DefaultTracer::~DefaultTracer()
{
	if (tracedir.size() != 0)
	{
		// unhook from TraceThread
		std::wcout << L"unhooking receivedTrace" << std::endl;
		TraceThread::instance()->receivedTrace -= 
		new ObserverDelegate<DefaultTracer, TraceData>
			(*this, &DefaultTracer::traceRoutine);

		std::wcout << L"unhooking parametersWereReparsed" << std::endl;
		StartupParameters::instance()->parametersWereReparsed -=
		new ObserverDelegate<DefaultTracer, BaseEventArgs>
			(*this, &DefaultTracer::reparseEvent);

		tracestream.close();
	}
}

void DefaultTracer::traceRoutine(Observable *obs, const TraceData &data)
{
	// somebody could be reconfiguring, you need to wait...
	boost::mutex::scoped_lock lk(mtx, true);

	if (data.traceLevel >= traceAtOrAbove)
	{
		std::wstring subs = std::wstring(L",") + data.subsystem + std::wstring(L",");
		if (subsystems.find(subs, 0) != subsystems.npos)
		{
			tracestream << data << std::endl;
		}
	}
}

void DefaultTracer::reparseEvent(Observable *Obs, const BaseEventArgs &args)
{
	TRACE_SETUP(L"utils");

	// lock down before you reconfigure
	boost::mutex::scoped_lock lk(mtx, true);

	TRACE(TraceData::info, L"Reparse event received");

	if (tracestream) tracestream.close();
	configure();
}


}

