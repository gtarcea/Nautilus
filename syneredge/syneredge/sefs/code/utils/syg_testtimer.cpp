#include "Timer.hpp"
#include "SignalThread.hpp"

#include <signal.h>
#include <iostream>

using namespace SynerEdge;

class cb 
{
public:
	void callback(Observable *o, const DateTime &now)
	{
		DateTimeFields fields;
		now.toFields(fields, TimeZoneFactory::instance()->getLocal());
		std::wcout << "in callback of timer: "
			<< fields.toString() << std::endl;
	}
};

int main(int argc, char** argv)
{

	std::list<int> siglist;
	siglist.push_back(SIGALRM);
	SignalThread::blockSignals(siglist);
	
	Timer tmr1(5000, false);
	Timer tmr2(2000, false);
	cb cb1;
	tmr1.timerExpired += new ObserverDelegate<cb, DateTime>(cb1, &cb::callback);
	tmr2.timerExpired += new ObserverDelegate<cb, DateTime>(cb1, &cb::callback);
	DateTime now;
	DateTimeFields fields;
	now.toFields(fields, TimeZoneFactory::instance()->getLocal());
	std::wcout << "start: " << fields.toString() << std::endl;

	tmr1.start();
	tmr2.start();

	Timer::sleep(10000);

	DateTime then;
	then.toFields(fields, TimeZoneFactory::instance()->getLocal());
	std::wcout << "start: " << fields.toString() << std::endl;

	return 0;
}
