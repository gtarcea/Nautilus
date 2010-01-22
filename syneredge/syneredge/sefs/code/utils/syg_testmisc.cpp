#include "DateTime.hpp"
#include "Observer.hpp"

using namespace SynerEdge;

class EventArgs
{
public: 
	EventArgs() {}
};

class NewEventArgs 
{
public:
	NewEventArgs() {}
};

class X
{
public:
    void callback(Observable *obs, const EventArgs &args)
    {
        std::wcout << L"got callback in X" << std::endl;
    }

    void func(Observable *obs, const EventArgs &args)
    {
	std::wcout << L"got func in X" << std::endl;
    }

    void cb3(Observable *obs, const NewEventArgs &args)
    {
    }
};

class Y
{
public:
    void func(Observable *obs, const EventArgs &args)
    {
        std::wcout << L"got callback in Y" << std::endl;
    }
};

class Observed : public Observable
{
public:
	Observed() : myevent(this) {}

	virtual void testvirt()
	{
		std::wcout << "in testvirt" << std::endl;
	}

	ObservableEvent<EventArgs> myevent;

	void fire_event()
	{
		EventArgs a;
		myevent(a);
	}
};

void do_Callback_test()
{
    X x;
    Y y;
    Observed observed;

    //observed.myevent += ObserverDelegate<X, NewEventArgs>(x, &X::cb3);

    std::wcout << L"add delegate" << std::endl;
    observed.myevent += new ObserverDelegate<X, EventArgs>(x, &X::callback);
    observed.myevent += new ObserverDelegate<X, EventArgs>(x, &X::func);
    observed.myevent += new ObserverDelegate<Y, EventArgs>(y, &Y::func);
    std::wcout << L"fire all events" << std::endl;
    observed.fire_event();
    std::wcout << L"remove delegate" << std::endl;
    observed.myevent -= new ObserverDelegate<X, EventArgs>(x, &X::callback);
    std::wcout << L"fire all events" << std::endl;
    observed.fire_event();
    std::wcout << L"remove delegate" << std::endl;
    observed.myevent -= new ObserverDelegate<Y, EventArgs>(y, &Y::func);
    std::wcout << L"fire all events" << std::endl;
    observed.fire_event();
}


int main(int argc, char **argv)
{
	TimeZone tz = TimeZoneFactory::instance()->getLocal();

	std::wcout << tz.getName(false) << std::endl;
	std::wcout << tz.secondsWestOfUTC << std::endl;
	std::wcout << tz.secondsWestOfUTC / 3600 << std::endl;
	std::wcout << "useDst: " << tz.useDst << std::endl;
	std::wcout << tz.getSecondsToAddToGetUTC(false) << std::endl;

	try
	{
	DateTime dt;
	std::wcout << dt.toString(TimeZoneFactory::instance()->getUTC(),
				  L"%x %X %Z", 200) << std::endl;

	TimeZone local = TimeZoneFactory::instance()->getLocal();
	std::wcout << L"constructing datetime" << std::endl;
	DateTime dt2(1984, DateTimeFields::July, 18, 0, local);
	std::wcout << L"toFields" << std::endl;
	DateTimeFields dtf;
	dt2.toFields(dtf, local);

	std::wcout << L"tostring" << std::endl;
	std::wcout << dtf.toString() << std::endl;
	}
	catch (DateException &de)
	{
		std::wcout << L"error: " << de.getMsg() << std::endl;
	}

	do_Callback_test();

	return 0;
}
