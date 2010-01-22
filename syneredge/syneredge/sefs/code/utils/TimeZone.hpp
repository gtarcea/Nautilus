#ifndef SygTimeZone_hpp__
#define SygTimeZone_hpp__

#include <string>
#include <iostream>
#include "boost/format.hpp"

namespace SynerEdge 
{

struct  TimeZone
{
public:
	TimeZone() 
        : standardName(), daylightName(), useDst(false), secondsWestOfUTC(0)
        {}

	TimeZone(const std::wstring &standardName_, 
		 const std::wstring &daylightName_,
 		 bool useDst_, 
		 long secondsWestOfUTC_);
 
	bool operator==(const TimeZone &tz) const;

	// default copy constructor, destructor, and operator= overload are OK.

	std::wstring getName(bool isDst) const;
	long getSecondsToAddToGetUTC(bool isDst) const;

	std::wstring standardName;
	std::wstring daylightName;
	bool useDst;
	long secondsWestOfUTC;	
};


class TimeZoneFactory
{
public:
	static TimeZoneFactory *instance();

	TimeZone getUTC();
	TimeZone getLocal();

private:

	TimeZoneFactory();
	TimeZoneFactory(const TimeZoneFactory &);
	~TimeZoneFactory() {}

	TimeZoneFactory &operator=(const TimeZoneFactory &);
	
	static TimeZoneFactory *_tz;
};

}; // namespace SynerEdge

#endif

