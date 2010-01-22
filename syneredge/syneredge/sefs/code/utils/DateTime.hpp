#ifndef SygDateTime_hpp__
#define SygDateTime_hpp__

#include <string>
#include <iostream>
#include "boost/format.hpp"
#include "TimeZone.hpp"
#include "SynerEdge.hpp"

namespace SynerEdge 
{

class DateException : public SynerEdgeException
{
public:
	DateException(std::wstring msg) : SynerEdgeException(msg) {}
	~DateException() {}
};


class DateTimeFields {
public:
	// This enum is hopelessly english...
	// Internationalization would require the output routine
	// to convert the DateTimeWeekday to a reasonable string.
	enum Weekday 
	{ NoWeekday=-1, Sunday, Monday, Tuesday, Wednesday, 
          Thursday, Friday, Saturday };

	enum Month
	{ NoMonth=-1, January, February, March, April, May, June,
	  July, August, September, October, November, December };

	int year;
	Month month;
	int day;
	Weekday weekday;
	int hour;
	int minute;
	int second;
	int millisecond;
	TimeZone timezone;
	bool isDst;

	std::wstring toString()
	{
		boost::wformat fmt(L"%04d-%02d-%02d %02d:%02d:%02d.%03d");
		fmt % year % (int) (month + 1) % day % hour % minute % 
		      second % millisecond;

		return fmt.str();
	}

	DateTimeFields();
	DateTimeFields(int year_, Month month_, int day_, Weekday weekday_, 
		       int hour_, int minute_, int second_, 
		       int millisecond_, const TimeZone &timezone_);
	DateTimeFields(int year_, Month month_, int day_,  
		       int hour_, int minute_, int second_, 
		       int millisecond_);
private:
	bool calculateDstFromFields(const DateTimeFields &fields);
};

std::wostream &operator<<(std::wostream &out, enum DateTimeFields::Weekday dtw);

class DateTime 
{
public:
	static const int HOURSPERDAY = 24;
	static const int MINSPERDAY = 24 * 60;
	static const int SECSPERDAY = 24 * 60 * 60;
	static const int MILLISECSPERDAY = 24 * 60 * 60 * 1000;
	static const int MILLISECSPERHOUR = 60 * 60 * 1000;
	static const int MILLISECSPERMINUTE = 60 * 1000;
	static const int MILLISECSPERSECOND = 1000;
	static const double EPSILON = 1 / (2 * MILLISECSPERDAY);
	
	// constructors
	DateTime();
	DateTime(struct tm &tmin, const TimeZone &tz);
	DateTime(time_t &tmin);
	DateTime(struct timeval &tv);
	DateTime(int year, enum DateTimeFields::Month month, int day, 
		 int millis, const TimeZone &tz);
	DateTime(const DateTimeFields &dtf);
	DateTime(const double val);
	DateTime(const DateTime &copy);

	// operator= overloads (assigning a value)
	DateTime &operator=(const DateTime &equal);

	// operator== overload (compare two timestamps for equality)
	bool operator==(const DateTime &compare) const;

	// arithmetic overloads - time period manipulation
	// supports: Date += time_period, Date = Date + time_period,
	//           Date -= time_period, Date = Date - time_period,
	//	     time_period = Date - Date
	DateTime &operator+=(double plus);
	DateTime &operator-=(double minus);
	DateTime operator+(double addto) const;
	DateTime operator-(double subfrom) const;
	double operator-(const DateTime &subfrom) const;

	// output conversions
	void toUTC(struct tm &tmutc) const;
	void toLocal(struct tm &tmlocal) const;
	void toTimeval(struct timeval &timevalout) const;
	time_t toTime() const;
	double toDouble() const;
	std::wstring toString(const TimeZone &tz, 
			      const std::wstring &format, 
                              size_t maxsize) const;
	void toFields(DateTimeFields &fields, const TimeZone &tz) const;

	// is date/time in a given range?
	bool isInRange(const DateTime &compare, double range) const;

	// debug dump
	std::wostream &dump(std::wostream &out) const;

	static double makeRange(int days, int hours, int minutes,
				int seconds, int millis);
	static double makeRange(int days, int millis);
	static double makeRange(int millis);

private:
	// This DateTime class uses astronomical julian dates.
	double val;

	static const double julian10151582;

	static double from_Tm_UTC(struct tm &utctm);
	static double from_Tm_Local(struct tm &tmlocal);
	static double from_Time(time_t &timein);
	static double from_Timeval(struct timeval &tv);
	static double from_Fields(const DateTimeFields &fields);
	static double from_current();

	static int DateTime::month_fix(int month);
	static int DateTime::year_fix(int month, int year);
	static int DateTime::year_cent(int month, int year);
	static int DateTime::isCalculatedGregorian(int year, int month, double day);
	static double DateTime::make_julian(int month, int year, 
				     double days, int isGregorian);
	static double DateTime::julian_fix(double julian);
	static int DateTime::julian_z(double julian);
	static double DateTime::julian_f(double julian);
	static int DateTime::julian_alpha(double z);
	static int DateTime::julian_a(int z, int isGregorian);
	static int DateTime::julian_b(int a);
	static int DateTime::julian_c(int b);
	static int DateTime::julian_d(int c);
	static int DateTime::julian_e(double julian, int b, int d, int isGregorian);
	static double DateTime::julian_to_calendar_day(double julian, int isGregorian);
	static int DateTime::julian_to_calendar_month(double julian, int isGregorian);
	static int DateTime::julian_to_calendar_year(double julian, int isGregorian);
	static time_t DateTime::mktime_from_utc (struct tm *t);
	static int DateTime::leaps(int month, int year, double day, int isGregorian);
	static double DateTime::utc_from_YMD_ms(int year, enum DateTimeFields::Month month, int day, int millis);
	static double DateTime::local_from_YMD_ms(int year, enum DateTimeFields::Month month, int day, int millis);

};

};

#endif

