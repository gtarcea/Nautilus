#include "DateTime.hpp"
#include <sys/time.h>
#include <time.h>
#include "StringConversion.hpp"
#include "boost/format.hpp"
#include <cerrno>
#include <cassert>

namespace SynerEdge
{

const double DateTime::julian10151582 = 2299161;

bool DateTimeFields::calculateDstFromFields(const DateTimeFields &fields)
{
	bool result = false;

	struct tm tm1;
	tm1.tm_sec = fields.second;
	tm1.tm_min = fields.minute;
	tm1.tm_hour = fields.hour;
	tm1.tm_mon = static_cast<int>(fields.month);
	tm1.tm_yday = fields.day;
	tm1.tm_year = fields.year;
	tm1.tm_isdst = -1;
	time_t t1 = mktime(&tm1);

	struct tm *tmptr = localtime_r(&t1, &tm1); 
		
	if (! tmptr)
	{
		result = (tmptr->tm_isdst != 0);
	}
	return result;	
}

DateTimeFields::DateTimeFields(
	int year_, Month month_, int day_, int hour_, int minute_,
	int second_, int millisecond_)
: year(year_), month(month_), day(day_), hour(hour_), minute(minute_),
  second(second_), millisecond(millisecond_)
{
	timezone = TimeZoneFactory::instance()->getLocal();
	isDst = calculateDstFromFields(*this);
}

DateTimeFields::DateTimeFields(int year_, Month month_, int day_, 
	Weekday weekday_, int hour_, int minute_, int second_, 
	int millisecond_, const TimeZone &timezone_)
: year(year_), month(month_), day(day_), weekday(weekday_), hour(hour_),
  minute(minute_), second(second_), millisecond(millisecond_),
  timezone(timezone_)
{
	isDst = calculateDstFromFields(*this);
}

DateTimeFields::DateTimeFields()
: year(0), month(NoMonth), day(0), weekday(NoWeekday), hour(0), 
  minute(0), second(0), millisecond(0), timezone(), isDst(false)
{
}


std::wostream &operator<<(std::wostream &out, enum DateTimeFields::Weekday dtw)
{
        switch(dtw)
        {
        case DateTimeFields::Sunday : out << "Sunday"; break;
        case DateTimeFields::Monday : out << "Monday"; break;
        case DateTimeFields::Tuesday : out << "Tuesday"; break;
        case DateTimeFields::Wednesday : out << "Wednesday"; break;
        case DateTimeFields::Thursday : out << "Thursday"; break;
        case DateTimeFields::Friday : out << "Friday"; break;
        case DateTimeFields::Saturday : out << "Saturday"; break;
        case DateTimeFields::NoWeekday : out << "NoWeekday"; break;
        }
        return out;
}

DateTime::DateTime()
{
	val = from_current();
}

DateTime::DateTime(struct tm &tmin, const TimeZone &tz)
{
	if (tz == TimeZoneFactory::instance()->getUTC())
	{
		val = from_Tm_UTC(tmin);
	}
	else
	{
		val = from_Tm_Local(tmin);
	}
}

DateTime::DateTime(time_t &timein)
{
	val = from_Time(timein);
}

DateTime::DateTime(struct timeval &tv)
{
	val = from_Timeval(tv);
}

DateTime::DateTime(int year, enum DateTimeFields::Month month, int day,
         	   int millis, const TimeZone &tz)
{
	if (tz == TimeZoneFactory::instance()->getUTC())
	{
		val = utc_from_YMD_ms(year, month, day, millis);
	}
	else
	{
		val = local_from_YMD_ms(year, month, day, millis);
	}
}

DateTime::DateTime(const DateTimeFields &dtf)
{
	val = from_Fields(dtf);
}

DateTime::DateTime(const double inval)
{
	val = inval;
}

DateTime::DateTime(const DateTime &copy)
{
	val = copy.val;
}

DateTime &DateTime::operator=(const DateTime &assign)
{
	val = assign.val;
	return *this;
}

DateTime &DateTime::operator+=(double addval)
{
	val += addval;
	return *this;
}

DateTime DateTime::operator+(double addval) const
{
	return DateTime(val + addval);
}

DateTime &DateTime::operator-=(double subval)
{
	val -= subval;
	return *this;
}

double DateTime::operator-(const DateTime &dt) const
{
	return (val - dt.val);
}

DateTime DateTime::operator-(double subval) const
{
	return DateTime(val - subval);
}

void DateTime::toUTC(struct tm &tmout) const
{
        double calday = 0;
        double calmillisecs = 0;
        int isGregorian = -1;

        tmout.tm_year = julian_to_calendar_year(val, isGregorian) - 1900;
	tmout.tm_mon = julian_to_calendar_month(val, isGregorian) - 1;
        calday = julian_to_calendar_day(val, isGregorian);
        tmout.tm_mday = (int) calday;
        calmillisecs = ((calday - tmout.tm_mday) * MILLISECSPERDAY) + 0.5;
        tmout.tm_hour = ((int) calmillisecs) / 3600000;
        tmout.tm_min = (int) ((((int) calmillisecs) % 3600000) / 60000);
        tmout.tm_sec = (((int) calmillisecs) % 60000) / 1000;
        tmout.tm_wday = (((int) julian_fix(val)) + 1) % 7;
        tmout.tm_isdst = 0;
}

void DateTime::toLocal(struct tm &tmout) const
{
        time_t systim;
        struct tm *tmptr = 0;

	toUTC(tmout);

        systim = mktime_from_utc(&tmout);
        if (systim >= 0) 
	{
#ifdef _WIN32
                tmptr = localtime(&systim);
                if (tmptr == 0)
		{
                	throw DateException(StringConversion::syserr());
		}
                else 
		{
			tmout = *tmptr;
		}
#else
                tmptr = localtime_r(&systim, &tmout);
		if (tmptr == 0)
		{
			throw DateException(StringConversion::syserr());
		}
#endif
        } 
	else 
	{
                throw DateException(StringConversion::syserr());
        }

}

time_t DateTime::toTime() const
{
        struct tm tmutc;
	time_t result;

        toLocal(tmutc);
        result = mktime(&tmutc);
        if (result < 0)
	{
		throw DateException(StringConversion::syserr());	
	}

        return result;
}

void DateTime::toTimeval(struct timeval &timevalout) const
{
        struct tm tmlocal;
        double calday = 0;
        double calmillisecs = 0;
        int isGregorian = 1;

	toLocal(tmlocal);
        timevalout.tv_sec = (long) mktime(&tmlocal);
        if (timevalout.tv_sec >= 0) 
	{
                calday = julian_to_calendar_day(val, isGregorian);
                calmillisecs = ((calday - ((int) calday)) * MILLISECSPERDAY) + 0.5;
                timevalout.tv_usec = (((int) calmillisecs) % 1000) * 1000;
        } 
	else 
	{
                throw DateException(StringConversion::syserr()); 
        }
}

double DateTime::toDouble() const
{
	return val;
}

std::wstring DateTime::toString(const TimeZone &tz, 
				const std::wstring &format,
				size_t maxsize) const
{
        struct tm tmstruct;
	std::wstring fmt = format;
        std::string fmtUTF8;

        char *str = new char[maxsize];


	if (tz == TimeZoneFactory::instance()->getUTC())
	{
        	toUTC(tmstruct);
	}
	else
	{
		toLocal(tmstruct);
	}

        // may need to replace %z with "Coordinated Universal Time" and
        // %Z with UTC.  Not available on all platforms???
	size_t zpos = fmt.find(L"%z", 0);
	if (zpos != fmt.npos)
	{
		fmt.replace(zpos, 2, tz.getName(tmstruct.tm_isdst != 0));
	}
	zpos = fmt.find(L"%Z", 0);
	if (zpos != fmt.npos)
	{
		fmt.replace(zpos, 2, tz.getName(tmstruct.tm_isdst != 0));
	}

        fmtUTF8 = StringConversion::toUTF8(fmt);

        size_t len = strftime(str, maxsize, fmtUTF8.c_str(), &tmstruct);
        std::string resultUTF8(str, len);
        delete[] str;

        std::wstring result = StringConversion::toUTF16(resultUTF8);
        return result;

}

void DateTime::toFields(DateTimeFields &fields, const TimeZone &tz) const
{
        struct tm tmstruct;
        double calday;
        double calmillisecs;
        int isGregorian = -1;

	if (tz == TimeZoneFactory::instance()->getUTC())
	{
		toUTC(tmstruct);
	}
	else
	{
		toLocal(tmstruct);
	}

        fields.year = tmstruct.tm_year + 1900;
        fields.month = (DateTimeFields::Month) tmstruct.tm_mon;
        fields.day = tmstruct.tm_mday;
        fields.weekday = (DateTimeFields::Weekday) tmstruct.tm_wday;
       	fields.hour = tmstruct.tm_hour;
        fields.minute = tmstruct.tm_min;
        fields.second = tmstruct.tm_sec;

        calday = julian_to_calendar_day(val, isGregorian);
        calmillisecs = (calday - ((int) calday)) * MILLISECSPERDAY + 0.5;
        fields.millisecond = ((int) calmillisecs) % 1000;
	fields.timezone = tz;
}

bool DateTime::isInRange(const DateTime &startDate, double range) const
{
	bool result = false;

	if (range > 0)
	{
		result = ((val > (startDate.val - EPSILON)) &&
	                  (val < (startDate.val + range + EPSILON)));

	}
	else
	{
		result = ((val > (startDate.val + range - EPSILON)) &&
			  (val < (startDate.val + EPSILON)));
	}
	return result;
}

bool DateTime::operator==(const DateTime &compare) const
{
	return isInRange(compare, 0);
}

std::wostream &DateTime::dump(std::wostream &out) const
{
	out << val;
	return out;
}

double DateTime::makeRange(int days, int hours, int minutes,
                           int seconds, int millis)
{
	double result = 0;

	result += days;
	result += (hours / DateTime::HOURSPERDAY);
	result += (minutes / DateTime::MINSPERDAY);
	result += (seconds / DateTime::SECSPERDAY);
	result += (millis / DateTime::MILLISECSPERDAY);

	return result;
}

double DateTime::makeRange(int days, int millis)
{
	double result = 0;

	result += days;
	result += (millis / DateTime::MILLISECSPERDAY);

	return result;
}

double DateTime::makeRange(int millis)
{
	double result = 0;

	result += (millis / DateTime::MILLISECSPERDAY);

	return result;
}

#ifdef _WIN32

//Windows only
double DateTime::from_current()
{
        SYSTEMTIME tv;
        double result = -1;

        GetSystemTime(&tv);

        result = utc_from_YMD_ms(tv.wYear, (enum DateTimeFields::Month) tv.wMonth, tv.wDay, 
	(tv.wHour * MILLISECSPERHOUR) + (tv.wMinute * MILLISECSPERMINUTE) +
		(tv.wSecond * MILLISECSPERSECOND) + tv.wMilliseconds);

        return result;

}

#else

// Linux only
double DateTime::from_current()
{
        double result = -1;
        struct timeval tv;
        struct timezone *tz = 0;
        double us = 0;

        if (gettimeofday(&tv, tz) == 0) 
	{
        	struct tm utctm;
                time_t systim = tv.tv_sec;
                struct tm *utctm_ptr = gmtime_r(&systim, &utctm);
                if (utctm_ptr != 0) 
		{
                        result = from_Tm_UTC(utctm);
                        result += (((double) tv.tv_usec) / 
					MILLISECSPERDAY / 1000);
                } 
		else 
		{
                        throw DateException(StringConversion::syserr());
                }
        } 
	else 
	{
                throw DateException(StringConversion::syserr());
        }

        return result;
}

#endif

double DateTime::utc_from_YMD_ms(int year, enum DateTimeFields::Month month, int day, int millis) 
{
        int isGregorian = 1;
        double days = day + (((double) millis) / MILLISECSPERDAY);
        double result = make_julian(((int) month + 1), year, days, isGregorian);

        return result;

}

double DateTime::local_from_YMD_ms(int year, enum DateTimeFields::Month month, int day, int millis)
{
        struct tm localtm;
        int hour = millis / (3600000);
        int minute = (millis / 60000) % 60;
        int seconds = (millis % 60000) / 1000;
        double result = 0;


        localtm.tm_year = year - 1900;
        localtm.tm_mon = (int) month;
        localtm.tm_mday = day;
        localtm.tm_hour = hour;
        localtm.tm_min = minute;
        localtm.tm_sec = seconds;
	localtm.tm_wday = 0;
	localtm.tm_yday = 0;
        localtm.tm_isdst = -1;

        result = from_Tm_Local(localtm);
        return result;

}


double DateTime::from_Timeval(struct timeval &tv)
{
        double result = 0;
        result = from_Time(tv.tv_sec);
        result += ((double) tv.tv_usec) / MILLISECSPERDAY / 1000;
        return result;

}

double DateTime::from_Time(time_t &timein)
{
        double result = -1;
        struct tm *utctm_ptr = 0;
        struct tm utctm;

#ifdef _WIN32
        utctm_ptr = gmtime(&timein);
        if (utctm_ptr == 0) 
	{
		throw DateException(StringConversion::syserr());
        } 
	else 
	{
                utctm = *utctm_ptr;
        }
#else
        utctm_ptr = gmtime_r(&timein, &utctm);
        if (utctm_ptr == 0) 
	{
		throw DateException(StringConversion::syserr());
        }
#endif

        result = from_Tm_UTC(utctm);

        return result;

}

double DateTime::from_Tm_Local(struct tm &tmin)
{
        time_t systim = 0;
        double result = -1;
        struct tm utctm;
        struct tm *utctm_ptr = 0;

        systim = mktime(&tmin);
        if (systim != (time_t) -1) 
	{
#ifdef _WIN32
                utctm_ptr = gmtime(&systim);
                if (utctm_ptr == 0)
		{
			throw DateException(StringConversion::syserr());
		} 
		else 
		{
			utctm = *utctm_ptr;
		}
#else
                utctm_ptr = gmtime_r(&systim, &utctm);
                if (utctm_ptr == 0)
		{
			throw DateException(StringConversion::syserr());
		}
#endif
                result = from_Tm_UTC(utctm);
        } 
	else 
	{
                throw DateException(L"Bad result from mktime - only dates from Jan 01, 1970 - Jan 19, 2038 are convertible via this function.");
        }

        return result;
}

double DateTime::from_Tm_UTC(struct tm &tmin)
{
	double day = 0;
	double day_fraction = 0;
	double result = 0;
	int isGregorian = 1;

        day = (double) tmin.tm_mday;
        day_fraction = (((double) tmin.tm_hour) / HOURSPERDAY) +
                         (((double) tmin.tm_min) / MINSPERDAY) +
                         (((double) tmin.tm_sec) / SECSPERDAY);

        result = make_julian(tmin.tm_mon + 1, tmin.tm_year + 1900, 
			day + day_fraction, isGregorian);

        return result;
}

double DateTime::from_Fields(const DateTimeFields &fields)
{
        double result;
        int millis = 0;

        millis = (fields.hour * MILLISECSPERHOUR) + 
		 (fields.minute * MILLISECSPERMINUTE) + 
		 (fields.second * MILLISECSPERSECOND) + 
		 (fields.millisecond);
        result = utc_from_YMD_ms(fields.year, fields.month, fields.day, millis);
        result += ((double) fields.timezone.getSecondsToAddToGetUTC(fields.isDst)) / SECSPERDAY;


        return result;
	
}

int DateTime::month_fix(int month) {
        return (month > 2) ? month : month + 12;
}

int DateTime::year_fix(int month, int year) {
        return (month > 2) ? year : year - 1;
}

int DateTime::year_cent(int month, int year) {
        return (year_fix(month, year) / 100);
}

int DateTime::isCalculatedGregorian(int year, int month, double day) {
        int result = 0;
        if (year < 1582) {
                result = 0;
        } else if (year > 1582) {
                result = 1;
        } else {
                if (month < 10) {
                        result = 0;
                } else if (month > 10) {
                        result = 1;
                } else {
                        if (day < 15) {
                                result = 0;
                        } else {
                                result = 1;
                        }
                }
        }
        return result;
}

int DateTime::leaps(int month, int year, double day, int isGregorian) 
{
        int result = 0;
        if ((isGregorian == 1) || (isCalculatedGregorian(year, month, day))) {
                result = 2 - year_cent(month, year) + ( (int) year_cent(month, year) / 4 );
        }

        return result;
}

double DateTime::make_julian(int month, int year, double days, int isGregorian) {
        double result = 0;

        result = ((int) (365.25 * (year_fix(month, year) + 4716))) + ((int) (30.6001 * (month_fix(month) + 1))) + days + leaps(month, year, days, isGregorian) - 1524.5;

        return result;
}

double DateTime::julian_fix(double julian) {
        return julian + 0.5;
}

int DateTime::julian_z(double julian) {
        return (int) julian_fix(julian);
}

double DateTime::julian_f(double julian) {
        return (julian_fix(julian) - julian_z(julian));
}

int DateTime::julian_alpha(double z) {
        return (int) ((z - 1867216.25) / 36524.25);
}

int DateTime::julian_a(int z, int isGregorian) {
        int result;
        if ((isGregorian == 0) || ((isGregorian == -1) && (z < julian10151582))) {
                result = z;
        } else {
                result = z + 1 + julian_alpha(z) - ((int) (julian_alpha(z) / 4));
        }
        return result;
}

int DateTime::julian_b(int a) {
        return a + 1524;
}

int DateTime::julian_c(int b) {
        return (int) ((b - 122.1) / 365.25);
}

int DateTime::julian_d(int c) {
        return (int) (365.25 * c);
}

int DateTime::julian_e(double julian, int b, int d, int isGregorian) {
        return (int) ((julian_b(julian_a(julian_z(julian), isGregorian)) - julian_d(julian_c(julian_b(julian_a(julian_z(julian), isGregorian))))) / 30.6001);
}

double DateTime::julian_to_calendar_day(double julian, int isGregorian) {
        double result = 0;
        result = julian_b(julian_a(julian_z(julian), isGregorian)) - julian_d(julian_c(julian_b(julian_a(julian_z(julian), isGregorian)))) - ((int) (30.6001 * julian_e(julian, julian_b(julian_a(julian_z(julian), isGregorian)), julian_d(julian_c(julian_b(julian_a(julian_z(julian), isGregorian)))), isGregorian))) + julian_f(julian);
        return result;
}

int DateTime::julian_to_calendar_month(double julian, int isGregorian) {
        int result = 0;
        if (julian_e(julian, julian_b(julian_a(julian_z(julian), isGregorian)), julian_d(julian_c(julian_b(julian_a(julian_z(julian), isGregorian)))), isGregorian) < 14) {
                result = julian_e(julian, julian_b(julian_a(julian_z(julian), isGregorian)), julian_d(julian_c(julian_b(julian_a(julian_z(julian), isGregorian)))), isGregorian) - 1;
        } else {
                result = julian_e(julian, julian_b(julian_a(julian_z(julian), isGregorian)), julian_d(julian_c(julian_b(julian_a(julian_z(julian), isGregorian)))), isGregorian) - 13;
        }

        return result;
}

int DateTime::julian_to_calendar_year(double julian, int isGregorian) {
        int result = 0;
        int subresult = 0;
        if (julian_to_calendar_month(julian, isGregorian) > 2) {
                subresult = 0;
        } else {
                subresult = 1;
        }
        result = julian_c(julian_b(julian_a(julian_z(julian), isGregorian))) - 4716 + subresult;

        return result;
}

// This routine is freely available, often included
// with GNU compilers under the name timegm (the
// reverse of gmtime.  Since windows versions are
// not included with the OS, we include the source
// here.
//
// Contributed by Roger Beeman <beeman@cisco.com>, with the help of
// Mark Baushke <mdb@cisco.com> and the rest of the Gurus at CISCO.
// Further improved by Roger with assistance from Edward J. Sabol
// based on input by Jamie Zawinski.
//
// Final improvements by Ravi Desai to make it thread safe in both
// Unix and Windows.
time_t DateTime::mktime_from_utc (struct tm *t) {
        time_t tl, tb;
        struct tm tg;
        struct tm *tg_ptr;

        tl = mktime (t);
        if (tl == -1) 
	{
                t->tm_hour--;
                tl = mktime (t);
                if (tl == -1) return -1;
                tl += 3600;
        }

#ifdef _WIN32
        tg_ptr = gmtime (&tl);
        if (tg_ptr == 0) return -1;
        else tg = *tg_ptr;
#else
        tg_ptr = gmtime_r(&tl, &tg);
        if (tg_ptr == 0) return -1;
#endif

        tg.tm_isdst = 0;
        tb = mktime (&tg);
        if (tb == -1) 
	{
                tg.tm_hour--;
                tb = mktime (&tg);
                if (tb == -1) return -1;
                tb += 3600;
        }

        return (tl - (tb - tl));
}



} // namespace
