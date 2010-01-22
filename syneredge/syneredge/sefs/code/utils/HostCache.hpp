#ifndef SynerEdge_HostCache_h__
#define SynerEdge_HostCache_h__

#include "Net.hpp"
#include "Observer.hpp"

#include "boost/utility.hpp"
#include <string>
#include <map>

namespace SynerEdge
{

class HostCache : public boost::noncopyable
{
public:
	~HostCache();

	static HostCache *instance();
	Host getHost(const std::wstring &dnsname, bool isIP6);

	void clearCache(Observable *obs, const int &sig);

private:
	typedef std::map<std::wstring, Host> Cache;
	typedef std::map<std::wstring, Host>::iterator CacheIterator;

	Cache cache;
	boost::mutex mtx;

	// singleton semantics
	HostCache();
	static HostCache *_instance;
	static boost::once_flag _sentry;
	static void createInstance();

	// non-copyable semantics
	HostCache(const HostCache &);
	HostCache &operator=(const HostCache &);
};


}

#endif

