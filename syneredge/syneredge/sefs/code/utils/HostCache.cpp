#include "HostCache.hpp"
#include "SignalThread.hpp"

namespace SynerEdge
{

boost::once_flag HostCache::_sentry = BOOST_ONCE_INIT;
HostCache *HostCache::_instance = 0;

HostCache::HostCache() 
{
	SignalThread::instance(SIGHUP)->raisedSignal += 
		new ObserverDelegate<HostCache, int>(*this,
		&HostCache::clearCache);
}

HostCache::~HostCache() 
{
	SignalThread::instance(SIGHUP)->raisedSignal -= 
		new ObserverDelegate<HostCache, int>(*this,
		&HostCache::clearCache);
}

void HostCache::createInstance()
{
	_instance = new HostCache();
}

HostCache *HostCache::instance()
{
	boost::call_once(&HostCache::createInstance, _sentry);
	return _instance;
}

Host HostCache::getHost(const std::wstring &dnsname, bool isIP6)
{
	boost::mutex::scoped_lock lck(mtx);

	std::wstring lookupname = dnsname;
	if (isIP6) lookupname += L"&6";

	CacheIterator itor = cache.find(lookupname);
	if (itor != cache.end())
	{
		return (*itor).second;
	}
	else
	{
		Host hst(dnsname, isIP6);
		cache[lookupname] = hst;
		return hst;
	}	
}

void HostCache::clearCache(Observable *obs, const int &sig)
{
	boost::mutex::scoped_lock lck(mtx);
	cache.clear();
}

}

