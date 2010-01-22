#ifndef SygGlobalSemaphore_hpp
#define SygGlobalSemaphore_hpp

#include "SynerEdge.hpp"
#include <string>
#include <map>
#include "boost/utility.hpp"
#include "boost/thread.hpp"
#include "boost/thread/mutex.hpp"

namespace SynerEdge
{

class GlobalSemaphoreException : public SynerEdgeException
{
public:
        GlobalSemaphoreException(const std::wstring &msg) 
	: SynerEdgeException(msg) {}
        virtual ~GlobalSemaphoreException() {}
};

class GlobalSemaphore : boost::noncopyable
{
public:
	enum Cleanup { never, always, iffirst };

	GlobalSemaphore(
		unsigned initial, 
		const std::wstring &name, 
		enum Cleanup clean);
	~GlobalSemaphore();

	virtual void post(unsigned count);
	virtual void wait();
	virtual bool tryWait();

	std::wstring getRequestedName() const;
	std::wstring getAssignedGlobalName() const;

	bool willCleanup() const;
	
private:
	void unlink();

	std::wstring givenName;
	std::wstring globalName;
	bool openExclusive;
	void *semHandle;
	unsigned initialCount;

	static std::map<void *, int> globalMap;
	static boost::mutex mtx;
	
	// noncopyable semantics
	GlobalSemaphore(const GlobalSemaphore &);
	GlobalSemaphore &operator=(const GlobalSemaphore &);
};

}

#endif

