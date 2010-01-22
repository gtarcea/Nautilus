
#ifndef __DAEMON_INCLUDE_
#define __DAEMON_INCLUDE_
#include <sys/types.h>

namespace SynerEdge {

class Daemon {
public:
	Daemon(bool detach) ;

private:
	Daemon() ;
	pid_t mypid ;

} ; /* class Daemon */

}  ; /* namespace SynerEdge */

#endif /* __DEAMON_INCLUDE_ */
