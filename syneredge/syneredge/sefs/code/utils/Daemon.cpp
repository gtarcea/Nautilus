#include "Daemon.hpp"
#include <sys/types.h>
#include <unistd.h>

extern "C" {
	int exit(int) ;
	int umask(int) ;
} ;

namespace SynerEdge {

Daemon::Daemon(bool detach)
{
	if (detach) {
		pid_t pid ;
		if ( (pid = fork() ) < 0) {
			/*
			** Raise an exception.
			*/
		} else if ( pid != 0) {
			exit(0) ;
		}
	} 

	mypid = getpid() ;
	setsid() ;
	chdir("/") ;
	umask(0) ;
}

} /* namespace SynerEdge */
