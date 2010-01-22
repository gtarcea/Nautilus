
#ifndef __DAEMON_INCLUDE_
#define __DAEMON_INCLUDE_

#include <sys/types.h>
#include <unistd.h>
#include <signal.h>

#ifdef __cplusplus
extern "C" {
#endif

int daemon_setup_sighandlers(__sighandler_t sigfunc) ;
int daemon_start(int becomedaemon) ;
int daemon_stop(pid_t pid) ;
int daemon_reinit(pid_t pid) ;
int daemon_send_sig(pid_t, int signo) ;

#ifdef __cplusplus
}
#endif

#endif /* __DAEMON_INCLUDE_ */

