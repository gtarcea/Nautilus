#include "daemon.h"
#include "tester.h"

static int sawhup ;
static int sawquit ;
static int sawint ;
static int sawmisc ;

static void
signal_handler(int signo)
{
	if (signo == SIGQUIT) {
		printf("SIGQUIT\n") ;
		sawquit = 1 ;
	} else if (signo == SIGHUP) {
		printf("SIGHUP\n") ;
		sawhup = 1 ;
	} else if (signo == SIGINT) {
		printf("SIGINT\n") ;
		sawint = 1 ;
	} else {
		printf("SIG - MISC\n") ;
		sawmisc = 1 ;
	}
}

int
test_signal_handlers()
{
	pid_t mypid ;

	sawmisc = sawhup = sawquit = sawint = 0 ;

	daemon_setup_sighandlers(signal_handler) ;

	mypid = getpid() ;

	daemon_send_sig(mypid, SIGINT) ;
	daemon_send_sig(mypid, SIGHUP) ;
	daemon_send_sig(mypid, SIGQUIT) ;

	if (sawint && sawhup && sawquit) {
		if (sawmisc) {
			return 0 ;
		}

		return 1 ;
	}

	return 0 ;
}

main()
{
	int rc ;

	P("test_signal_handlers()") ;
	rc = test_signal_handlers() ;
	S(rc, "test_signal_handlers()") ;

	exit(0) ;
}

