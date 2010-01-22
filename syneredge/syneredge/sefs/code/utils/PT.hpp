
#ifndef __PT_INCLUDE_
#define __PT_INCLUDE_

#include <sys/time.h>
#include <time.h>
#include <stdio.h>

#ifdef DO_PT

#define PT(a) { struct timeval t ; gettimeofday(&t, NULL); printf("\n%s:\tTime = %ul:(%ul) threadid = %ul\n", a, t.tv_sec, t.tv_usec, pthread_self()) ; }

#else

#define PT(a)

#endif // DO_PT

#endif // __PT_INCLUDE_
