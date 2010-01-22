
#ifndef __SEMAPHORE_INCLUDE_
#define __SEMAPHORE_INCLUDE_

#ifdef __cplusplus
extern "C" {
#endif

struct SEMAPHORE ;
typedef struct SEMAPHORE SEMAPHORE ;
typedef SEMAPHORE *SEMAPHOREP ;

SEMAPHOREP semaphore_create(key_t key, int value) ;
SEMAPHOREP semaphore_open(key_t) ;
int semaphore_increment(SEMAPHOREP sem) ;
int semaphore_decrement(SEMAPHOREP sem) ;
int semaphore_getvalue(SEMAPHOREP sem) ;
int semaphore_destroy(SEMAPHOREP sem, int removesys) ;

#ifdef __cplusplus
}
#endif

#endif /* __SEMAPHORE_INCLUDE_ */
