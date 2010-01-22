
#ifndef __SHMEM_INCLUDE_
#define __SHMEM_INCLUDE_

#ifdef __cplusplus
extern "C" {
#endif

int shmem_open() ;
int shmem_close(int remove) ;
void *shmem_get_memptr() ;

#ifdef __cplusplus
}
#endif

#endif /* __SHMEM_INCLUDE_ */

