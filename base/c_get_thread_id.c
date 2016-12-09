#define _GNU_SOURCE
#include <unistd.h>
#include <sys/syscall.h>
#include <sys/types.h>

// M.Valin 2016/12/08  initial release

#pragma weak c_get_thread_id_=c_get_thread_id
#pragma weak c_get_thread_id__=c_get_thread_id
int c_get_thread_id__();
int c_get_thread_id_();

int c_get_thread_id(){   // get thread id from Linux, callable from C and Fortran
  pid_t tid;
  tid = syscall(SYS_gettid);
  return tid;
}
