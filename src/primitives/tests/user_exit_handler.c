#include <rpnmacros.h>
extern exit(), f77name(qqexit)();
typedef void *fn_ptr();
fn_ptr *c_exit_handler = (fn_ptr *) exit;
/* static int *f_exit_handler() = &f77name(qqexit); */
fn_ptr *f_exit_handler = (fn_ptr *) f77name(qqexit);

void user_exit_handler(int err)
{
  fn_ptr *u_func = c_exit_handler;
  c_exit_handler = (fn_ptr *) exit;
  (*u_func)(err);
}

void f77name(f_user_exit_handler)(int *err)
{
  fn_ptr *u_func = f_exit_handler;
  f_exit_handler = (fn_ptr *) f77name(qqexit);
  (*u_func)(err);
}

void set_user_exit_handler(void *userfunc())
{
  c_exit_handler = userfunc;
}

void f77name(f_set_user_exit_handler)(void *userfunc())
{
  
  f_exit_handler = (fn_ptr *) userfunc;
} 


