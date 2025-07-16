#include <stdio.h>

#include <rmn/va_args_num.h>

#define PRINT_NUM_ARGS(N, ...) { fprintf(stderr, "expecting %2d VA_ARGS arguments , detected %2d\n", N, VA_ARGS_NUM(__VA_ARGS__)) ; \
                                 if(N != VA_ARGS_NUM(__VA_ARGS__)) goto fail ; \
                               }

int main() {
  fprintf(stderr, "test of macro VA_ARGS_NUM, detect number of arguments to a macro\n") ;
  PRINT_NUM_ARGS(0) ;
  PRINT_NUM_ARGS(1, 1) ;
  PRINT_NUM_ARGS(2, 1, 2) ;
  PRINT_NUM_ARGS(3, 1, 2, 3) ;
  PRINT_NUM_ARGS(4, 1, 2, 3, 4) ;
  PRINT_NUM_ARGS(10, 1, 2, 3, 4, 5, a, b, c, d, e) ;
  PRINT_NUM_ARGS(20, 1, 2, 3, 4, 5, a, b, c, d, e, 1, 2, 3, 4, 5, a, b, c, d, e) ;
  PRINT_NUM_ARGS(4, 1.2, "a", 3, 4l) ;
  fprintf(stderr, "SUCCESS\n") ;
  return 0 ;

fail:
  fprintf(stderr, "FAIL\n") ;
  return 1 ;
}
