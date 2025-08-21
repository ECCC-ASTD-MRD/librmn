#ifndef RMN_PUBLIC_PRIMITIVES_H
#define RMN_PUBLIC_PRIMITIVES_H

#include <stdio.h>
#include <stdint.h>

#include "rmn/rpnmacros.h"

size_t fread32( void *ptr, size_t size, size_t nitems, FILE *stream);
void f77name(micro_sleep) (double *secs);
void rmn_print_build_info(void);

#endif
