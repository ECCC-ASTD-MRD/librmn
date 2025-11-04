#ifndef RMN_PUBLIC_PRIMITIVES_H
#define RMN_PUBLIC_PRIMITIVES_H

#include <stdio.h>
#include <stdint.h>

#include "rmn/rpnmacros.h"

size_t fread32( void *ptr, size_t size, size_t nitems, FILE *stream);
void f77name(micro_sleep) (double *secs);
int32_t c_exdb(const char* titre, const char* revis, const char* flag);
int32_t f77name(exdb)(const char* titre, const char* revis, const char* flag, F2Cl len_1, F2Cl len_2, F2Cl len_3);

int32_t c_exfin(const char* titre, const char* revis, const char* flag);
int32_t f77name(exfin)(const char* titre, const char* revis, const char* flag, F2Cl len_1, F2Cl len_2, F2Cl len_3);

#endif
