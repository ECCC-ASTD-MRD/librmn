#ifndef RMN_CONVIP_H__
#define RMN_CONVIP_H__

#include <rmn/rpnmacros.h>

void igapg_c(const char* grtyp, char pg1[7], char pg2[7], char pg3[8], char pg4[8],
             const int32_t* ig1, const int32_t* ig2, const int32_t* ig3, const int32_t* ig4);

void f77name(convip_plus)(int32_t *iip1, float *level, int32_t *kind, int32_t *mode, char c_level[16],
                          int32_t *flag, F2Cl l);
#endif // RMN_CONVIP_H__
