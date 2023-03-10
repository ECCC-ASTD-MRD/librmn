#ifndef RMN_CONVIP_H__
#define RMN_CONVIP_H__

#include <rmn/rpnmacros.h>

void f77name(igapg)(char gtyp[], char pg1[7], char pg2[7], char pg3[8], char pg4[8],
                    int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4,
                    F2Cl lc1, F2Cl lc2, F2Cl lc3, F2Cl lc4, F2Cl lc5);

void f77name(convip_plus)(int32_t *iip1, float *level, int32_t *kind, int32_t *mode, char c_level[16],
                          int32_t *flag, F2Cl l);
#endif // RMN_CONVIP_H__
