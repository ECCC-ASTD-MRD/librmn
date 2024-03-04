#ifndef RMN_CONVIP_H__
#define RMN_CONVIP_H__

#include <rmn/rpnmacros.h>

void f77name(igapg)(const char gtyp[], char pg1[7], char pg2[7], char pg3[8], char pg4[8],
                    const int32_t *ig1, const int32_t *ig2, const int32_t *ig3, const int32_t *ig4,
                    F2Cl lc1, F2Cl lc2, F2Cl lc3, F2Cl lc4, F2Cl lc5);

#endif // RMN_CONVIP_H__