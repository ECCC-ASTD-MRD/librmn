#ifndef RMN_BASE_BASE_H__
#define RMN_BASE_BASE_H__

#include <rmn/rpnmacros.h>

#include "rmn/base.h"

void f77name(grll)(
    float * const lat,
    float * const lon,
    const int32_t * const ni,
    const int32_t * const nj,
    const float * const xlat00,
    const float * const xlon00,
    const float * const dlat,
    const float * const dlon
);
void f77name(grps)(
    float * const lat,
    float * const lon,
    const int32_t * const ni,
    const int32_t * const nj,
    const float * const  pi,
    const float * const pj,
    const float * const d60,
    const float * const dgrw,
    const int32_t * const hem
);
void f77name(mxm)(
    const float * const a,
    const int32_t * const nar,
    const float * const b,
    const int32_t * const nac,
    float * const c,
    const int32_t * const nbc
);
void f77name(permut)(float *lat, int32_t *ni, int32_t *nj);

#endif // RMN_BASE_BASE_H__
