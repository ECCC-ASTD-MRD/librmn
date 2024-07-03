#ifndef RMN_BASE_BASE_H__
#define RMN_BASE_BASE_H__

#include <rmn/rpnmacros.h>

#include "rmn/base.h"

void f77name(grll)(float *lat, float *lon, int32_t *ni, int32_t *nj,
                   float *xlat00, float *xlon00, float *dlat, float *dlon);
void f77name(grps)(float *lat, float *lon, int32_t *ni, int32_t *nj,
                   float * pi, float *pj, float *d60, float *dgrw, int32_t *hemisphere);
void f77name(mxm)(
    const float * const a,
    const int32_t * const nar,
    const float * const b,
    const int32_t * const nac,
    float * const c,
    const int32_t * const nbc);
void f77name(permut)(float *lat, int32_t *ni, int32_t *nj);

#endif // RMN_BASE_BASE_H__
