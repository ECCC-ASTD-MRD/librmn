#ifndef RMN_BASE_BASE_H__
#define RMN_BASE_BASE_H__

#include <rmn/rpnmacros.h>

void f77name(difdatr)(int32_t *date1, int32_t *date2, double *diff_deb);
void f77name(incdatr)(int32_t *date1, int32_t *date2, double *nhours);
int32_t f77name(newdate)(int32_t *date_valid, int32_t *dat2, int32_t *dat3, int32_t *minus3);

void f77name(cigaxg)(char grtyp[], float *xlat1, float *xlon1, float *xlat2, float *xlon2,
                     int32_t *ig1,  int32_t *ig2, int32_t *ig3, int32_t *ig4, F2Cl l);
void f77name(cxgaig)(char grtyp[], int32_t *ig1n, int32_t *ig2n, int32_t *ig3n, int32_t *ig4n,
                     float *pi, float *pj, float *d60, float *dgrw, F2Cl l);
void f77name(grll)(float *lat, float *lon, int32_t *ni, int32_t *nj,
                   float *xlat00, float *xlon00, float *dlat, float *dlon);
void f77name(grps)(float *lat, float *lon, int32_t *ni, int32_t *nj,
                   float * pi, float *pj, float *d60, float *dgrw, int32_t *hemisphere);
void f77name(mxm)(float *a, int32_t *nar, float *b, int32_t *nac, float *c, int32_t *nbc);
void f77name(permut)(float *lat, int32_t *ni, int32_t *nj);

#endif // RMN_BASE_BASE_H__
