#ifndef RMN_RMN_BASE_H__
#define RMN_RMN_BASE_H__

#include <rmn/rpnmacros.h>

#ifdef __cplusplus
extern "C" {
#endif

void f77name(cxgaig)(char *grtyp, int32_t *ig1n, int32_t *ig2n, int32_t *ig3n, int32_t *ig4n,
                     float *pi, float *pj, float *d60, float *dgrw, F2Cl l);
void f77name(cigaxg)(char *grtyp, float *xlat1, float *xlon1, float *xlat2, float *xlon2,
                     const int32_t *ig1,  const int32_t *ig2, const int32_t *ig3, const int32_t *ig4, F2Cl l);

void f77name(difdatr)(int32_t *fdat1,int32_t *fdat2,double *fnhours);
void f77name(incdatr)(int32_t *fdat1,int32_t *fdat2,double *fnhours);
int f77name(newdate)(int32_t *fdat1,int32_t *fdat2,int32_t *fdat3,int32_t *fmode);

void difdatr_c(int32_t *fdat1,int32_t *fdat2,double *fnhours);
void incdatr_c(int32_t *fdat1,int32_t *fdat2,double *fnhours);
int newdate_c(int32_t *fdat1,int32_t *fdat2,int32_t *fdat3,int32_t *fmode);

#ifdef __cplusplus
}
#endif

#endif // RMN_RMN_BASE_H__
