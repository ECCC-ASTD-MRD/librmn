#ifndef RMN_RMN_BASE_H__
#define RMN_RMN_BASE_H__

#include <stdint.h>
#include <rmn/rpnmacros.h>

#ifdef __cplusplus
extern "C" {
#endif

void f77name(cxgaig)(const char * const grtyp,
                     int32_t * const ig1, int32_t * const ig2 , int32_t * const ig3, int32_t * const ig4,
                     const float * const xg1, const float * const xg2, const float * const xg3, const float * const xg4,
                     F2Cl l);
void f77name(cigaxg)(const char * const grtyp,
                     float * const xlat1, float * const xlon1, float * const xlat2, float * const xlon2,
                     const int32_t * const ig1,  const int32_t * const ig2, const int32_t * const ig3, const int32_t * const ig4,
                     F2Cl l);

void f77name(difdatr)(int32_t *fdat1,int32_t *fdat2,double *fnhours);
void f77name(incdatr)(int32_t *fdat1,int32_t *fdat2,double *fnhours);
int f77name(newdate)(int32_t * const fdat1, int32_t * const fdat2, int32_t * const fdat3, const int32_t * const fmode);

void difdatr_c(int32_t *fdat1,int32_t *fdat2,double *fnhours);
void incdatr_c(int32_t *fdat1,int32_t *fdat2,double *fnhours);
int newdate_c(int32_t * const fdat1, int32_t * const fdat2, int32_t * const fdat3, const int32_t * const fmode);

void f77name(rmnlib_version)(char *rmn,int *print,int len);
void f77name(ipsort8)       (int32_t  *ip,double *a,int32_t *n);
void f77name(convip)        (int32_t *ip,float *p,int32_t *kind,int32_t *mode,char *string,int32_t *flag);
void f77name(sort)          (float *work,int32_t *n);
void f77name(ipsort)        (int32_t *ip,float *a,int32_t *n);
void f77name(fd1)           (float *gd1,float *f,float *h);
void f77name(fdm)           (float *gdm,float *f,float *h,int *m);
void f77name(int1d1)        (float *fi,float *f,float *xi,float *x,float *fx,float *h,int32_t *m,int32_t *mi,float *cmu1,float *c1,float *clmdam,float *cm,float *a,float *c,float *d);
void f77name(xyfll)         (float *x,float *y,float *dlat,float *dlon,float *d60,float *dgrw,int32_t *nhem);
void f77name(llfxy)         (float *dlat,float *dlon,float *x,float *y,float *d60,float *dgrw,int32_t *nhem);
void f77name(mscale)        (float *r,float *d60,float *pi,float *pj,int32_t *ni,int32_t *nj);
int f77name(hyb_to_pres)    (float *pres,float *hyb,float *ptop,float *rcoef,float *pref,int32_t *kind,float *ps,int32_t *NI,int32_t *NJ,int32_t *NK);

void f77name(interp1d_findpos) ();
void f77name(interp1d_nearestneighbour) ();
void f77name(interp1d_linear) ();
void f77name(interp1d_cubicwithderivs) ();
void f77name(interp1d_cubiclagrange) ();
void f77name(extrap1d_lapserate) ();
#ifdef __cplusplus
}
#endif

#endif // RMN_RMN_BASE_H__
