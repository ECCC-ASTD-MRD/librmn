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

int f77name(rmnlib_version)(char *rmn,int *print,int len);
int f77name(r8ipsort)      (int32_t  *ip,double *a,int32_t *n);
int f77name(convip)        (int32_t *ip,float *p,int32_t *kind,int32_t *mode,char *string,int32_t *flag);
int f77name(sort)          (float *work,int32_t *n);
int f77name(ipsort)        (int32_t *ip,float *a,int32_t *n);
int f77name(fd1)           (float *gd1,float *f,float *h);
int f77name(fdm)           (float *gdm,float *f,float *h,int *m);
int f77name(int1d1)        (float *fi,float *f,float *xi,float *x,float *fx,float *h,int32_t *m,int32_t *mi,float *cmu1,float *c1,float *clmdam,float *cm,float *a,float *c,float *d);
int f77name(xyfll)         (float *x,float *y,float *dlat,float *dlon,float *d60,float *dgrw,int32_t *nhem);
int f77name(llfxy)         (float *dlat,float *dlon,float *x,float *y,float *d60,float *dgrw,int32_t *nhem);
int f77name(mscale)        (float *r,float *d60,float *pi,float *pj,int32_t *ni,int32_t *nj);
int f77name(hyb_to_pres)   (float *pres,float *hyb,float *ptop,float *rcoef,float *pref,int32_t *kind,float *ps,int32_t *NI,int32_t *NJ,int32_t *NK);

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
