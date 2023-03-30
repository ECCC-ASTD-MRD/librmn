#ifndef _RMN_H
#define _RMN_H

#include "rmn/fnom.h"
#include "rmn/fstd98.h"
#include "rmn/fstdsz.h"
#include "rmn/ezscint.h"
#include "rmn/fst_missing.h"
#include "rmn/c_wkoffit.h"
#include "rmn/burp.h"
#include "rmn/ezscint.h"
#include "rmn/c_ccard.h"
#include "rmn/ftnStrLen.h"
#include "rmn/convert_ip.h"

// To put in respective inlude file
void f77name(cxgaig)(char *grtyp,int32_t *ig1,int32_t *ig2,int32_t *ig3,int32_t *ig4,float *xg1,float *xg2,float *xg3,float *xg4);
void f77name(cigaxg)(char *grtyp,float *xg1,float *xg2,float *xg3,float *xg4,int32_t *ig1,int32_t *ig2,int32_t *ig3,int32_t *ig4);
void f77name(convip_plus)(int32_t *ipnew,float *level,int32_t *fkind,int32_t *fmode,char *strg,int32_t *flag,F2Cl strglen);
int f77name(newdate)(int32_t *fdat1,int32_t *fdat2,int32_t *fdat3,int32_t *fmode);
int f77name(difdatr)(int32_t *fdat1,int32_t *fdat2,double *fnhours);
int f77name(incdatr)(int32_t *fdat1,int32_t *fdat2,double *fnhours);

#endif
