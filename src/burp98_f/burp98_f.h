#ifndef RMN_BURP98_F_H__
#define RMN_BURP98_F_H__

#include <rmn/rpnmacros.h>

int32_t f77name(mrbcol)(int32_t* liste, int32_t* cliste, int32_t *nele);
int32_t f77name(mrbcov)(int32_t *elem);
int32_t f77name(mrbcvt)(int32_t* liste, int32_t* tblval, float* rval, int32_t *nele, int32_t *nval,
                        int32_t *nt, int32_t *mode);
int32_t f77name(mrbdcl)(int32_t* cliste, int32_t* liste, int32_t *nele);
int32_t f77name(mrbdcv)(int32_t *elem);
int32_t f77name(mrbini)(int32_t *iun, int32_t* buf, int32_t *temps, int32_t *flgs, char* stnid,
                        int32_t *idtp, int32_t *lati, int32_t *longi, int32_t *dx, int32_t *dy, int32_t *elev,
                        int32_t *drcv, int32_t *date, int32_t *oars, int32_t *runn, int32_t* sup, int32_t *nsup,
                        int32_t* xaux, int32_t *nxaux, F2Cl l1);
int32_t f77name(mrblocx)(int32_t *buf, int32_t *bfam, int32_t *bdesc, int32_t *bknat, int32_t *bktyp,
                         int32_t *bkstp, int32_t *blk0);
int32_t f77name(mrbprml)(const int32_t *buf, const int32_t *bkno, int32_t *blprm, const int32_t *nprm,
                         const int32_t *inblocs);
int32_t f77name(mrbrpt)(int32_t *elem);
int32_t f77name(mrbsct)(int32_t* tablusr, int32_t *neleusr);
int32_t f77name(mrbtbl)(int32_t* tablusr, int32_t *nslots, int32_t *neleusr);
int32_t f77name(mrbtyp)(int32_t *hbknat, int32_t *hbktyp, int32_t *hbkstp, int32_t *hbtyp);
int32_t f77name(mrbupd)(int32_t *iun, int32_t* buf, int32_t *temps, int32_t *flgs, char* stnid, int32_t *idtp,
                        int32_t *lati, int32_t *longi, int32_t *dx, int32_t *dy, int32_t *elev,
                        int32_t *drcv, int32_t *date, int32_t *oars, int32_t *runn, int32_t* sup,
                        int32_t *nsup, int32_t* xaux, int32_t *nxaux, F2Cl l1);
int32_t f77name(mrfcls)(const int32_t *iun);
int32_t f77name(mrfdel)(int32_t *handle);
int32_t f77name(mrfgoc)(char* optnom, char opvalc[9], F2Cl l1, F2Cl l2);
int32_t f77name(mrfgor)(char* optnom, float *opvalr, F2Cl l1);
int32_t f77name(mrfloc)(int32_t *iun, int32_t *handle, char* stnid, int32_t *idtyp, int32_t *lat, int32_t *lon,
                        int32_t *date, int32_t *temps, int32_t* sup, int32_t *nsup, F2Cl l1);
int32_t f77name(mrfmxl)(int32_t *iun);
int32_t f77name(mrfnbr)(int32_t *iun);
int32_t f77name(mrfopc)(char* optnom, char* opvalc, F2Cl l1, F2Cl l2);
int32_t f77name(mrfopn)(int32_t *iun, char* mode, F2Cl l1);
int32_t f77name(mrfopr)(char* optnom, float *opvalr, F2Cl l1);
int32_t f77name(mrfprm)(int32_t *handle, char stnid[10], int32_t *idtyp, int32_t *lat, int32_t *lon, int32_t *dx,
                        int32_t *dy, int32_t *date, int32_t *temps, int32_t *flgs, int32_t* sup, int32_t *nsup,
                        int32_t *lng, F2Cl l1);
int32_t f77name(mrfvoi)(int32_t *iun);


#endif // RMN_BURP98_F_H__
