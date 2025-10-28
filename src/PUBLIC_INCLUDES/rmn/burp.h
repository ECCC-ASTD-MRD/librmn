#ifndef BURP_H
#define BURP_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// c_burp: C wrappers around Fortran functions
int32_t c_mrblocx(int32_t *buf, const int32_t bfam, const int32_t bdesc, const int32_t bknat, const int32_t bktyp,
                  const int32_t bkstp, const int32_t blk0);
int32_t c_mrbprml(const int32_t* buf, const int32_t bkno, int32_t* tblprm, const int32_t nprm, const int32_t inblocs);
int32_t c_mrbini(const int32_t iun, int32_t* buf, const int32_t temps, const int32_t flgs, char* stnid,
                 const int32_t idtp, const int32_t lati, const int32_t longi, const int32_t dx, const int32_t dy,
                 const int32_t elev, const int32_t drcv, const int32_t date, const int32_t oars, const int32_t runn,
                 int32_t* sup, const int32_t nsup, int32_t* xaux, const int32_t nxaux);
int32_t c_mrbtyp(int32_t *hbknat, int32_t *hbktyp, int32_t *hbkstp, const int32_t hbtyp);
int32_t c_mrbupd(const int32_t iun, int32_t* buf, const int32_t temps, const int32_t flgs, char* stnid,
                 const int32_t idtp, const int32_t lati, const int32_t longi, const int32_t dx, const int32_t dy,
                 const int32_t elev, const int32_t drcv, const int32_t date, const int32_t oars, const int32_t runn,
                 int32_t* sup, const int32_t nsup, int32_t* xaux, const int32_t nxaux);
int32_t c_mrbcvt(int32_t* liste, int32_t* tblval, float* rval, const int32_t nele, const int32_t nval,
                 const int32_t nt, const int32_t mode);
int32_t c_mrbcol(int32_t* liste, int32_t* cliste, const int32_t nele);
int32_t c_mrbdcl(int32_t* cliste, int32_t* liste, const int32_t nele);
int32_t c_mrbcov(const int32_t elem);
int32_t c_mrbdcv(const int32_t elem);
int32_t c_mrbsct(int32_t* tablusr, const int32_t neleusr);
int32_t c_mrbrpt(const int32_t elem);
int32_t c_mrbtbl(int32_t* tablusr, const int32_t nslots, const int32_t neleusr);

int32_t c_mrfcls(const int32_t iun);
int32_t c_mrfdel(const int32_t handle);
int32_t c_mrfgoc(char* optnom, char opvalc[9]);
int32_t c_mrfgor(char* optnom, float *opvalr);
int32_t c_mrfloc(int32_t iun, int32_t handle, char* stnid, int32_t idtyp, int32_t lat, int32_t lon, int32_t date,
                 int32_t temps, int32_t* sup, int32_t nsup);
int32_t c_mrfmxl(const int32_t iun);
int32_t c_mrfnbr(const int32_t iun);
int32_t c_mrfopc(char* optnom, char* opvalc);
int32_t c_mrfopn(const int32_t iun, char* mode);
int32_t c_mrfopr(char* optnom, const float opvalr);
int32_t c_mrfprm(int32_t handle, char stnid[10], int32_t *idtyp, int32_t *lat, int32_t *lon, int32_t *dx, int32_t *dy,
                 int32_t *date, int32_t *temps, int32_t *flgs, int32_t* sup, const int32_t nsup, int32_t *lng);
int32_t c_mrfvoi(const int32_t iun);


// burp98: BURP functions implemented in C
int c_mrbadd(void *buffer, int *bkno, const int nele, const int nval, const int nt, const int bfam,
             const int bdesc, const int btyp, const int nbit, int *bit0, const int datyp, uint32_t *lstele,
             uint32_t *tblval);
int c_mrbloc(void *buffer, const int bfam, const int bdesc, const int btyp, const int blkno);
int c_mrbdel(void *buffer, const int number);
int c_mrbhdr(void *buf, int *temps, int *flgs, char *stnid, int *idtyp, int *lati, int *lon, int *dx, int *dy,
             int *elev, int *drcv, int *date, int *oars, int *run, int *nblk, uint32_t *sup, const int nsup,
             uint32_t *xaux, const int nxaux);
int c_mrblen(const void *buffer, int *bitsUsed, int *bitsLeft);
int c_mrbprm(void *buf, const int bkno, int *nele, int *nval, int *nt, int *bfam, int *bdesc, int *btyp,
             int *nbit, int *bit0, int *datyp);
int c_mrbrep(void *buffer, int blkno, uint32_t *tblval);
int c_mrbxtr(void *buffer, int bkno, uint32_t *lstele, uint32_t *tblval);

int c_mrfget(int handle, void *buffer);
int c_mrfput(const int iun, const int handle, void *buffer);
int c_mrfbfl(int iun);




#ifdef __cplusplus
}
#endif

#endif
