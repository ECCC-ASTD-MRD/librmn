#ifndef BURP_H
#define BURP_H

#include <stdint.h>

int c_mrbprml(int buf[], int bkno, int tblprm[], int nprm, int inblocs);

int c_mrbadd(void *buffer, int *bkno, int nele, int nval, int nt, int bfam, int bdesc, int btyp, int nbit, int *bit0, int datyp, uint32_t *lstele, uint32_t *tblval);

int c_mrbdel(void *buffer, int number);

int c_mrbhdr(uint32_t *buf, int *temps, int *flgs, char *stnid, int *idtyp, int *lati, int *lon, int *dx, int *dy, int *elev, int *drcv, int *date, int *oars, int *run, int *nblk, uint32_t *sup, int nsup, uint32_t *xaux, int nxaux);

int c_mrbini(int iun, int buf[], int temps, int flgs, char stnid[], int idtp, int lati, int longi, int dx, int dy, int elev, int drcv, int date, int oars, int runn, int sup[], int nsup, int xaux[], int nxaux);

int c_mrblen(void *buffer, int *bitsUsed, int *bitsLeft);

int c_mrbloc(void *buffer, int bfam, int bdesc, int btyp, int blkno);

int c_mrbprm(uint32_t *buf,int  bkno, int *nele, int *nval, int *nt, int *bfam, int *bdesc, int *btyp, int *nbit, int *bit0, int *datyp);

int c_mrbrep(void *buffer, int blkno, uint32_t *tblval);

int c_mrbtyp(int *hbknat, int *hbktyp, int *hbkstp, int hbtyp);

int c_mrbupd(int iun, int buf[], int temps, int flgs, char stnid[], int idtp, int lati, int longi, int dx, int dy, int elev, int drcv, int date, int oars, int runn, int sup[], int nsup, int xaux[], int nxaux);

int c_mrbxtr(void *buffer, int bkno, uint32_t *lstele, uint32_t *tblval);

int c_mrfcls(int iun);

int c_mrfdel(int handle);

int c_mrfget(int handle, void *buffer);

int c_mrfgoc(char optnom[], char opvalc[9]);

int c_mrfgor(char optnom[], float *opvalr);

int c_mrfloc(int iun, int handle, char stnid[], int idtyp, int lat, int lon, int date, int temps, int sup[], int nsup);

int c_mrfmxl(int iun);

int c_mrfnbr(int iun);

int c_mrfopc(char optnom[], char opvalc[]);

int c_mrfopn(int iun, char mode[]);

int c_mrfopr(char optnom[], float opvalr);

int c_mrfprm(int handle, char stnid[10], int *idtyp, int *lat, int *lon, int *dx, int *dy, int *date, int *temps, int *flgs, int sup[], int nsup, int *lng);

int c_mrfput(int iun, int handle, void *buffer);

int  c_mrfvoi(int iun);

int c_mrbcvt(int liste[], int tblval[], float rval[], int nele, int nval, int nt, int mode);

int c_mrbcol(int liste[], int cliste[], int nele);

int c_mrbdcl(int cliste[], int liste[], int nele);

int c_mrbcov(int elem);

int c_mrbdcv(int elem);

int c_mrbsct(int tablusr[], int neleusr);

#endif
