/* RMNLIB - Library of useful routines for C and FORTRAN programming
 * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
 *                          Environnement Canada
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/*splitpoint a000000 */
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include "qstdir.h"

file_table_entry_ptr file_table[MAX_XDF_FILES]; /* file table , exported symbol */

static char errmsg[1024];     /* buffer to format error messages */
static int msg_level=INFORM;  /* error tolerance before error message is issued */
static int xdf_toler=ERROR;   /* error tolerance before program is aborted */
static stdf_rec_parms params; /* collection area for record parameters */
static key_descriptor truc[] = {
   { 'STI1', 7,7,33,0},
   { 'STI2',15,7,33,0},
   { 'STI3',23,7,33,0},
   { 'STI4',31,7,33,0},
   { 'STI5',39,7,33,0},
   { 'STI6',47,7,33,0},
   { 'STI7',55,7,33,0},
   { 'STI8',63,7,33,0},
   { 'STI9',71,7,33,0},
   { 'FLGS',95,23,0,0},
   { 'LATI',111,15,0,0},
   { 'LONG',127,15,0,0},
   { 'DATE',147,19,0,0},
   { 'DX  ',159,11,0,0},
   { 'IDTP',167,7,0,0},
   { 'DY  ',179,11,0,0},
   { 'HEUR',185,5,0,0},
   { 'MIN ',191,5,0,0}
};

static key_descriptor auxkey[] = {
   { 'NBLK',15,15,0,0},
   { 'OARS',31,15,0,0},
   { 'ELEV',44,12,0,0},
   { 'DRCV',55,10,0,0},
   { 'RUNN',63,7,0,0},
};

/* prototypes declaration */

static int error_msg(char *function_name, int errcode, int errlevel);
static int file_index(int iun);
static int get_free_index();
static int fnom_index(int iun);
static void init_file(int i);
static void init_package();
static long scan_random(int file_index);
static long add_dir_page(int file_index,int wflag);
static long rewind_file(int file_index, int handle);
static int create_new_xdf(int index, int iun, ftnword_2 *pri, int npri,
                          ftnword_2 *aux, int naux, char *appl);
ftnword f77name(xdfopn)(ftnword *fiun,char *mode,ftnword_2 *pri,ftnword *fnpri,
             ftnword_2 *aux,ftnword *fnaux,char *appl,F2Cl l1,F2Cl l2);
int c_xdfopn(int iun,char *mode,ftnword_2 *pri,int npri,
             ftnword_2 *aux,int naux,char *appl);
ftnword f77name(xdfcls)(ftnword *fiun);
int c_xdfcls(int iun);
ftnword f77name(xdfsta)(int *fiun,ftnword *stat,int *fnstat,
                    ftnword_2 *pri,int *fnpri,ftnword_2 *aux,int *fnaux,
                    char *vers,char *appl,F2Cl l1,F2Cl l2);
int c_xdfsta(int iun,ftnword *stat,int nstat,
                    ftnword_2 *pri,int npri,ftnword_2 *aux,int naux,
                    char *vers,char *appl);
ftnword f77name(xdfimp)(int *fiun,ftnword *stat,int *fnstat,
                    ftnword_2 *pri,ftnword_2 *aux,
                    char *vers,char *appl,F2Cl l1,F2Cl l2);
int c_xdfimp(int iun,ftnword *stat,int nstat,ftnword_2 *pri,ftnword_2 *aux,
                    char *vers,char *appl);
ftnword f77name(xdfini)(int *fiun,buffer_interface_ptr buf,int *fidtyp,
                    ftnword *keys,int *fnkeys,ftnword *info,int *fninfo);
int c_xdfini(int iun,buffer_interface_ptr buf,int idtyp,
             ftnword *keys,int nkeys,ftnword *info,int ninfo);
static void build_burp_prim_keys(burp_record *brpk, ftnword *keys,
                                 burp_record *mask, int mode);
static void build_burp_info_keys(burp_record *brpk, ftnword *keys,int mode);
ftnword f77name(xdfadd)(buffer_interface_ptr buf,ftnword *donnees,
                        ftnword *fnelm, ftnword *fnbits, ftnword *fdatyp);
int c_xdfadd(buffer_interface_ptr buf,ftnword *donnees,
             int nelm, int nbits, int datyp);
static void scan_dir_page(file_table_entry *f);
static word next_match(int file_index);
ftnword f77name(xdfprm)(ftnword *fhandle,ftnword *addr,ftnword *lng,
                        ftnword *idtyp,ftnword *primk,int *fnprim);
int c_xdfprm(int handle,int *addr,int *lng,int *idtyp,word *primk,int nprim);
ftnword f77name(xdfhdr)(buffer_interface_ptr buf,ftnword *addr,ftnword *lng,
                        ftnword *idtyp,ftnword *primk,ftnword *fnprim,
			ftnword *info, ftnword *fninfo);
int c_xdfhdr(buffer_interface_ptr buf ,int *addr,int *lng,int *idtyp,
	     word *primk,int nprim,word *info,int ninfo);
ftnword f77name(xdfloc)(int *fiun, word *fhandle, ftnword *primk,int *fnprim);
int c_xdfloc(int iun, int handle, word *primk,int nprim);
ftnword f77name(xdfget)(ftnword *fhandle, buffer_interface_ptr buf);
int c_xdfget(int handle, buffer_interface_ptr buf);
ftnword f77name(xdfput)(ftnword *fiun, ftnword *fhandle,
			buffer_interface_ptr buf);
int c_xdfput(int iun, int handle, buffer_interface_ptr buf);
ftnword f77name(xdfopt)(char *foptname, char *foptc, ftnword *foptv,
			F2Cl l1, F2Cl l2);
int c_xdfopt(char *optname, char *optc, int optv);
ftnword f77name(xdfgop)(char *foptname, char *foptc, ftnword *foptv,
			F2Cl l1, F2Cl l2);
int c_xdfgop(char *optname, char *optc, int *optv);
ftnword f77name(xdfins)(buffer_interface_ptr buf,ftnword *donnees,
                        ftnword *fbitpos, ftnword *fnelm,
			ftnword *fnbits, ftnword *fdatyp);
int c_xdfins(buffer_interface_ptr buf, ftnword *donnees, int bitpos,
             int nelm, int nbits, int datyp);
ftnword f77name(xdfxtr)(buffer_interface_ptr buf,ftnword *donnees,
                        ftnword *fbitpos, ftnword *fnelm,
			ftnword *fnbits, ftnword *fdatyp);
int c_xdfxtr(buffer_interface_ptr buf, void *data, int bitpos,
             int nelm, int nbits, int datyp);
ftnword f77name(xdfrep)(buffer_interface_ptr buf,ftnword *donnees,
                        ftnword *fbitpos, ftnword *fnelm,
			ftnword *fnbits, ftnword *fdatyp);
int c_xdfrep(buffer_interface_ptr buf, void *data, int bitpos,
             int nelm, int nbits, int datyp);
ftnword f77name(xdfcut)(buffer_interface_ptr buf,
			ftnword *fbitpos, ftnword *fnelm,
			ftnword *fnbits, ftnword *fdatyp);
int c_xdfcut(buffer_interface_ptr buf, int bitpos, int nelm, int nbits,
	     int datyp);
ftnword f77name(xdfupd)(int *fiun,buffer_interface_ptr buf,int *fidtyp,
                    ftnword *keys,int *fnkeys,ftnword *info,int *fninfo);
int c_xdfupd(int iun,buffer_interface_ptr buf,int idtyp,
             ftnword *keys,int nkeys,ftnword *info,int ninfo);
ftnword f77name(xdfuse)(int *fsrc_unit, int *fdest_unit);
int c_xdfuse(int src_unit, int dest_unit);

/*splitpoint aaamain */
/***************************************************************************** 
 *                              M A I N                                      * 
 *****************************************************************************/

main(Void)
{
stdf_dir_keys keys_entry;
stdf_dir_info info_entry;
int i,j,k,nij,nk,bits,missing,ier,iun,handle,addr,lng,idtyp;
float srce[1000];
long dest[2000];
char vers[5], appl[5];
int stat[12];
ftnword buf[100];
double date0= 19940914.123341;
word keys[18]={-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,14980,32030,-1,0,36,0,-1,-1};
word keyz[18]={-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,136,0,-1,-1};

/*  word ncle:32, bit1:13, lcle:5, tcle:6, reserved:8 */
printf("-----------------------------------------\n");
for (i=0;i<18;i++){
j=truc[i].ncle;
printf("%-4.4s:%d:%d:%d:%d\n",&j,truc[i].bit1,truc[i].lcle,truc[i].tcle,truc[i].reserved);
}
printf("-----------------------------------------\n");
for (i=0;i<5;i++){
j=auxkey[i].ncle;
printf("%-4.4s:%d:%d:%d:%d\n",&j,auxkey[i].bit1,auxkey[i].lcle,auxkey[i].tcle,auxkey[i].reserved);
}
printf("-----------------------------------------\n");
for (i=0 ; i<1000 ; i++) srce[i] = i; srce[999] = 65534.0 ;
nij=1000;
nk=1;
bits=16;
missing=0;

params.aammjj = date0;
date0 = date0 - params.aammjj;
date0 = 0.25+date0*1000000;
params.hhmmss = date0;

i=sizeof(stdf_dir_keys);
j=sizeof(stdf_dir_info);
k=sizeof(base_dir_page);
printf("integer date is %ld:%ld\n",params.aammjj,params.hhmmss);
printf("size of dir_keys is %ld\n",i);
printf("size of file_header is %ld\n",sizeof(file_header));
printf("size of file_table_entry is %ld\n",sizeof(file_table_entry));
printf("size of dir_info is %ld\n",j);
printf("size of base directory_page is %ld\n",k);
printf(" uppercase of a is %c\n",upper_case('a'));
printf(" ascii64 of a is %c\n",ascii64('a'));
iun = 99;

/* TEST POUR XDFOPN, XDFCLS */
ier = c_fnom(iun,"new_xdf_burp_file","RND",0);
ier = c_xdfopn(iun,"CREATE",(ftnword_2 *)&truc,18,(ftnword_2 *)&auxkey,5,"BRP0");
ier = c_xdfcls(iun);

/* TEST POUR XDFSTA */
ier = c_xdfsta(iun,(ftnword *)&stat,12,(ftnword_2 *)&truc,18,(ftnword_2 *)&auxkey,5,vers,appl);

/* TEST POUR REOUVERTURE VERIFICATION DE LE CREATION */
ier = c_xdfopn(iun,"READ",(ftnword_2 *)&truc,18,(ftnword_2 *)&auxkey,5,"BRP0");
fprintf(stdout,"Debug reouverture du fichier new nrec =%d\n",ier);
ier = c_xdfcls(iun);

ier = c_fnom(12,"/data/dormrb3/armn/_si/lib/workfile","RND+OLD+R/W",0);
/*ier = c_fnom(12,"/data/local/tmpdirs/armnlib/workfile","rnd+burp+OLD+r/w",0);*/
if (ier != 0) exit(10);
/*ier = c_xdfopn(12,"READ",(ftnword_2 *)&truc,18,(ftnword_2 *)&auxkey,5,"BRP0");*/
ier = c_xdfopn(12,"R-W",(ftnword_2 *)&truc,18,(ftnword_2 *)&auxkey,5,"BRP0");
fprintf(stdout,"Debug nombre d'enregistrements =%d\n",ier);

/* TEST POUR XDFLOC */
keys[0] = 'O';
keys[1] = 'Z';
keys[2] = 'J';
keys[3] = 'P';
handle = c_xdfloc(12,0,keys,18);

/* TEST POUR XDFPRM */
ier = c_xdfprm(handle,&addr,&lng,&idtyp,keys,18);
  for (i=0; i<9; i++)
     fprintf(stdout,"%c",keys[i]);
  fprintf(stdout," %6.6x ",keys[9]);
  for (i=10; i<18; i++)
     fprintf(stdout," %6d",keys[i]);
  fprintf(stdout," %3d %3d %6d ",idtyp,lng,addr);
  fprintf(stdout,"page# %2d rec# %3d\n",
	  PAGENO_FROM_HANDLE(handle),RECORD_FROM_HANDLE(handle));

/* TEST POUR XDFGET */
buf[0]=100;
ier = c_xdfget(handle,(buffer_interface_ptr) buf);
for (i=0; i<50; i++)
   fprintf(stdout,"Debug buf[%d]=%d\n",i,buf[i]);

/* TEST POUR XDFPUT */
ier = c_xdfput(12,-handle,(buffer_interface_ptr) buf);

/* TEST POUR XDFLOC MODE SUIVANT ET XDFDEL */
handle = 0;
handle = c_xdfloc(12,handle,keyz,18);
while (handle >= 0) {
  ier = c_xdfprm(handle,&addr,&lng,&idtyp,keys,18);
  for (i=0; i<9; i++)
     fprintf(stdout,"%c",keys[i]);
  fprintf(stdout," %6.6x ",keys[9]);
  for (i=10; i<18; i++)
     fprintf(stdout," %6d",keys[i]);
  fprintf(stdout," %3d %3d %6d ",idtyp,lng,addr);
  fprintf(stdout,"page# %2d rec# %3d\n",
	  PAGENO_FROM_HANDLE(handle),RECORD_FROM_HANDLE(handle));
  ier = c_xdfdel(handle);
  handle = c_xdfloc(12,-1,keyz,0);
  }
fprintf(stdout,"Debug recherche termine avec handle = %d\n",handle);
/* ier = c_fnom(33,"/data/local/tmpdirs/armnlib/newburp","rnd+burp+r/w",0); */
ier = c_fnom(33,"/data/dormrb3/armn/_si/lib/newburp","RND+R/W",0);
ier = c_xdfcls(12);
/* TEST POUR XDFUSE */
ier = c_xdfuse(12,33);
ier = c_xdfcls(33);

ier = c_fclos(12);
ier = c_fclos(iun);
}

