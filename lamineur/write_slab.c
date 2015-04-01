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
/* write_slab.c
   original author: Karim Tegguiche
   Revision V2.0 - V.Lee July 1997
   Revision V3.0 - V.Lee March 1998 (to catch more bugs)
   Revision V4.0 - V.Lee November 1998 (slabini will not allow append)
   Revision V5.0 - V.Lee January 1999  (slabopt added for parallel code)
   Revision V6.0 - V.Lee Dec 2000(convip1 determines IP1, remove IP1 check)
*/

#include<rpnmacros.h>
#include<fcntl.h>
#include<stdio.h>
#include<string.h>
#include<stdlib.h>
#include <unistd.h>
#include "slab_old.h"


#define BUFSIZE 1048576
#define GRTYPES "ABGLNSXYZEH"

#define put_in_buffer(pVal,buffer,pos,nbre) for(i=0;i<nbre;i++){ \
	                                        buffer[pos] = *pVal;\
				 	        pos++;\
					        pVal++; } \

static int ERROR_LEVEL=-2;
static file_table_desc file_table[MAX_SLAB_FILES];
static Id_Block_file  id_block;
static Slab_Descrt_file slab_descrt;
static Data_Block_file data_block;
static Slab_End slab_end;

static int *intBuffer, pos;
static int *iVal;
static ftnword *pVal;

static float *fBuffer;
static ftnfloat *fVal;

static int init=0; 
static int proc0=1;
static int numproc=1;
static int f_index[MAX_SLAB_FILES];

/* prototypes declaration */

static void init_index();
static int slab_exit(int level);
static int get_free_index(int fd);
static int get_file_index(int fd);

ftnword f77name(slabini)(char *f_name, ftnword dateo[2], ftnword *f_npas,
		     ftnword *f_deet, char *f_etiket, int l1, int l2);
ftnword f77name(slabopt)(ftnword *f_proc, ftnword *f_numproc );
ftnword f77name(slabdsc)(ftnword *f_hand, ftnword *f_snum,char *f_gxtyp,
		     ftnword *f_ixyg1,ftnword *f_ixyg2,
		     ftnword *f_ixyg3, ftnword *f_ixyg4,ftnword *f_nio,
		     ftnword *f_njo,ftnword *f_nxgrid, 
		     ftnword *f_nygrid, ftnfloat *f_xgrid,ftnfloat *f_ygrid,
		     char *f_grtyp,ftnword *f_ig1,
		     ftnword *f_ig2,ftnword *f_ig3,ftnword *f_ig4,
		     ftnword *f_mtout, ftnword *f_np,
		     char *f_typvar,char *f_nomvar,ftnword *f_ip1,
		     ftnword *f_ip2, ftnword *f_ip3,
		     ftnword *f_datyp,ftnword *f_nbits,ftnword *iflt,
		     ftnfloat *f_xp, int l1, int l2, int l3, int l4);
ftnword f77name(slabxtr)(ftnword *f_hand, ftnword *f_snum, ftnword *f_nx,
		      ftnword *f_xnio,ftnword *f_mt,ftnword *f_mtas,
		      ftnfloat *f_mtadd, ftnfloat *f_mtmult, ftnfloat *f_mtval);
ftnword f77name(slabend)(ftnword *f_hand, char *f_sf_hand, int l1);


/***************************************************************************** 
 *                            S L A B _ E X I T                              *
 *                                                                           * 
 *Object                                                                     *
 *  To check level of error. If less or equal to ERROR_LEVEL, EXIT program   *
 *                                                                           * 
 *****************************************************************************/

static int slab_exit(int level)
  {
  
   if (level <= ERROR_LEVEL)
         exit(level);
   else 
       return(level);
 }


/***************************************************************************** 
 *                            I N I T _ I N D E X                            *
 *                                                                           * 
 *Object                                                                     *
 *  Initialize f_index values to all -1. Should only be used once.           *
 *                                                                           * 
 *****************************************************************************/

static void init_index()
  {
   int i;
   char *slab_config;
  
   if ( (slab_config=getenv("SLAB_CONFIG")) != NULL){
    ERROR_LEVEL=atoi(slab_config);
   printf("NOTE: ERROR_LEVEL set to %d\n",ERROR_LEVEL);
    }
   for(i=0;i<MAX_SLAB_FILES;i++)
       f_index[i] = -1;
   init = 1;
 }/* end init_index */

/***************************************************************************** 
 *                          G E T _ F R E E _ I N D E X                      *
 *                                                                           *
 *Object                                                                     *
 *  Return a free index entry in file table and record file information.     *
 *****************************************************************************/

static int get_free_index(int fd)
  {
   int i;
  
   for(i=0;i<MAX_SLAB_FILES;i++)
       if(f_index[i] == -1){
          f_index[i] = fd;
	  return(i);
          }

   fprintf(stderr,"\n***ERROR in GET_FREE_INDEX: slab file table is full\n");
   fprintf(stderr,"   MAX_SLAB_FILES = %d\n",MAX_SLAB_FILES);
   return(ERR_TAB_FULL);
   }

/***************************************************************************** 
 *                          G E T _ F I L E _ I N D E X                      *
 *                                                                           *
 *Object                                                                     *
 *  Return a file index entry in file table given the file handler           *
 *****************************************************************************/

static int get_file_index(int fd)
  {
   int i;
  
   for(i=0;i<MAX_SLAB_FILES;i++)
       if(f_index[i] == fd){
	  return(i);
          }

   fprintf(stderr,"\n***ERROR in GET_FILE_INDEX: slab file not initialized\n");
   return(ERR_NO_FILE);
   }

ftnword f77name(slabopt)(ftnword *f_proc, ftnword *f_numproc)
{
     numproc = (int) *f_numproc; 
     if ( (int ) *f_proc != 0)  proc0=0;
     return(proc0);
}
/***************************************************************************** 
 *                              S L A B I N I                                * 
 *Object                                                                     *
 *  Initialize slab file. Upon completion the function returns a file        * 
 *  index related to the file handler                                        *
 *                                                                           * 
 *Arguments                                                                  *
 *  IN   f_name   file name                                                  * 
 *  IN   dateo    origin date, dimension 2.                                  *
 *                dateo[0] AAAAMMJJ                                          * 
 *                dateo[1] HHMMSS00                                          *
 *  IN   npas     time step number                                           * 
 *  IN   deet     length of time step                                        * 
 *  IN   etiket   record identificator                                       * 
 *****************************************************************************/

ftnword f77name(slabini)(char *f_name, ftnword dateo[2], ftnword *f_npas,
			ftnword *f_deet, char *f_etiket, int l1, int l2)
{                                               
 int fd, ix, i, j, taille, npas;
 char name[MAX_LEN], etiket[MAX_LEN];


 if (init == 0) init_index();
 l1 = (l1 < MAX_LEN) ? l1 : MAX_LEN-1; 
 strncpy(name,f_name,l1);
 name[l1] = '\0';

 while ((name[l1-1] == ' ') && (l1 > 1)) {
   l1--;
   name[l1] = '\0';
 }

 l2 = (l2 < 12) ? l2 : 12; 
 strncpy(etiket,f_etiket,l2);
 etiket[l2] = '\0';
   
 while ((etiket[l2-1] == ' ') && (l2 > 1)) {
   l2--;
   etiket[l2] = '\0';
 }

 if((fd = open(name, O_RDWR | O_CREAT | O_EXCL ,0744)) == ERR_NO_FILE)
    {
     fprintf(stderr,"\n***ERROR in SLABINI: error opening file %s\n",name);
     slab_exit(-3);
     }

 ix = get_free_index(fd);
 if (ix == ERR_TAB_FULL) {
     fprintf(stderr,"\n***ERROR in SLABINI(%s): slab file table is full\n",name);
     return(slab_exit(-2));
     }

 strcpy(file_table[ix].file_name,name);
 for (j=0;j < MAX_SLAB_TYPES; j++){
 file_table[ix].count[j] = 0;
 file_table[ix].nrows[j] = 0;
 file_table[ix].nio_njo[j] = 0;
 }

 if ((intBuffer = (int *) malloc(BUFSIZE * sizeof(int)))==NULL)
    {
     fprintf(stderr,"\n***ERROR in SLABINI(%s): Cannot allocate memory for intBuffer\n",name);
     return(slab_exit(-3));
     }
 
 file_table[ix].buffer = intBuffer;
   
 id_block.slb0    = 'SLB0';
 id_block.nBytes  = 32;
 id_block.deet    = (int ) *f_deet;
 id_block.npas    = (int ) *f_npas;
 strcpy(id_block.etiket,etiket);
 id_block.dateo1  = (int ) dateo[0];
 id_block.dateo2  = (int ) dateo[1];
 id_block.val15   = 1.5;

 pos=0;

 iVal = (int *) &id_block;
 taille = (sizeof(id_block) / sizeof(int));
 if (proc0) put_in_buffer(iVal,intBuffer,pos,taille);

 file_table[ix].pos = pos;
 return (ftnword) fd;
}

/*****************************************************************************
 *                              S L A B D S C                                *
 *Object :                                                                   *
 *  Description slab file.                                                   *
 *Arguments :                                                                *
 *     IN   f_hand   :   file handler of slab file                           *
 *     IN   f_snum   :   slab number                                         *
 *     IN   f_gxtyp  :   grid type for >> and ^^                             *
 *     IN   f_ixyg1  :   grid descriptor 1 for >> and ^^                     *
 *     IN   f_ixyg2  :   grid descriptor 2 for >> and ^^                     *
 *     IN   f_ixyg3  :   grid descriptor 3 for >> and ^^                     *
 *     IN   f_ixyg4  :   grid descriptor 4 for >> and ^^                     *
 *     IN   f_nio    :   dimension x of grid                                 *
 *     IN   f_njo    :   dimension y of grid                                 *
 *     IN   f_nxgrid :   dimension of f_xgrid                                *
 *     IN   f_nygrid :   dimension of f_ygrid                                *
 *     IN   f_xgrid  :   >> fields (dim : f_nxgrid)                          *
 *     IN   f_ygrid  :   ^^ fields (dim : f_nygrid)                          *
 *     IN   f_grtyp  :   grid type (A,B,G,L,N,S,X,Y,Z,E,H)                   *
 *     IN   f_ig1    :   grid descriptor 1                                   *
 *     IN   f_ig2    :   grid descriptor 2                                   *
 *     IN   f_ig3    :   grid descriptor 3                                   *
 *     IN   f_ig4    :   grid descriptor 4                                   *
 *     IN   f_mtout  :   dimension of ip(1..3),nomvar,datyp,nbits and first  *
 *                       dimension of f_xp                                   *
 *     IN   f_np     :   second dimension of f_xp                            *
 *     IN   f_typvar :   type field                                          *
 *     IN   f_nomvar :   variable name                                       *
 *     IN   f_ip1    :   level descriptor                                    *
 *     IN   f_ip2    :   time descriptor                                     *
 *     IN   f_ip3    :   descriptor no 3                                     *
 *     IN   f_datyp  :   data type                                           *
 *     IN   f_nbits  :   bits number for each data field                     *
 *     IN   f_iflt   :   number of filter passes for each data field         *
 *     IN   f_xp     :   optional variables (dim : mtout,np)                 * 
 *                                                                           *
 *****************************************************************************/

ftnword f77name(slabdsc)(ftnword *f_hand, ftnword *f_snum,char *f_gxtyp,
		     ftnword *f_ixyg1,ftnword *f_ixyg2,
                     ftnword *f_ixyg3, ftnword *f_ixyg4,ftnword *f_nio,
		     ftnword *f_njo, ftnword *f_nxgrid,
                     ftnword *f_nygrid, ftnfloat *f_xgrid,ftnfloat *f_ygrid,
		     char *f_grtyp,ftnword *f_ig1,
		     ftnword *f_ig2,ftnword *f_ig3,ftnword *f_ig4,
		     ftnword *f_mtout, ftnword *f_np, 
                     char *f_typvar, char *f_nomvar,ftnword *f_ip1,
		     ftnword *f_ip2, ftnword *f_ip3,
                     ftnword *f_datyp,ftnword *f_nbits,ftnword *f_iflt,
		     ftnfloat *f_xp, int l1, int l2, int l3, int l4)
{
 int nrows, nio, njo, nxtra, 
     taille, i, x, MAX_GRTYP;
 int lng, n, nn, ix, snum;
 char grtyp[MAX_LEN], grtyp_[MAX_LEN];
 char *nomvars, *nomvars_0, *typvars, *typvars_0, *p_nomvar, *p_typvar;
 ftnword *p_ip1,*p_ip2,*p_ip3,*p_datyp,*p_nbits,*p_iflt;

/* for debug only 
  int ip1[20],ip2[20],ip3[20],datyp[20],nbits[20],iflt[20];
  ftnword *p_ip1,*p_ip2,*p_ip3,*p_datyp,*p_nbits,*p_iflt; 
*/

 MAX_GRTYP = strlen(GRTYPES);
 nio = (int ) *f_nio;
 njo = (int ) *f_njo;
 nrows = (int ) *f_mtout;
 nxtra = (int ) *f_np;
 if ( (ix = get_file_index( (int ) *f_hand)) < 0 ) return(slab_exit(-3));

 intBuffer = file_table[ix].buffer;
 pos = file_table[ix].pos;

 snum = (int ) *f_snum;
 
 if (snum < MAX_SLAB_TYPES && snum >=0){
       if (file_table[ix].nrows[snum] == 0){
           file_table[ix].nrows[snum] = nrows;
           file_table[ix].nio_njo[snum] = nio*njo;
           }
       else{
       fprintf(stderr,"\n***ERROR in SLABDSC(%s)slabid %d is already defined\n",
                       file_table[ix].file_name,snum);
       fprintf(stderr,"   mtout=%d, nio*njo=%d\n",
                       file_table[ix].nrows[snum],file_table[ix].nio_njo[snum]);
       fprintf(stderr,"   set to: mtout=%d, nio=%d njo=%d ??\n",
                       nrows,nio,njo);
       return(slab_exit(-2));
       }
       }
 else{
       fprintf(stderr,"\n***ERROR in SLABDSC(%s)slabid %d is out of range\n",file_table[ix].file_name,snum);
       fprintf(stderr,"  slabid MUST be from 0 to %d\n",MAX_SLAB_TYPES-1);
       return(slab_exit(-2));
     }
 if (nio < 1 || nio > 32767){
      fprintf(stderr,"\n***ERROR in SLABDSC(%s)slabid %d: invalid NIO=%d\n",file_table[ix].file_name,snum,nio); 
      return(slab_exit(-2));
      }
 if (njo < 1 || njo > 32767){
      fprintf(stderr,"\n***ERROR in SLABDSC(%s)slabid %d: invalid NJO=%d\n",file_table[ix].file_name,snum,njo); 
      return(slab_exit(-2));
      }
 if ((nomvars_0 = malloc(4 * nrows))==NULL)
    {
     fprintf(stderr,"\n***ERROR in SLABDSC(%s)slabid %d: Cannot allocate memory for nomvars\n",file_table[ix].file_name,snum);
     return(slab_exit(-3));
     }

 if ((typvars_0 = malloc(4 * nrows))==NULL)
    {
     fprintf(stderr,"\n***ERROR in SLABDSC(%s)slabid %d: Can not allocate memory for typvars\n",file_table[ix].file_name,snum);
     return(slab_exit(-3));
     }

 nomvars = nomvars_0;
 typvars = typvars_0;
 p_nomvar = f_nomvar;
 p_typvar = f_typvar;

 lng = (l4 < 4) ? l4 : 4;
 for (n=0; n < nrows; n++) {
   for (i=0; i < lng; i++) {
     *nomvars = *p_nomvar;
     nomvars++;
     p_nomvar++;
   }
   for (i=lng; i < 4; i++) {
     *nomvars = ' ';
     nomvars++;
   }
 }

 lng = (l3 < 4) ? l3 : 4;
 for (n=0; n < nrows; n++) {
   for (i=0; i < lng; i++) {
     *typvars = *p_typvar;
     typvars++;
     p_typvar++;
   }
   for (i=lng; i < 4; i++) {
     *typvars = ' ';
     typvars++;
   }
 }

/* No checks on ip1s */

/* check ip2s */
     p_ip2 = f_ip2;
     for (n=0; n< nrows; n++){
      if ( (int) *p_ip2 < 0 || (int) *p_ip2 > 32767){
      fprintf(stderr,"\n***ERROR in SLABDSC(%s)slabid %d: ip2[%d]=%d\n",file_table[ix].file_name,snum,n+1,(int) *p_ip2);
      return(slab_exit(-2));
      }
      p_ip2++;
      }

/* check ip3s */
     p_ip3 = f_ip3;
     for (n=0; n< nrows; n++){
      if ( (int) *p_ip3 < 0 || (int) *p_ip3 > 32767){
      fprintf(stderr,"\n***ERROR in SLABDSC(%s)slabid %d: ip3[%d]=%d\n",file_table[ix].file_name,snum,n+1,(int) *p_ip3);
      return(slab_exit(-2));
      }
      p_ip3++;
      }

/* check datyps */
     p_datyp = f_datyp;
     for (n=0; n< nrows; n++){
      if ( (int) *p_datyp < 0  || (int) *p_datyp > 5){
      fprintf(stderr,"\n***ERROR in SLABDSC(%s)slabid %d: datyp[%d]=%d\n",file_table[ix].file_name,snum,n+1,(int) *p_datyp);
      return(slab_exit(-2));
      }
      p_datyp++;
      }

/* check nbits */
     p_nbits = f_nbits;
     for (n=0; n< nrows; n++){
      if ( (int) *p_nbits < 0 || (int) *p_nbits >  32767){
      fprintf(stderr,"\n***ERROR in SLABDSC(%s)slabid %d: nbits[%d]=%d\n",file_table[ix].file_name,snum,n+1,(int) *p_nbits);
      return(slab_exit(-2));
      }
      p_nbits++;
      }

/* check iflts */
     p_iflt = f_iflt;
     for (n=0; n< nrows; n++){
      if ( (int) *p_iflt < 0 || (int) *p_iflt >  32767){
      fprintf(stderr,"\n***ERROR in SLABDSC(%s)slabid %d: iflt[%d]=%d\n",file_table[ix].file_name,snum,n+1,(int) *p_iflt);
      return(slab_exit(-2));
      }
      p_iflt++;
      }

/* for debug only 
     printf("INSIDE SLABDSC ix=%d\n",ix);
     printf("l1=%d,l2=%d,l3=%d,l4=%d\n",l1,l2,l3,l4);
     printf("nio=%d,njo=%d,snum=%d,nrows=%d, nxtra=%d\n",
                           nio,njo,snum,nrows,nxtra);
     printf("ip1= ");
     p_ip1 = f_ip1;
     for (n=0; n< nrows; n++){
      ip1[n] = (int )*p_ip1;
      p_ip1++;
      printf("%d,",ip1[n]);
      }
     printf("\n");
     printf("ip2= ");
     p_ip2 = f_ip2;
     for (n=0; n< nrows; n++){
      ip2[n] = (int )*p_ip2;
      p_ip2++;
      printf("%d,",ip2[n]);
      }
     printf("\n");
     printf("ip3= ");
     p_ip3 = f_ip3;
     for (n=0; n< nrows; n++){
      ip3[n] = (int )*p_ip3;
      p_ip3++;
      printf("%d,",ip3[n]);
      }
     printf("\n");
     printf("datyp= ");
     p_datyp = f_datyp;
     for (n=0; n< nrows; n++){
      datyp[n] = (int )*p_datyp;
      p_datyp++;
      printf("%d,",datyp[n]);
      }
     printf("\n");
     printf("nbits= ");
     p_nbits = f_nbits;
     for (n=0; n< nrows; n++){
      nbits[n] = (int )*p_nbits;
      p_nbits++;
      printf("%d,",nbits[n]);
      }
     printf("\n");
     printf("iflt= ");
     p_iflt = f_iflt;
     for (n=0; n< nrows; n++){
      iflt[n] = (int )*p_iflt;
      p_iflt++;
      printf("%d,",iflt[n]);
      }
     printf("\n");
     printf("typvars=%s\n",typvars_0);
     printf("nomvars=%s\n",nomvars_0); 
     printf("grtyp=%s,",f_grtyp);
     printf("nxgrid=%d,nygrid=%d\n",(int )*f_nxgrid,(int )*f_nygrid);
     printf("gxtyp=%s\n",f_gxtyp);
     printf("ig1=%d,ig2=%d,ig3=%d,ig4=%d\n",
           (int) *f_ig1,(int) *f_ig2,(int) *f_ig3,(int) *f_ig4);
     printf("ixyg1=%d,ixyg2=%d,ixyg3=%d,ixyg4=%d\n",
           (int) *f_ixyg1, (int) *f_ixyg2, (int) *f_ixyg3, (int) *f_ixyg4);
     printf("\n");
*/ 

 slab_descrt.slb1 = 'SLB1';
 slab_descrt.nBytes = 4 * (15 + nrows * (8 + nxtra) + (int) *f_nxgrid + (int) *f_nygrid);
 slab_descrt.slab_id = (int ) *f_snum;
  
 l3 = (l3 < MAX_LEN) ? l3 : MAX_LEN-1;
 strncpy(grtyp,f_grtyp,l3);
 grtyp[l3] ='\0';
 
/* check gridtype requested */
 if (l3 == 0 || *grtyp==' '){
      fprintf(stderr,"\n***ERROR in SLABDSC(%s)slabid %d: invalid GRTYP='%s'\n",file_table[ix].file_name,snum,grtyp);
      return(slab_exit(-2));
      }
 n=0;
 while (*grtyp != GRTYPES[n] && n < MAX_GRTYP) n++;
 if (*grtyp != GRTYPES[n]){
    fprintf(stderr,"\n***ERROR in SLABDSC(%s)slabid %d: invalid GRTYP='%s'\n",
            file_table[ix].file_name,snum,grtyp);
      return(slab_exit(-2));
      }
 strcpy(slab_descrt.grtyp,grtyp);
 
 slab_descrt.ig1    = (int ) *f_ig1;
 slab_descrt.ig2    = (int ) *f_ig2;
 slab_descrt.ig3    = (int ) *f_ig3;
 slab_descrt.ig4    = (int ) *f_ig4;
 slab_descrt.Nrows  = (int ) *f_mtout;
 slab_descrt.Niout  = (int ) *f_nio;
 slab_descrt.Njout  = (int ) *f_njo;
 slab_descrt.nxgrid = (int ) *f_nxgrid;
 slab_descrt.nygrid = (int ) *f_nygrid;
 slab_descrt.Nextra = (int ) *f_np;
 
 strcpy(slab_descrt.grtyp_,"    ");
 slab_descrt.ig1_   = -2;
 slab_descrt.ig2_   = -2;
 slab_descrt.ig3_   = -2;
 slab_descrt.ig4_   = -2;

 if(*f_grtyp == 'Z')
    {
      if(njo != (int ) *f_nygrid)
	{
	  fprintf(stderr,"\n***ERROR in SLABDSC(%s)slabid %d: nygrid should be equal to njo for Z grid\n",file_table[ix].file_name,snum);
  fprintf(stderr," nygrid = %d njo = %d\n",(int )*f_nygrid, njo);
	  return(slab_exit(-2));
	}
      if( (int)*f_nxgrid != nio && (int)*f_nxgrid != (nio+1) )
	{
  fprintf(stderr,"\n***ERROR in SLABDSC(%s)slabid %d: nxgrid should be equal to nio or (nio+1) for Z grid\n",file_table[ix].file_name,snum);
  fprintf(stderr," nxgrid = %d nio = %d\n",(int )*f_nxgrid, nio);
  return(slab_exit(-2));
        }
    }

 if(*f_grtyp == 'Y')
    {
    if( (nio*njo) != (int ) *f_nxgrid)
	{
	  fprintf(stderr,"\n***ERROR in SLABDSC(%s)slabid %d: nxgrid should be equal to nio*njo for Y grid\n",file_table[ix].file_name,snum);
  fprintf(stderr," nxgrid = %d nio = %d njo = %d\n",(int )*f_nxgrid, nio,njo);
	  return(slab_exit(-2));
	}
    if( (nio*njo) != (int ) *f_nygrid)
	{
	  fprintf(stderr,"\n***ERROR in SLABDSC(%s)slabid %d: nygrid should be equal to nio*njo for Y grid\n",file_table[ix].file_name,snum);
  fprintf(stderr," nygrid = %d nio = %d njo = %d\n",(int )*f_nygrid, nio,njo);
	  return(slab_exit(-2));
	}
     }
 if(*f_grtyp == 'Z' || *f_grtyp == 'Y'){
      l1 = (l1 < MAX_LEN) ? l1 : MAX_LEN-1;
      strncpy(grtyp_,f_gxtyp,l1);
      grtyp_[l1] = '\0';
      strcpy(slab_descrt.grtyp_,grtyp_);
      if (l1 == 0 || *grtyp_==' '){
      fprintf(stderr,"\n***ERROR in SLABDSC(%s)slabid %d: invalid GXTYP='%s'\n",file_table[ix].file_name,snum,grtyp_);
      return(slab_exit(-2));
      }
      
      slab_descrt.ig1_ = (int ) *f_ixyg1;
      slab_descrt.ig2_ = (int ) *f_ixyg2;
      slab_descrt.ig3_ = (int ) *f_ixyg3;
      slab_descrt.ig4_ = (int ) *f_ixyg4;
    }

 if (proc0) 
 {
 taille = (sizeof(slab_descrt) / sizeof(int));
 iVal = (int *) &slab_descrt;
 put_in_buffer(iVal,intBuffer,pos,taille);
 pVal = f_ip1;
 put_in_buffer(pVal,intBuffer,pos,nrows);
 pVal = f_ip2;
 put_in_buffer(pVal,intBuffer,pos,nrows);
 pVal = f_ip3;
 put_in_buffer(pVal,intBuffer,pos,nrows);
 pVal = f_nbits;
 put_in_buffer(pVal,intBuffer,pos,nrows);
 pVal = f_datyp;
 put_in_buffer(pVal,intBuffer,pos,nrows);
 iVal = (int *)nomvars_0;
 put_in_buffer(iVal,intBuffer,pos,nrows);
 iVal = (int *)typvars_0;
 put_in_buffer(iVal,intBuffer,pos,nrows);

 fBuffer = (float *) intBuffer;  

 if((*f_grtyp == 'Z')  || ( *f_grtyp == 'Y'))
    {
     fVal = f_xgrid;
     put_in_buffer(fVal, fBuffer, pos, *f_nxgrid);
     fVal = f_ygrid;
     put_in_buffer(fVal, fBuffer, pos, *f_nygrid);
     }

 pVal = f_iflt;
 put_in_buffer(pVal,intBuffer,pos,nrows);
 
 if(nxtra != 0)
   {
    taille = (int )(*f_mtout)  *  (int )(*f_np);
    fVal = f_xp;
    put_in_buffer(fVal,fBuffer,pos,taille);
    }
 }
 file_table[ix].pos = pos;
 free (nomvars_0);
 nomvars_0 = NULL;
 free (typvars_0);
 typvars_0 = NULL;
 return(0);
 }/* end slabdsc */

/****************************************************************************
 *                              S L A B X T R                               *
 *                                                                          *
 *Object:                                                                   *
 *       Extraction of valid data field into the slab file                  *
 *Arguments:                                                                *
 *     IN   f_hand   :    file handler of slab file                         *
 *     IN   f_snum   :    slab number                                       *
 *     IN   f_nx     :    f_xnio dimension                                  *
 *     IN   f_xnio   :    output indicator for grid position (dim : f_nx)   *
 *     IN   f_mt     :    mtas dimension                                    *
 *     IN   f_mtas   :    output indicator for row in slab  (dim: f_mt)     *
 *     IN   f_mtadd  :    values to add for each row in slab                *
 *     IN   f_mtmult :    values to multiply for each row in slab           *
 *     IN   f_mtval  :    raw slab; value of elements (dim : f_nx, f_mt)    *
 *                                                                          *
 ****************************************************************************/

ftnword f77name (slabxtr)(ftnword *f_hand, ftnword *f_snum, ftnword *f_nx,
		      ftnword *f_xnio,ftnword *f_mt,ftnword *f_mtas,
		      ftnfloat *f_mtadd, ftnfloat *f_mtmult, ftnfloat *f_mtval)
{                                               
 int i, j, ix, ij = 0, 
     taille,
     nrows = 0,
     nX = 0,
     nBytes, Nrows, Nx, fd, snum;
 int n;

 Nrows = (int ) *f_mt;
 Nx    = (int ) *f_nx; 
 snum  = (int ) *f_snum;
if ( (ix = get_file_index( (int ) *f_hand)) < 0 ) return(slab_exit(-3));
 fd = (int) *f_hand;
 intBuffer = file_table[ix].buffer; 
 pos = file_table[ix].pos;
 fBuffer = (float *) intBuffer;


 for(j = 0; j < Nrows; j++)
    if(f_mtas[j] != 0)
       nrows++;
 
 if(nrows != file_table[ix].nrows[snum])
   {
    fprintf(stderr,"***ERROR in SLABXTR(%s)slabid %d:\n",
             file_table[ix].file_name,snum);
    fprintf(stderr,"  nrows in mtas(=%d) must be equal to SLABDSC mtout(=%d)\n",
   nrows,file_table[ix].nrows[snum]);
    return(slab_exit(-2));
    }

 for(i = 0; i < Nx; i++)
    if(f_xnio[i] != 0)
       nX++;
 
 file_table[ix].count[snum] += nX;
 
 data_block.slb2 = 'SLB2';
 data_block.nBytes = 4 * (3 + nX * (1 + nrows));
 data_block.slab_id = (int ) *f_snum;
 data_block.nX = nX; 
 data_block.Nrows = nrows;
 
 iVal = (int *) &data_block;
 taille = (sizeof(data_block) / sizeof(int));

 put_in_buffer(iVal,intBuffer,pos,taille);

 for(i=0; i < Nx; i++)
    { 
     if(f_xnio[i] != 0)
       {
        if(pos >= BUFSIZE)
          {
	   taille = sizeof(float) * pos;
           nBytes =  write(fd, fBuffer, taille);
	   if(nBytes != (sizeof(float) *  BUFSIZE))
	      {
	       fprintf(stderr,"\n***ERROR in SLABXTR(%s)slabid %d: WRITE ERROR in slab file\n",file_table[ix].file_name,snum);
	       return(slab_exit(-2));
	       }
	   pos=0;
	   }
 	intBuffer[pos] = (int  ) f_xnio[i];
        pos++;
	}
     }/* end for */
 
 for(j = 0; j < Nrows; j++)
    {
     if(f_mtas[j] != 0)
       {
        if((pos + nX) <= BUFSIZE)
           {
            if(nX == Nx )  /* Si le nombre de colonnes a     */
              {            /* extraire est egale au nombre   */
               for(i = 0; i < Nx; i++) /* de colonnes de chaque slab     */
                  {                                            
                   fBuffer[pos] = (float)(f_mtval[ij]*f_mtmult[j] + f_mtadd[j]);
                   pos++;
                   ij++;

                   }/* end for */
                }
            else{         /* Si le nombre de colonnes a     */
                 for(i = 0; i < Nx; i++)  /* extraire est different a celui */
                    {                     /* de slab                        */
                     if(f_xnio[i] != 0)   /* je cherche les valeurs valides */
                       {                  /* une par une                    */
                        fBuffer[pos] = (float)(f_mtval[ij]*f_mtmult[j] + f_mtadd[j]);
                        pos++;
                        }
                     
                      ij++;

                    }/* end for */

                 }

            }
        else{                             /* Si la taille qui reste dans le */
             for(i = 0; i < Nx; i++)      /* buffer est inssufisante pour   */
                {                         /* le reste des donnees que j'ai  */
                 if(pos >= BUFSIZE)
		   {     
                    taille = sizeof(float) * pos;  
                    /* alors, on ecrit le buffer dans */
                    nBytes =  write(fd, fBuffer, taille);
		    if(nBytes != (sizeof(float) *  pos))
		       {
		        fprintf(stderr,"\n***ERROR in SLABXTR(%s)slabid %d: WRITE ERROR in slab file\n",file_table[ix].file_name,snum);
			return(slab_exit(-2));
			}
	            pos = 0;             /* le fichier slab et on met le   */
                                         /* buffer a zero                  */
		    }

                 if(f_xnio[i] != 0)      /* a extraire,                    */
                   {                                          
                    fBuffer[pos] =(float)(f_mtval[ij]*f_mtmult[j] + f_mtadd[j]);
                    pos++;
                    }
                 
                 ij++;

                 }/* end for */
             
             }

        }
     
     else ij += Nx;

     }/* end for */

 file_table[ix].pos = pos;
 return(0);
 }/* end slabxtr */

/*****************************************************************************
 *                              S L A B E N D                                *
 * Object :                                                                  *
 *         Put an end slab indicator into a file                             *
 * Arguments :                                                               *
 *            IN   f_hand : file handler of slab file                        *
 *            IN   sf_end : section or file end indicator                    *
 *                                                                           *
 *****************************************************************************/

ftnword f77name(slabend)(ftnword *f_hand, char *f_sf_hand, int l1)
 {
  int end, taille, i, ix, fd; 
  int nBytes;
  int n;

  if ((ix = get_file_index( (int ) *f_hand)) < 0 ) return(slab_exit(-3));
  fd = (int) *f_hand;
  intBuffer = file_table[ix].buffer;
  pos = file_table[ix].pos;
  fBuffer = (float *) intBuffer;

  end = (f_sf_hand[0] << 24) | (f_sf_hand[1]  << 16) | 
                       (f_sf_hand[2] << 8) | (f_sf_hand[3]);
  for (i=0; i < MAX_SLAB_TYPES; i++)
  /* check to see if number of values written equal to what is requested */
  /* this is only a valid check if one CPU is used */
  if(file_table[ix].nio_njo[i] != file_table[ix].count[i] && numproc == 1 )
     {
      fprintf(stderr,"\n***ERROR in SLABEND(%s)slabid %d\n",
                                    file_table[ix].file_name,i);
      fprintf(stderr,"   Value of nio*njo must be equal to number of valid values in xnio\n");
      fprintf(stderr,"   No. of selected elements in xnio = %d, nio*njo=%d\n",
                      file_table[ix].count[i],file_table[ix].nio_njo[i]);
      return(slab_exit(-2));   
      }

  slab_end.id_end = end;

  if(slab_end.id_end != 'SLB9')
     {
      fprintf(stderr,"\n***ERROR in SLABEND(%s):end indicator of slabend must be -> SLB9\n\n",file_table[ix].file_name);
      return(slab_exit(-2));
      }

  iVal = (int *) &slab_end;

  taille = sizeof(slab_end) / sizeof(int);
  /* add SLB9 if only one CPU is used */
  if (numproc == 1) put_in_buffer(iVal,intBuffer,pos,taille);

  if(pos == 0)
     {
      taille=sizeof(slab_end);
      nBytes = write(fd,intBuffer,taille);
      if(nBytes != taille)
	 {
          fprintf(stderr,"\n***ERROR in SLABEND(%s): WRITE ERROR in slab file\n",file_table[ix].file_name);
	  return(slab_exit(-2));
	  }
      }
  
  else{
       taille = sizeof(int) * pos;
       nBytes = write(fd,intBuffer,taille);
       if(nBytes != taille)
	  {
	  fprintf(stderr,"\n***ERROR in SLABEND(%s): WRITE ERROR in slab file\n",file_table[ix].file_name);
	  return(slab_exit(-2));
	  }
       
       }

  free(intBuffer);
  file_table[ix].buffer = NULL;
  file_table[ix].pos = 0;
   for (i=0;i < MAX_SLAB_TYPES; i++){
       file_table[ix].count[i] = 0;
       file_table[ix].nrows[i] = 0;
       file_table[ix].nio_njo[i] = 0;
       }
  f_index[ix] = -1; /* reset to -1 */
  if (numproc == 1) return(0); /* indicates that SLB9 was added */
  else return(1); /* indicates that SLB9 was not added */
  }/* end slabend */
