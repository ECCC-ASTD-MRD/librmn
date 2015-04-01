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
#include<stdio.h>
#include<fcntl.h>
#include<stdlib.h>
#include<string.h>
#include<ctype.h>
#include <rpnmacros.h>
#include "slab.h"

/*Author: Karim Tegguiche
 *
 *Revision V2.0 - M. Lepine - lecture a partir d'un fichier "piped"
 *Revision V3.0 - V. Lee  July 1997
 *Revision V4.0 - V. Lee  Jan.1998 - fixed label for 8 char output,
 *                uses "newdate" function
 *Revision Y2K  - V. Lee  Aug.1998 - compiled with $ARMNLIB/lib/librmnbeta.a
 *Revision V5.0 - M. Lepine - Rechargement avec librmn32stack_LR.a
 *                V. Lee    - Descripteurs de grille a 32 bits (Oct 1998)
 *Revision V6.0 - V. Lee  June 1999 - data overflow error in field is replaced
 *                                    a NAN value 0xffffffff instead
 *Revision V7.0 - M.Valin Jan 2001 - added support for # type grids and 
 *          endianness correction revised usage of block structures and memory
 *Revision V8.0 - M.Valin Jan 2001 - correction on read_buf when bytes read
 *          are shorter than anticipated, make sure they get endian checked
 *          before they are returned.
 *Revision V8.1 - V.Lee Oct 2001   - allow 12 characters for ETIKET
 *
 */

typedef struct  {
   int identf, nBytes;
   }Id_Block;

struct block2 { /*  SLB1 header */
   int   slab_id, ig1, ig2, ig3, ig4, Nrows, Niout, Njout, 
         nxgrid, nygrid, Nextra, ig1_, ig2_, ig3_, ig4_;
   char  grtyp[4] ,grtyp_[4];
   int   *ptr_ip1, *ptr_ip2, *ptr_ip3, *iflt, *ptr_nBits, *p_datyp;
   char  *p_nomvar, *p_typvar;
   float *p_xpos, *p_ypos, *p_xtra, *ZOUT;
   int   count, NIout, NJout; 
   }block2_dsc[MAX_SLAB_TYPES], block2_tmp, *p_block2;;

struct block3 { /*  SLB2 header */
   int   slab_id, nX, Nrows, *p_hxpos;
   float *p_datxtr;
   }block3_dat;

static Id_Block_file  block1;  /*  SLB0 header */
static unsigned INT_32  *ptr_IEEE_IBM=(unsigned INT_32  *)&block1.val15;

static Slab_Descrt_file slab_dsc;
static Data_Block_file data_block;
static Slab_End slab_end;

static Id_Block id_blck;

static float *p_ZOUT; 
static int iun = 2;
static int list[10];
static int k = 0;
static int j = 0;
static int newdate_mode=3;

static int entier_quelconque=1;
static char *little_endian=(char *)&entier_quelconque; 

/* Prototypes des fonctions */
void transfert_IBM_IEEE( unsigned INT_32 *source, int nelements); 



/*****************************************************************************
 *                              R E A D _ B U F                              *
 *                                                                           *
 *Object                                                                     *
 * read from file / socket / pipe to integer buffer                          *
 *                                                                           *
 *****************************************************************************/
static int read_buf(int fd, void *Ibuffer, int nbytes)
{
  unsigned INT_32 *buffer=(unsigned INT_32 *)Ibuffer;
  int n;
  int nread;
  int really_read=0;
  char *cbuf=(char *)buffer;
  int nitems=nbytes/sizeof(INT_32);
 
  n=nitems*sizeof(INT_32);
  while(n>0){
    nread=read(fd,cbuf,n);
    if(nread<=0) break;
    really_read+=nread;
    n-=nread;
    cbuf+=nread;
  }

  if(really_read == 0) return(-1);

  nitems = really_read/sizeof(INT_32);
  n=nitems;
  if(*little_endian){  /* slab files are BIG ENDIAN */
    unsigned INT_32 *tmpbuf=buffer;
    while(n--){
     unsigned INT_32 temp=*tmpbuf;
     *tmpbuf=SWAP32(temp);
     tmpbuf++;
    }
  }
  return(nitems*sizeof(INT_32));
}

static void swap_words(void *what, int nwords){
  unsigned INT_32 *What=(unsigned INT_32  *)what;
  while(nwords--){
    unsigned INT_32 temp=*What;
    *What=SWAP32(temp);
    What++;
  }
}

/**************************************************
       A L L O C A T E _ B L O C K 
  allocate a block of memory, print error
  message and abort if unsuccessful
**************************************************/
INT_32 *allocate_block(int size, char *errmsg){
  INT_32 *temp=(INT_32 *)malloc(size);
#ifdef DEBUG
  printf("Allocating %s, %d bytes\n",errmsg,size);
#endif
  if(temp == NULL) {
    fprintf(stderr,"Cannot allocate memory for %s\n",errmsg);
    exit(1);
  }
  return(temp);
}

void convert_to_IEEE(void *source, int nelements){
  if (*ptr_IEEE_IBM != 0x3fc00000) { 
    if (*ptr_IEEE_IBM == 0x41180000) {
       transfert_IBM_IEEE((unsigned INT_32 *) source, nelements);
    } else{
       fprintf(stderr,"Floating points in data field ");
       fprintf(stderr,"are not IBM/IEEE\n");
       exit(1);
    }
  }       /* transferer les points           */
          /*  flottants IBM en IEEE.         */
}

f77name(delamineur)(int argc, char *argv[]) {
 int fd;
#ifdef DEBUG
 printf("delamineur running with DEBUG flag\n");
#endif
 if ((f77name(fst_version)())/100 >= 2000) {
     printf("\nDelamineur2000 (version 8.1)\n\n");
      }
 else {
     printf("\nDelamineur_Y2K+ (version 8.1)\n\n");
      }

 if((sizeof(int) != 4) || (sizeof(float) != 4)) {
     printf(" Inproper size for INT/FLOAT\n");                     
     exit(1);
 }
 

 if(argc != 3) {
     fprintf(stderr," Usage: delamineur slab_file std_file\n");
     exit(1);
 }

 if(strcmp(argv[1],"-") == 0)    /* lecture sur stdin */
   fd = 0;
 else {                          /* ouverture du fichier a lire */
   if((fd = open(argv[1],O_RDONLY)) == -1) {                  
       fprintf(stderr,"error opening file %s\n",argv[1]);
       exit(1);
   }
 }
 delamineur2(fd,argv[2],argv[1]);
 return(0);

}

int delamineur2(int fd, char *std_file_name, char *slab_file_name){

 int  taille, taille1,i, dateo, deet, i_len, ni, nj, nk = 1, npas, ier, nb_data, date;
 int nbytes, l;

 int n1, n2, n3, n4, n5, n6;
 
 int fd1;

 unsigned INT_32 *p_xpos, *p_ypos, *p_xtra;

 for(i=0 ; i<MAX_SLAB_TYPES ; i++) block2_dsc[i].slab_id = -1;

 k=1;  /* filtering is deactivated permanently because of possible tile processing */
 list[0]=1;

 nbytes = read_buf(fd,&block1,sizeof(block1));     /* read file header SLB0 block */
 if(* little_endian)swap_words(block1.Ietiket,3);  /* back to character stream layout */

 f77name(newdate)(&date,&block1.dateo1,&block1.dateo2,&newdate_mode);

#ifdef DEBUG
 printf("dateo1=%d dateo2=%d, date=%d\n",block1.dateo1,block1.dateo2,date);
 printf("deet=%d,npas=%d,etiket=%s\n",block1.deet,block1.npas,(char *)block1.Ietiket);
 printf("strlen(etiket)=%d\n",strlen((char *)block1.Ietiket));
#endif

 if(block1.slb0 != 'SLB0') {
     fprintf(stderr," %s ====> is not a slab file\n",slab_file_name);
     exit(1);
 }

#ifdef DEBUG
    printf("SLB0 detected\n");
#endif

 /* open standard file */

 iun = 1;
 ier = c_fnom(iun,std_file_name,"STD+RND",0); /* ouvrir le fichier standard  */
 if(ier != 0) {
     fprintf(stderr,"Error opening file %s\n", std_file_name);
     exit(1);
  }
  ier = c_fstouv(iun,"STD+RND+CREATE");
  if(ier < 0) {
     fprintf(stderr,"can not open unit %d",iun);
     exit(1);
  }
 
  l=0;

 nbytes = read_buf(fd,&id_blck,sizeof(id_blck));  /* read id of next block */

 i=0;
 while(id_blck.identf != 'SLB9') {
#ifdef DEBUG
     printf("BLOCK Header =%c%c%c%c nbytes=%d\n",id_blck.identf>>24,id_blck.identf>>16 & 0xFF,
             id_blck.identf>>8 & 0xFF,id_blck.identf & 0xFF, nbytes);
#endif

     switch(id_blck.identf) {
        case 'SLB1' :  i++;

#ifdef DEBUG
     printf("SLB1 detected\n");
#endif
                   p_block2 = &block2_tmp;
                   nbytes= read_buf(fd,p_block2,sizeof(slab_dsc) - sizeof(id_blck));
                   if(* little_endian)swap_words(p_block2->grtyp,1);  /* back to character stream layout */
                   if(* little_endian)swap_words(p_block2->grtyp_,1);  /* back to character stream layout */

                   if(p_block2->grtyp[0] == '#') {  /* # grid, get size of grid from ig3, ig4 */
                     p_block2->NIout = (p_block2->ig3 >> 20) & 0xFFF;
                     p_block2->NJout = (p_block2->ig4 >> 20) & 0xFFF;
                   }else{                           /* otherwise, use Niout, Njout */
                     p_block2->NIout = p_block2->Niout;
                     p_block2->NJout = p_block2->Njout;
                   }
                   p_block2->count = p_block2->NIout * p_block2->NJout;
#ifdef DEBUG
     printf("Grid block size is %d elements\n",p_block2->count);
#endif
                   taille = 4 * p_block2->Nrows;
                   /* Les allocations dynamiques     */

                   p_block2->ptr_ip1 =  (int *) allocate_block(taille,"ptr_ip1");
	           nbytes=read_buf(fd,p_block2->ptr_ip1, taille);

                   p_block2->ptr_ip2 =  (int *) allocate_block(taille,"ptr_ip2");
                   nbytes=read_buf(fd,p_block2->ptr_ip2, taille);

                   p_block2->ptr_ip3 =  (int *) allocate_block(taille,"ptr_ip3");
                   nbytes=read_buf(fd,p_block2->ptr_ip3, taille);

                   p_block2->ptr_nBits =  (int *) allocate_block(taille,"ptr_nBits");
                   nbytes=read_buf(fd,p_block2->ptr_nBits, taille);

                        /* recuperer tous les donnees      */
                        /* pour ecrire le fichier std      */
                   p_block2->p_datyp =  (int *) allocate_block(taille,"p_datyp");
                   nbytes=read_buf(fd,p_block2->p_datyp , taille);

                   p_block2->p_nomvar = (char *) allocate_block(taille+4,"p_nomvar");
                   nbytes=read_buf(fd,p_block2->p_nomvar,taille);
                   if(* little_endian)swap_words(p_block2->p_nomvar,p_block2->Nrows);

                   p_block2->p_typvar = (char *) allocate_block(taille+4,"p_typvar");
                   nbytes=read_buf(fd,p_block2->p_typvar,taille);
                   if(* little_endian)swap_words(p_block2->p_typvar,p_block2->Nrows);

                   /* si grtyp est 'Z' ou 'Y' ou '#'        */
                   if(strchr("ZY#",p_block2->grtyp[0])) {
		      taille = 4 * p_block2->nxgrid;
	              p_block2->p_xpos = (float *) allocate_block(taille,"p_xpos");
                      nbytes=read_buf(fd,p_block2->p_xpos,taille);

                      convert_to_IEEE(p_block2->p_xpos,p_block2->nxgrid);

		      taille = 4 * p_block2->nygrid;
	              p_block2->p_ypos = (float *) allocate_block(taille,"p_ypos");
                      nbytes=read_buf(fd, p_block2->p_ypos, taille);

                      convert_to_IEEE(p_block2->p_ypos,p_block2->nygrid);
#ifdef DEBUG
          printf("Printing read >> and ^^ descriptors, sizes= %d , %d\n",p_block2->nxgrid,p_block2->nygrid);
          printf(" >> \n");
          { int nn = p_block2->nxgrid ; float *xx=(float *)p_block2->p_xpos; while(nn--) { printf(" %f ",xx[nn]);}}
          printf("\n ^^ \n");
          { int nn = p_block2->nygrid ; float *xx=(float *)p_block2->p_ypos; while(nn--) { printf(" %f ",xx[nn]);}}
#endif
                   }
        
                   taille = 4 *  p_block2->Nrows;
                   p_block2->iflt = (int *) allocate_block(taille,"iflt");
                   nbytes=read_buf(fd,p_block2->iflt,taille);
#ifdef DEBUG
              printf("p_block2->slab_id=%d\n",p_block2->slab_id);
              printf("p_block2->Nrows=%d\n",p_block2->Nrows);
              printf("p_block2->Niout=%d\n",p_block2->Niout);
              printf("p_block2->Njout=%d\n",p_block2->Njout);
              printf("grtyp=%s,grtyp_=%s\n",p_block2->grtyp,p_block2->grtyp_);
              printf("ip1=");
              for(j=0;j<p_block2->Nrows;j++) printf(" %d",p_block2->ptr_ip1[j]);
              printf("\n");
              printf("ip2=");
              for(j=0;j<p_block2->Nrows;j++) printf(" %d",p_block2->ptr_ip2[j]);
              printf("\n");
              printf("ip3=");
              for(j=0;j<p_block2->Nrows;j++) printf(" %d",p_block2->ptr_ip3[j]);
              printf("\n");
              printf("nBits=");
              for(j=0;j<p_block2->Nrows;j++) printf(" %d",p_block2->ptr_nBits[j]);
              printf("\n");
              printf("datyp=");
              for(j=0;j<p_block2->Nrows;j++) printf(" %d",p_block2->p_datyp[j]);
              printf("\n");
              printf("p_block2->p_nomvar=%s\n",p_block2->p_nomvar);
              printf("p_block2->p_typvar=%s\n",p_block2->p_typvar);
              printf("iflt=");
              for(j=0;j<p_block2->Nrows;j++) printf(" %d",p_block2->iflt[j]);
              printf("\n");
#endif

	           if(p_block2->Nextra != 0) {
#ifdef DEBUG
              printf("Reading %d extra columns of data\n",p_block2->Nextra);
#endif
		      taille = 4 * p_block2->Nrows * p_block2->Nextra;  
		      p_block2->p_xtra = (float *) allocate_block(taille,"p_xtra");
	                /* transferer les points           */
                      nbytes=read_buf(fd,p_block2->p_xtra,taille); 

                      convert_to_IEEE(p_block2->p_xtra,p_block2->Nrows * p_block2->Nextra);
		     
		   }

                   p_block2->ZOUT = NULL;

                   if(p_block2->slab_id >= 0 && p_block2->slab_id < MAX_SLAB_TYPES) {
#ifdef DEBUG
  fprintf(stderr,"slab_id=%d description copied into table\n",p_block2->slab_id);
#endif
                     memcpy(&block2_dsc[p_block2->slab_id],p_block2,sizeof(block2_tmp));
                   } else {
                     fprintf(stderr,"Invalid slab type in descriptor block: %d \n",p_block2->slab_id );
                     exit(1);
                   }
                   break;

    case 'SLB2' :  i++;

	           nbytes=read_buf(fd,&block3_dat, sizeof(data_block) - sizeof(id_blck));
                   taille = 4 * block3_dat.nX;
                   block3_dat.p_hxpos = (int *) allocate_block(taille,"p_hxpos");
                   nbytes=read_buf(fd, block3_dat.p_hxpos, taille);

                   taille1 = 4 * block3_dat.Nrows * block3_dat.nX;
                   block3_dat.p_datxtr = (float *) allocate_block(taille1,"p_datxtr");
                   nbytes=read_buf(fd,block3_dat.p_datxtr,taille1);

                   if(block3_dat.slab_id >=0 && block3_dat.slab_id < MAX_SLAB_TYPES) {
                     p_block2 = &block2_dsc[block3_dat.slab_id];
                   } else {
                     fprintf(stderr,"Invalid slab type in data block:%d \n",p_block2->slab_id );
                     exit(1);
                   }
                   for(j=0;j < block3_dat.nX;j++)
                      if (block3_dat.p_hxpos[j] > (p_block2->Niout*p_block2->Njout)||
                                block3_dat.p_hxpos[j] < 0) {
                         printf("ERROR in xnio: value GT Niout*Njout %d*%d=%d\n",
                               p_block2->Niout,p_block2->Njout,
                               p_block2->Niout*p_block2->Njout);
                         printf("xnio[%d]= %d\n",j,block3_dat.p_hxpos[j]);
                         exit(1);
                      }

                   /* allocation dynamique pour       */
                   /* le block de donnees             */
                   if(p_block2->ZOUT == NULL){
                     taille = 4 * p_block2->Nrows * p_block2->count ;
                     p_block2->ZOUT = (float *) allocate_block(taille,"ZOUT");
                   }
                   if(p_block2 != NULL) { /* appel de la routine fortran pour assembler les donnees */
		      f77name(assemble)(p_block2->ZOUT, &p_block2->NIout,
			               &p_block2->NJout, &block3_dat.Nrows,
					block3_dat.p_datxtr, &block3_dat.nX,
					block3_dat.p_hxpos);
                   }
	           else{
		     fprintf(stderr,"error : invalid or undefined slab_id %d\n", block3_dat.slab_id);
		     exit(1);
		   }
                   p_block2->count -=  block3_dat.nX;

                   if(p_block2->count == 0){
                     convert_to_IEEE(p_block2->ZOUT,
                                     p_block2->Nrows * p_block2->NIout * p_block2->NJout);
#ifdef DEBUG
                     printf("SLAB type %d : data block complete\n",block3_dat.slab_id);
#endif
                     l++;
                     f77name(wrtstdf)(p_block2->ZOUT, &iun, &date, &block1.deet,
                      &block1.npas, &p_block2->NIout, &p_block2->NJout,
                      &p_block2->nxgrid, &p_block2->nygrid, &p_block2->Nrows,
                       p_block2->ptr_ip1,p_block2->ptr_ip2, p_block2->ptr_ip3,
                       p_block2->p_typvar,p_block2->p_nomvar, &block1.Ietiket,
                       p_block2->grtyp, &p_block2->ig1, &p_block2->ig2,
                       &p_block2->ig3, &p_block2->ig4, p_block2->p_datyp,
                      &p_block2->Nextra, p_block2->p_xtra, p_block2->ptr_nBits,
                       p_block2->iflt, list, &k ,&l);
 
                     if(p_block2->grtyp[0] == 'Z' || p_block2->grtyp[0] == 'Y' || p_block2->grtyp[0] == '#')
                        f77name(wstdfxy)(p_block2->p_xpos, p_block2->p_ypos, &iun,
                         &date, &block1.deet, &block1.npas,
                         &p_block2->nxgrid,&p_block2->nygrid,&p_block2->ig1,
                         &p_block2->ig2, &p_block2->ig3, &block1.Ietiket,
                          p_block2->grtyp_, &p_block2->ig1_, &p_block2->ig2_,
                         &p_block2->ig3_, &p_block2->ig4_,
                         &p_block2->Niout, &p_block2->Njout,p_block2->grtyp);
                     free(p_block2->ptr_ip1);
                     free(p_block2->ptr_ip2);
                     free(p_block2->ptr_ip3);
                     free(p_block2->p_typvar);
                     free(p_block2->p_nomvar);
                     free(p_block2->p_datyp);
                     free(p_block2->p_xtra);
                     free(p_block2->ptr_nBits);
                     free(p_block2->iflt);
                     free(p_block2->p_xpos);
                     free(p_block2->p_ypos);
                     free(p_block2->ZOUT);
#ifdef DEBUG
                     printf("SLAB type %d : data block complete, memory freed\n",block3_dat.slab_id);
#endif
                   }
	           free(block3_dat.p_hxpos);
	           free(block3_dat.p_datxtr);

                   break;
               
     default  :    fprintf(stderr,"\nerror : %s --> isn't a slab file\n", slab_file_name);
                   exit(1);

     } /*end switch*/

     nbytes=read_buf(fd,&id_blck,sizeof(id_blck));

 }/*end while*/
#ifdef DEBUG
     if (id_blck.identf == 'SLB9') {
     printf("BLOCK Header =%c%c%c%c nbytes=%d\n",id_blck.identf>>24,id_blck.identf>>16 & 0xFF,
             id_blck.identf>>8 & 0xFF,id_blck.identf & 0xFF, nbytes);
        }
#endif

 close(fd);
 
 ier = c_fstfrm(iun);
 
 return(0);
}          /*end main*/ 


void transfert_IBM_IEEE (unsigned INT_32 *tab_data_IBM, int nb_data)
{
   int signbit;
   int expos;
   INT_32 Mantis;
   int i;
   
   for(i=0; i<nb_data; i++) {
      signbit = tab_data_IBM[i] >> 31; 
      
      expos = (tab_data_IBM[i] >> 24) & 0x7f;
      
      Mantis = (tab_data_IBM[i] & 0xffffff);
      
      expos = ((expos - 64) << 2) + 128 - 2;
      
      if(Mantis != 0) {
	 while((Mantis & 0x800000) == 0) { 
	    Mantis <<= 1;
	    expos--;
	 }
	 Mantis &= 0x7fffff;
	 
	 tab_data_IBM[i] = Mantis | (expos << 23) | (signbit << 31);
      }
      
      if(expos <= 0) {                             /* || (Mantis == 0))*/
	 tab_data_IBM[i] = 0.0;
         expos = 0;
      }
      else{
          if(expos >= 255)
             {
             /* ERROR: replace value with NAN */
             tab_data_IBM[i] = 0xFFFFFFFF;
             }
	  }

   }/* end for */
   
}    /* end transfert_IBM_IEEE */
