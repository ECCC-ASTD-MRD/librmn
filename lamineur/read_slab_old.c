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
#include "slab_old.h"
#include "rpnmacros.h"

/*Author: Karim Tegguiche
 *
 *Revision V2.0 - M. Lepine - lecture a partir d'un fichier "piped"
 *Revision V3.0 - V. Lee  July 1997
 *Revision V4.0 - V. Lee  Jan.1998 - fixed label for 8 char output,
 *                uses "newdate" function
 *Revision Y2K  - V. Lee  Aug.1998 - compiled with $ARMNLIB/lib/librmnbeta.a
 *Revision V6.0 - V. Lee  June 1999 - data overflow error in field is replaced
 *                                    a NAN value 0xffffffff instead
 *
 */


#define ETIKET_LEN 8      /* number of chars for the etiket */

typedef struct 
          {
	   int identf, nBytes;
	   }Id_Block;

typedef struct block2
          {
	   int   slab_id,
	         ig1, ig2, ig3, ig4, Nrows,
                 Niout, Njout, nxgrid, nygrid, Nextra,
                 ig1_, ig2_, ig3_, ig4_;
	   char  grtyp[4] ,grtyp_[4];
	   int   *ptr_ip1, *ptr_ip2, *ptr_ip3, *iflt,
	         *ptr_nBits, *p_datyp;
	   char  *p_nomvar, *p_typvar;
 	   double *p_xpos, *p_ypos, *p_xtra, *ZOUT;

	   struct  block2 *suiv;
 
           }block2_dsc;

typedef struct block3
          {
	   int   slab_id, nX, Nrows, *p_hxpos;
           float *p_datxtr;
 
	   }block3_dat;

static Id_Block_file  block1;
static Slab_Descrt_file slab_dsc;
static Data_Block_file data_block;
static Slab_End slab_end;

static block2_dsc *p_block2,
                  *pfirst_block2,
                  *plast_block2;

static Id_Block id_blck;
static block3_dat *pdat_blk3;

static double *p_ZOUT; 
static int iun = 2;
int list[10];
int k = 0;
int j = 0;
int newdate_mode=3;



        /* Prototypes des fonctions */

static int lire(int fd, void *buffer, int nbytes_to_read);
void transfert_IBM_IEEE( unsigned long *, int); 



main(int argc, char *argv[])
 {
 int  fd, taille, taille1,i, 
       dateo, deet, i_len,
       ni, nj, nk = 1, npas,
       ier, nb_data, date;
 int nbytes, l;

 int n1, n2, n3, n4, n5, n6;
 
 int fd1;

  unsigned long *p_xpos, *p_ypos, *p_xtra;

  unsigned long  *ptr_IEEE_IBM;

 if((sizeof(int) != 4) || (sizeof(float) != 4))          
    {                                                 
     printf(" Inproper size for INT/FLOAT\n");                     
     exit(1);
     }
 

 if(argc < 3 || argc == 4)
    {
     fprintf(stderr," Usage: delamineur slab_file std_file [-f \"1 0 1\"]\n");
     exit(1);
     }

 if(argc == 5 && strcmp(argv[3],"-f") != 0)
    {
     fprintf(stderr," Usage: delamineur slab_file std_file [-f \"1 0 1\"]\n");
     fprintf(stderr," %s must be -f \n",argv[3]);
     exit(1);
     }

 if(strcmp(argv[1],"-") == 0)    /* lecture sur stdin */
   fd = 0;
 else {                          /* ouverture du fichier a lire */
   if((fd = open(argv[1],O_RDONLY)) == -1) 
     {                  
       fprintf(stderr,"error opening file %s\n",argv[1]);
       exit(1);
     }
 }

 k = sscanf(argv[4],"%d %d %d %d %d %d %d %d %d %d",
	    &list[0],&list[1],&list[2],&list[3],&list[4],
	    &list[5],&list[6],&list[7],&list[8],&list[9]);

 if (k > 1) {
   fprintf(stdout,"\nDelamineur_Y2K Info: Filter used =");
   for(i=0; i < k; i++)
     fprintf(stdout," %d",list[i]);
   fprintf(stdout,"\n");
 }
 else {
   fprintf(stdout,"\nDelamineur_Y2K Info: NO FILTERING\n\n");
   k=1;
   list[0]=1;
 }

 if((k % 2) == 0)
    {
     fprintf(stderr," error : la liste pour le filtrage doit etre impaire\n");
     fprintf(stderr," error : the list for filtering must be odd\n");
     exit(1);
     }

 lseek(fd,0L,SEEK_SET);
 nbytes = lire(fd,&block1,sizeof(block1)); 

 f77name(newdate)(&date,&block1.dateo1,&block1.dateo2,&newdate_mode);
/*
 printf("dateo1=%d dateo2=%d, date=%d\n",block1.dateo1,block1.dateo2,date);
 printf("deet=%d,npas=%d,etiket=%s\n",block1.deet,block1.npas,block1.etiket);
*/

 if ( (i_len=strlen(block1.etiket)) < ETIKET_LEN){
 /* fill with blanks */
    for (i=i_len; i< ETIKET_LEN; i++)
         block1.etiket[i] = ' ';
 block1.etiket[ETIKET_LEN] = '\0';
 }
/* printf("strlen(etiket)=%d\n",strlen(block1.etiket));*/

 ptr_IEEE_IBM = (unsigned long *) &block1.val15; 

 if(block1.slb0 != 'SLB0')
    {
     fprintf(stderr," %s ====> is not a slab file\n",argv[1]);
     exit(1);
     }

    nbytes = lire(fd,&id_blck,sizeof(id_blck));

/*  debug 
    printf("SLB0 detected\n"); */

 i=0;
 while(id_blck.identf != 'SLB9')
    {
     switch(id_blck.identf)
        {
        case 'SLB1' :  i++;
/* Debug      printf("SLB1 detected\n"); */
              if((p_block2 = (block2_dsc *) malloc(sizeof(block2_dsc)))==NULL)
                  {
                     fprintf(stderr,"Cannot allocate memory for block2_dsc\n");
                     exit(1);/* recuperer tous les donnees     */
                     }       /* pour ecrire le fichier std     */
                     nbytes= lire(fd,p_block2,sizeof(slab_dsc) - sizeof(id_blck));
                       p_block2->suiv = NULL;

                       if(pfirst_block2 == NULL)
                          pfirst_block2 = p_block2;

                       if (plast_block2 != NULL)
                          plast_block2->suiv = p_block2;
	   
                       plast_block2 = p_block2;
 
                       taille = 4 * p_block2->Nrows;
                       /* Les allocations dynamiques     */
                   if((p_block2->ptr_ip1 =  malloc(taille))  == NULL)
                      {
                       fprintf(stderr,"Can't allocate memory for ptr_ip1\n");
                       exit(1);
                       }

                   if((p_block2->ptr_ip2 =  malloc(taille))  == NULL)
                      {
                       fprintf(stderr,"Can't allocate memory for ptr_ip2\n");
                       exit(1);
                       }

                   if((p_block2->ptr_ip3 =  malloc(taille))  == NULL)
                      {
                       fprintf(stderr,"Can't allocate memory for ptr_ip3\n");
                       exit(1);
                       }

                   if((p_block2->ptr_nBits =  malloc(taille))  == NULL)
                      {
                       fprintf(stderr,"Can't allocate memory for ptr_nBits\n");
                       exit(1);
                       }

                   if((p_block2->p_datyp =  malloc(taille))  == NULL)
                      {
                       fprintf(stderr,"Can't allocate memory for p_datyp\n");
                       exit(1);
                       }/* recuperer tous les donnees      */
                        /* pour ecrire le fichier std      */
	           nbytes=lire(fd,p_block2->ptr_ip1, taille);
                   nbytes=lire(fd,p_block2->ptr_ip2, taille);
                   nbytes=lire(fd,p_block2->ptr_ip3, taille);
                   nbytes=lire(fd,p_block2->ptr_nBits, taille);
                   nbytes=lire(fd,p_block2->p_datyp , taille);
                   if((p_block2->p_nomvar = (char *) malloc(taille+1)) == NULL)
                      {
                       fprintf(stderr,"Can't allocate memory for p_nomvar");
	               exit(1);
	               }
                   if((p_block2->p_typvar = (char *) malloc(taille+1))==NULL)
                      {
	               fprintf(stderr,"Can't allocate memory for p_typvar");
	               exit(1);
	               }
                   nbytes=lire(fd,p_block2->p_nomvar,taille);
                   nbytes=lire(fd,p_block2->p_typvar,taille);

               /* si grtyp est 'Z' ou 'Y'         */
                   if(p_block2->grtyp[0] == 'Z' || p_block2->grtyp[0] == 'Y')
                     {                                                  
		      taille = 4 * p_block2->nxgrid;
	              if((p_block2->p_xpos = (double *) malloc(taille))==NULL)
		         {
		          fprintf(stderr,"Can't allocate memory for p_xpos");
			  exit(1);
	  		  }
                      nbytes=lire(fd,p_block2->p_xpos,taille);

                      if (*ptr_IEEE_IBM != 0x3fc00000)
                         { 
                         if (*ptr_IEEE_IBM == 0x41180000)
                            {
                             p_xpos = (unsigned long *) p_block2->p_xpos;
                             transfert_IBM_IEEE(p_xpos, p_block2->nxgrid);
                             }
                         else{
                              fprintf(stderr,"Floating points in data field ");
                              fprintf(stderr,"are not IBM/IEEE\n");
                              exit(1);
                              }
                         }    /* transferer les points           */
                              /*  flottants IBM en IEEE.         */
		      taille = 4 * p_block2->nygrid;
 	              if((p_block2->p_ypos = (double *) malloc(taille))==NULL)
			 {
			  fprintf(stderr,"Can't allocate memory for p_ypos");
			  exit(1);
			  }
                      nbytes=lire(fd, p_block2->p_ypos, taille);
                      if (*ptr_IEEE_IBM != 0x3fc00000)
                         { 
                          if (*ptr_IEEE_IBM == 0x41180000)
                             {
                              p_ypos = (unsigned long *) p_block2->p_ypos;
                              transfert_IBM_IEEE(p_ypos, p_block2->nygrid);
                              }
                          else{
                               fprintf(stderr,"Floating points in data field ");
                               fprintf(stderr,"are not IBM/IEEE\n");
                               exit(1);
                               }
                          } /* transferer les points           */
                            /* flottants IBM en IEEE.          */
                      }
        
                   taille = 4 *  p_block2->Nrows;
                   if ((p_block2->iflt = (int *) malloc(taille)) == NULL)
                      {
                       fprintf(stderr," Can't allocate memory for iflt\n");
                       exit(1);
                       }
        
                    nbytes=lire(fd,p_block2->iflt,taille);
              /* for debug only
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
              printf("\n"); */

	           if(p_block2->Nextra != 0)
		     {
		      taille = 4 * p_block2->Nrows * p_block2->Nextra;  
		      if((p_block2->p_xtra = (double *) malloc(taille)) ==NULL)
			 {
 			  fprintf(stderr,"Can't allocate memory for p_xtra");
		 	  exit(1);
			  } /* transferer les points           */
                      nbytes=lire(fd,p_block2->p_xtra,taille); 
                            /* flottants IBM en IEEE.          */
                      if (*ptr_IEEE_IBM != 0x3fc00000)
                         { 
                         if (*ptr_IEEE_IBM == 0x41180000)
                            {
                             p_xtra = (unsigned long *) p_block2->p_xtra;
                             transfert_IBM_IEEE(p_xtra, p_block2->Nrows * p_block2->Nextra);
                             }
                         else{
                              fprintf(stderr,"Floating points in data field ");
                              fprintf(stderr,"are not IBM/IEEE\n");
                              exit(1);
                              }
                         }
		     
		      }
                      /* allocation dynamique pour       */
                      /* le block de donnees             */
                   taille = 4 * p_block2->Nrows * p_block2->Niout * p_block2->Njout;
                   if((p_block2->ZOUT = (double *) malloc(taille)) == NULL)
                      {
                       fprintf(stderr,"Can't allocate memory for ZOUT\n");
		       exit(1);
                       }
                   break;

    case 'SLB2' :  i++;
            if((pdat_blk3 = (block3_dat *) malloc(sizeof(block3_dat)))==NULL)
                      {
                       fprintf(stderr,"Cannot allocate memory for pdat_blk3\n");
                       exit(1);
                       }

	        nbytes=lire(fd,pdat_blk3, sizeof(data_block) - sizeof(id_blck));
                   taille = 4 * pdat_blk3->nX;
                   if((pdat_blk3->p_hxpos = (int *) malloc(taille)) == NULL)
                      {
                       fprintf(stdout,"Can't allocate memory for p_hxpos\n");
                       exit(1);
                       }
                   nbytes=lire(fd, pdat_blk3->p_hxpos, taille);


                   taille1 = 4 * pdat_blk3->Nrows * pdat_blk3->nX;
                   if((pdat_blk3->p_datxtr = (float *) malloc(taille1)) == NULL)
	              {
	               fprintf(stderr,"Can't allocate memory for p_datxtr\n");
		       exit(1);
		       }
                   nbytes=lire(fd,pdat_blk3->p_datxtr,taille1);

	        for(p_block2=pfirst_block2;p_block2 != NULL;p_block2=p_block2->suiv)
		      { 
		       if(p_block2->slab_id == pdat_blk3->slab_id)
  		          break;
		       }
                   for(j=0;j < pdat_blk3->nX;j++)
              if (pdat_blk3->p_hxpos[j] > (p_block2->Niout*p_block2->Njout)||
                  pdat_blk3->p_hxpos[j] < 0) {
                     printf("ERROR in xnio: value GT Niout*Njout %d*%d=%d\n",
                               p_block2->Niout,p_block2->Njout,
                               p_block2->Niout*p_block2->Njout);
                     printf("xnio[%d]= %d\n",j,pdat_blk3->p_hxpos[j]);
                     exit(1);
                     }

                   if(p_block2 != NULL) /* appel de la routine fortran    */
                      { /* pour assembler les donnees     */
		      f77name(assemble)(p_block2->ZOUT, &p_block2->Niout,
			               &p_block2->Njout, &pdat_blk3->Nrows,
					pdat_blk3->p_datxtr, &pdat_blk3->nX,
					pdat_blk3->p_hxpos);
                      }
	           else{
		     fprintf(stderr,"error : invalid or undefined slab_id %d\n",
			                             pdat_blk3->slab_id);
			exit(1);
			}

	           free(pdat_blk3->p_hxpos);
	           free(pdat_blk3->p_datxtr);

                   break;
               
     default  : fprintf(stderr,"\nerror : %s --> isn't a slab file\n", argv[1]);
                exit(1);

    }/*end switch*/

    nbytes=lire(fd,&id_blck,sizeof(id_blck));

   }/*end while*/

 iun = 1;
 ier = c_fnom(iun,argv[2],"STD+RND",0); /* ouvrire le fichier standard    */
 if(ier != 0)
    {
     fprintf(stderr,"Error opening file %s\n", argv[2]);
     exit(1);
     }
  ier = c_fstouv(iun,"STD+RND+CREATE");
  if(ier < 0)
    {
     fprintf(stderr,"can not open unit %d",iun);
     exit(1);
     }

  l=0;
  for( p_block2 = pfirst_block2; p_block2 != NULL; p_block2 = p_block2->suiv)
     {
      if (*ptr_IEEE_IBM != 0x3fc00000)
         { 
          if (*ptr_IEEE_IBM == 0x41180000)
             {
              p_ZOUT = (double *) p_block2->ZOUT;              
              transfert_IBM_IEEE((unsigned long *)p_ZOUT, 
                 p_block2->Nrows * p_block2->Niout * p_block2->Njout);
              
              }
          else{
               fprintf(stderr,"Floating points in data field ");
               fprintf(stderr,"are not IBM/IEEE\n");
               exit(1);
               }
          }
                /* appel des routines fortran pour*/ 
      l++;      /* ecrire le fichier standard     */
     
    f77name(wrtstdf)(p_block2->ZOUT, &iun, &date, &block1.deet,
		      &block1.npas, &p_block2->Niout, &p_block2->Njout, 
		      &p_block2->nxgrid, &p_block2->nygrid, &p_block2->Nrows, 
		       p_block2->ptr_ip1,p_block2->ptr_ip2, p_block2->ptr_ip3,
		       p_block2->p_typvar,p_block2->p_nomvar, &block1.etiket, 
		       p_block2->grtyp, &p_block2->ig1, &p_block2->ig2, 
		       &p_block2->ig3, &p_block2->ig4, p_block2->p_datyp, 
		      &p_block2->Nextra, p_block2->p_xtra, p_block2->ptr_nBits,
		       p_block2->iflt, list, &k ,&l);

      if(p_block2->grtyp[0] == 'Z' || p_block2->grtyp[0] == 'Y')
         f77name(wstdfxy)(p_block2->p_xpos, p_block2->p_ypos, &iun,
			 &date, &block1.deet, &block1.npas,
			 &p_block2->nxgrid,&p_block2->nygrid,&p_block2->ig1,
                         &p_block2->ig2, &p_block2->ig3, &block1.etiket,
                          p_block2->grtyp_, &p_block2->ig1_, &p_block2->ig2_, 
                         &p_block2->ig3_, &p_block2->ig4_, 
                         &p_block2->Niout, &p_block2->Njout,p_block2->grtyp); 
      }   /* end for */

 close(fd);
 
 ier = c_fstfrm(iun);
 
 }/*end main*/ 


void transfert_IBM_IEEE (unsigned long *tab_data_IBM, int nb_data)
{
   int signbit;
   int expos;
   int long Mantis;
   int i;
   
   for(i=0; i<nb_data; i++)
      {
      signbit = tab_data_IBM[i] >> 31; 
      
      expos = (tab_data_IBM[i] >> 24) & 0x7f;
      
      Mantis = (tab_data_IBM[i] & 0xffffff);
      
      expos = ((expos - 64) << 2) + 128 - 2;
      
      if(Mantis != 0)
	 {
	 while((Mantis & 0x800000) == 0)
	    { 
	    Mantis <<= 1;
	    expos--;
	    }
	 Mantis &= 0x7fffff;
	 
	 tab_data_IBM[i] = Mantis | (expos << 23) | (signbit << 31);
	 }
      
      if(expos <= 0)                               /* || (Mantis == 0))*/
	 {
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
   
   }/* end transfert_IBM_IEEE */

static int lire(int fd, void *buffer, int nbytes_to_read)
{
  int nb, nlu = 0, nbytes=nbytes_to_read;
  char *buf = (char *) buffer;
  
  while (nlu < nbytes_to_read) {
    nb = read(fd,buf,nbytes);
    /*    fprintf(stdout,"Debug lire bytes=%d, nb %d\n",nbytes,nb); */
    if (nb <= 0) return(nb);
    nlu += nb;
    buf += nb;
    nbytes -= nb;
  }
  /*  fprintf(stdout,"Debug lire demande nbytes=%d, obtenu %d\n",nbytes_to_read,nlu); */
  return(nlu);
}
