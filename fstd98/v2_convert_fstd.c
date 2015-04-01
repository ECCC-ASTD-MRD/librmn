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
#include<stdlib.h>
#include<fcntl.h>
#include<sys/types.h>
#include<unistd.h>

#define Bitmot 32

typedef unsigned long word;

typedef struct
  {
   word swa : 32, npas1 : 16, nk : 12, epce1 : 4;
   word ni : 16, nj : 16, nomvar : 16, typvar : 8, nBits : 8; 
   word ip1 :16, ip2 : 16, ip3 : 16, epce2 : 7, dltf : 1, npas2 : 8; 
   word etiq14 :32, etiq56 : 16, etiq78 : 16;
   word epce3 : 32, epce4 : 16, ig2 : 16;
   word ig3 : 16, ig4 : 16, grtyp : 8, datyp : 8, ig1 : 16;
   word date : 32, ubc : 16, deet : 16;  
   word lng : 32, eof : 32;                  

              /****Format de bits 480-959 SEQ/SQI****/
              /* Le eof defini precedemant appartient
                 a cet interval de nombre de bits   */

   word vide1 : 32, vide2 : 32;
   word vide3 : 32, vide4 : 32;
   word epce5 : 32, epce6 : 32;
   word epce7 : 32, epce8 : 32;
   word epce9 : 32, epce10: 32;
   word epce11: 32, epce12: 32;
   word vide5 : 32, vide6 : 32;

   }seq_dir_keys;


typedef struct
  {
   word etiqt1  : 32, etiqt2  : 32;
   word dirsiz  : 32, inuti1  : 32;
   word nutil   : 32, inuti2  : 32;
   word nbecr   : 32, inuti3  : 32;
   word nbrec   : 32, inuti4  : 32;
   word nbext   : 32, inuti5  : 32;
   word nrecup  : 32, inuti6  : 32;
   word nbeff   : 32, inuti7  : 32;
   word nbcorr  : 32, inuti8  : 32;
   word inuti9  : 32, inuti10 : 32;
   word inuti11 : 32, inuti12 : 32;
   word inut13  : 32, inuti14 : 32;
   word inuti15 : 32, inuti16 : 32;
   word inuti17 : 32, inuti18 : 32;
     
     }stdf_struct_RND;

typedef struct
  {
   word swa : 32, npas1 : 16, nk : 12, epce1 : 4;
   word ni : 16, nj : 16, nomvar : 16, typvar : 8, nBits : 8; 
   word ip1 : 16, ip2 : 16, ip3 : 16, epce2 : 7, dltf : 1, npas2 : 8; 
   word etiq14 : 32, etiq56 : 16, etiq78 : 16;
   word epce3 : 32, epce4 : 16, ig2 : 16;
   word ig3 : 16, ig4 : 16, grtyp : 8, datyp : 8, ig1 : 16;
   word date : 32, ubc : 16, deet : 16;  
   word lng : 32;                  
          
   }rnd_dir_keys;

typedef struct
  {
   word signa : 12, ninj : 20, nscal : 16, iexpo : 12, signe : 4;
   word mantis : 32, mantirest : 16, nBits : 8, espce : 8;

   }stdf_struct_data;

union base                                                           
   {
    unsigned long iXmin;
    float Xmin;
    }Xbase;                                      


static word iexpo, iexpmin, iscal, iXmin, iTemp, result,isign,nBits, 
             nBits1, nBits2, nBytes, ninj,left, iexp_local, 
             *idata, *rawdata, *unpacked, *funpacked,ofset, block,*iidata, offset;  
          
static long taille, alloue=0,allouer=0;
static int  ni, nj, nk,
       record, i, fdr, fdw; 
static stdf_struct_data *indata;

float *ffunpacked;

stdf_struct_data *data;
stdf_struct_RND randm;
rnd_dir_keys dir[4096];
seq_dir_keys key;     

		    /*******Prototype des fonctions********/
 
static void entet_enreg(void),
            traiter_fichier_seq(seq_dir_keys *),
            traiter_fichier_rndm(stdf_struct_RND *,rnd_dir_keys *),
            decompact_float(rnd_dir_keys *),
            decompact_integer(rnd_dir_keys *),
            sortie_header (rnd_dir_keys *);
      
       
main(int argc, char *argv[])
 { 
  if((sizeof(int) != 4) || (sizeof(float) != 4))
     {
      printf(" Inproper size for INT/FLOAT\n");
      exit(1);
      }

  Xbase.Xmin = 1.5;
  if(Xbase.iXmin != 0x3fc00000)
    {
     printf(" floating point not IEEE\n");
     exit(1);
     }

  if(argc!=3)
    {
     fprintf(stderr,"\n\n usage : commande source result!\n\n");
     exit(1);
     }
 
  if((fdr = open(argv[1], O_RDONLY)) == -1)                     /* Ouverture du fichier d'entree.  */
     {
      fprintf(stderr,"\nErreur opening file %s !\n\n",argv[1]);
      exit(1);
      }
  
  if((fdw = open(argv[2], O_RDWR | O_CREAT,0644)) == -1)        /* ouverture du fichier de sortie. */
     {
      fprintf(stderr, "\nErreur opening file %s\n", argv[2]);
      exit(1);
      }
  
  nBytes = read(fdr,&randm,sizeof(stdf_struct_RND));            /* lecture de l'entete, de taille  */ 
  lseek(fdr,0L,SEEK_SET);                                       /* 120 Bytes, du fichir randm.     */
  nBytes = read(fdr,&key,sizeof(seq_dir_keys));                 /* lecture de l'entete, de taille  */
                                                                /* 120 Bytes, du fichier sequentiel*/
  entet_enreg();
  
  if ((key.vide5)== 0xaaaaaaaa)                                 /* si le patron binaire est celui  */
       traiter_fichier_seq(&key);                               /* d'un fichier sequentiel, alors  */
  else                                                          /* traiter_fichier_seq. Sinon si   */
     if((randm.etiqt1) == 0x55555555)                           /* c'est celui d'un fichier random */
         traiter_fichier_rndm(&randm,dir);                      /* alors traiter_fichier_random.   */ 
     else{                                                      /* sinon, le fichier n'est pas en  */
          printf("\n This file is not standard file!");         /* format standard.                */ 
          exit(1);
	  }
  

  close(fdr);                                                   /*Fermeture du fichier d'entree.   */
  close(fdw);                                                   /*Fermeture du fichier de sortie.  */

 }/*end main*/
 

 /*================================================================================================*/
 /*                       Imprimer l'entete des enregistrements.                                   */
 /*================================================================================================*/

  void entet_enreg(void)
    {
     printf ("#KEY%1sID%4sIP1=%2sIP2=%4sIP3=%5sNI=%4sNJ=%3sNK=%6s","","","","","","","","");
     printf("ETIQ.%5sDATE ORIG.%4sDEET=%3sNPAS=%4sGR%3sIG1=%3sIG2=%3sIG3=%3sIG4=%2sDTY\n\n",
                                                             "","","","","","","","","");
     }/*end entet_enreg*/



 /*================================================================================================*/
 /* Definition de la fonction pour le traitement des fichiers sequentiels. Elle fait appel a       */
 /* decompact_float, decompact_integer.                                                            */
 /*================================================================================================*/

  void traiter_fichier_seq(seq_dir_keys *keys)
    { 
     lseek(fdr,0L,SEEK_SET);  
     for(nBytes = read(fdr,keys,sizeof(seq_dir_keys));          /* Lecture des enregistrements     */
         nBytes==120;                                           /* du fichier standard sequentiel. */
         nBytes = read(fdr,keys,sizeof(seq_dir_keys)))
	{ 
	 if((keys->nk) == 0)
	     keys->nk = 1;
	      
	 if((keys->vide5) != 0xaaaaaaaa)
	     exit(1);

	 if(keys->eof != 0)
	    {
	     if (keys->eof > 14)
		break;
             else
                continue;       
             }

	 taille = ((keys->lng + 59) / 60) * 120;                /* calcul de la taille de l'enre-  */
	                                                        /* gistrement .                    */
	 if (taille != 0) 
            { 
	     if (alloue<taille)
		{ 
                 if(alloue)free(indata);
		 alloue=taille;
                                                                /* Allocation de la memoire.       */
		 if ((indata = (stdf_struct_data *) malloc(taille)) == NULL)
		    {
                     printf("malloc : cannot allocate memory");
		     exit(1);
		     }
		 }
             rawdata = (word *) indata;                               
            
             nBytes = read(fdr,indata,taille);                  /* lecture des donnees en fonction */
             }                                                  /* de la taille allouee.           */ 
                                                                
	 if(keys->datyp == 1)                                   /* Si les donnees sont en point    */ 
	    decompact_float((rnd_dir_keys *)keys);              /* flottant appel a decompact_     */
	                                                        /* float.                          */             
          if((keys->datyp) == 2 || (keys->datyp) == 4)          /* Si les donnees sont des integer */
	     decompact_integer((rnd_dir_keys *)keys);           /* non signees ou signees, appel a */
	                                                        /* decompact_integer.              */
	 }/*end for loop*/
 
     }/*end traiter_fichier_seq*/


 /*================================================================================================*/
 /* Definition de la fonction pour decompater les fichiers sequentiels.Elle fait appel a           */
 /* recuperer_donnees ainsi qu'a la fonction processus_decompact_float                             */
 /*================================================================================================*/

  void decompact_float(rnd_dir_keys *fkeys)
    { 
     int ier;
     
     nBits = indata->nBits;                                     /* enregistrements.                */
     ni = fkeys->ni;
     nj = fkeys->nj;
     nk = fkeys->nk;
     ninj  = ni * nj * nk;

     if((indata->ninj) != (ninj & 0xfffff))
        return;  
    
        if(allouer < (sizeof(float) * ninj))
          {
           if(allouer) free(idata);
              allouer = (sizeof(float) * ninj);                 /* allocation dynamique du tableau */
                                                                /* pour les donnees decompactees.  */
           if((idata = (word *) malloc(allouer)) == NULL)         
	     {
              printf("\n\nmalloc : cannot allocate memory!\n\n");
              exit(1);
              }
           }
     funpacked = (word *) idata;

     ier = compact_float (funpacked, rawdata, ninj, nBits, 120, 1, 2);
     
     sortie_header (fkeys);                                     /* de decompaction.                */
       
    }/*end decompact_float*/
 
 
 /*================================================================================================*/
 /* Definition de lafonction pour decompacter les entiers organises sequentiellement.              */
 /*================================================================================================*/

  void decompact_integer(rnd_dir_keys *ikeys)
   {
    int ier, OP;

    nBits = ikeys->nBits;
    ni = ikeys->ni;
    nj = ikeys->nj;
    nk = ikeys->nk;
    ninj  = ni * nj * nk;
    left = Bitmot;   
    block = (ninj + 3) * 4;
    
    if(allouer < (sizeof(int) * ninj))
      {
       if(allouer) free(idata);
          allouer = (sizeof(int) * ninj);

       if((idata = (word *) malloc(allouer)) == NULL)           /* Allocation de la memoire.       */
         {
          printf("\nmalloc : cannot allocate memory!\n");
          exit(1);
          }
         
       }
    unpacked = (word *) idata;

    if(ikeys->datyp == 2)
       OP = 2;
    else
       OP = 4;

    ier = compact_int (unpacked, rawdata, ninj, nBits, 0 ,1 , OP);
    
    sortie_header (ikeys);                                      /* appel de sortie_header pour     */  
                                                                /* ecrire les resultats.           */
    }/*end decompact_integer*/

 


 /*================================================================================================*/
 /* Definition de la fonction pour le traitement des fichiers aleatoires. Elle fait appel a        */
 /* decompact_float et decompact_integer.                                                          */
 /*================================================================================================*/

  void traiter_fichier_rndm (stdf_struct_RND *randm,rnd_dir_keys *dir)
   {
    nBytes = read(fdr,dir,sizeof(rnd_dir_keys) * randm->nutil);
          	     
    for(record=0; record <( randm->nutil); record++ )
       { 
        if(dir[record].nk ==0)                                  /* pour eviter que ni * nj * nk   */
           dir[record].nk = 1;                                  /* soit null.                     */
	       
        if(dir[record].dltf == 1)                               /* si c'est un delete file        */
           continue;                                            /* on continu.                    */
              
        taille = ((dir[record].lng + 59) / 60) * 120;           /* calcul de la taille du record  */
        if(taille != 0)
	  {
	   if(alloue<taille)
	     {
	      if(alloue) free(indata);
	         alloue=taille;                                 /* allocation de la memoire selon */
                                                                /* la taille calculee.            */
	      if((indata = (stdf_struct_data *) malloc(taille)) == NULL)
	        {
	         printf("\n malloc : cannot allocate memory\n");
	         exit(1);
	         }
	      }
           rawdata = (word *) indata;
           ofset = dir[record].swa;                             /* determiner la position pour    */
           ofset <<= 1;                                         /* la lecture.                    */
           lseek(fdr, ofset, SEEK_SET);                         /* lecture  en mode d'acces direct*/
           nBytes=read(fdr, indata,taille);                     /* (random acces).                */        
           }  
        
        if (dir[record].datyp == 1)                             /* si c'est des donnees en float  */
            decompact_float(&(dir[record]));                    /* appel a decompact_float        */
        
	if (dir[record].datyp == 2 || (dir[record].datyp == 4)) /* et si c'est des integer        */
	    decompact_integer(&(dir[record]));                  /* appel a decompact_integer      */        
        
       }/*end for loop*/          
       
    }/*end traiter_fichier_rndm*/
	

 /*===============================================================================================*/
 /* Definition de la fonction qui affiche, sur l'ecran,de l'information sur les enregistrements du*/
 /* fichier sequentiel.                                                                           */
 /*===============================================================================================*/

  void sortie_header (rnd_dir_keys *hkeys)
    {
     word ea,eb,ec; 
     char c1, c2, c3, etiq14, etiq56, etiq78,
          grtyp,e1,e2,e3,e4,e5,e6,e7,e8;
     int  npas, deet,ip1,ip2,ip3,ig1,ig2,ig3,
          ig4, date,c4,j;
     static int k=0;
    
     c1 = (hkeys->nomvar) >> 8;
     c2 = (hkeys->nomvar) &0xff;
     c3 = (hkeys->typvar);
     c4 = (hkeys->datyp) ;
     grtyp = hkeys->grtyp;
     npas = (hkeys->npas1) | ((hkeys->npas2) >> 16);
     deet = (hkeys->deet) >> 3;
     ip1 = hkeys->ip1;
     ip2 = hkeys->ip2;
     ip3 = hkeys->ip3;
     ea = hkeys->etiq14;
     e1 = ea >> 24;
     e2 = (ea >> 16) & 0xff;
     e3 = (ea >> 8) & 0xff;
     e4 =  ea & 0xff;
     eb = hkeys->etiq56;
     e5 = eb >> 8;
     e6 = eb & 0xff;
     ec = hkeys -> etiq78;
     e7 = ec >> 8;
     e8 = ec & 0xff;
     date = (hkeys->date) >> 3;      
     ig1 = hkeys->ig1;
     ig2 = hkeys->ig2;
     ig3 = hkeys->ig3;
     ig4 = hkeys->ig4;
     ni = hkeys->ni;
     nj= hkeys->nj;
     nk= hkeys->nk;                                      /* Write record descriptors into stdout */
                                                         /* substitute appropriate local code as */
                                                         /* necessary.                           */

     printf (" %c%c%3s%c%s%6u%3s%3u%5s%3u%3s%6u%2s%5u%3s%3u%3s",c1,c2,"",c3,"",ip1,"",
                                                       ip2,"",ip3,"",ni,"",nj,"",nk,"");
     printf("%3c%c%c%c%c%c%c%c%5s%u%7s%u%5s%3u%6s%2c%3s%2u%5s%2u%5s%2u%4s %2u%4s %u\n",
             e1,e2,e3,e4, e5,e6,e7,e8,"",date,"",deet,"",npas,"",grtyp,"",ig1,"",ig2,"",
	                                                             ig3,"",ig4,"", c4);
  
     write(fdw, &block, sizeof(float));                 /* write data by idata into output file  */
     write(fdw, &ni, sizeof(float));                    /* if hkeys->datyp == 1 idata point to   */
     write(fdw, &nj, sizeof(float));                    /* float.                                */ 
     write(fdw, &nk, sizeof(float));
     write(fdw, idata, (ninj * 4));
     write(fdw, &block, sizeof(float));                                                     

     
   }/*end sortie_header_seq*/


