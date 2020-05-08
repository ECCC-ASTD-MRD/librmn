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

/* test pour interface c des fichiers rapport


*/

  extern int c_mrfcls(), c_mrfget(), c_mrfloc(), c_mrfopn(), c_mrfprm(),
             c_mrfput(), c_mrbadd(), c_mrbdel(), c_mrbhdr(), c_mrbini(),
             c_mrblen(), c_mrbloc(), c_rbmprm(), c_mrbrep(), c_mrbxtr(),
             c_mrbupd(), c_mrfvoi(), c_mrfmxl(), c_mrbcov(), c_mrbdcv(),
             c_mrfopr(), c_mrfgor(), c_mrfopc(), c_mrfgoc(), c_mrbrpt();
  extern int c_mrbcvt(), c_mrbcol(), c_mrbdcl(), c_mrbsct(), c_mrfnbr();
  extern int c_mrblocx(), c_mrbtyp(), c_mrbtbl(), c_mrbprml();

  extern float second_();

#include <stdio.h>
#include "macros.h"

main ()
{
   int bufa[8000],bufb[5000],bufc[5000],tblval[10000],bufd[8000],xtra[3],
       bufe[20000],lstelea[10],lsteleb[4],lstelec[6],lstelee[130];
   float rval[500], opvalr;
   int clistea[10], clisteb[4], clistec[6], clistee[130];
   int tblburp[10][4];
   int bit0,blkno,lbits,left,ier,ier1,ier2,ier3,i,j, relem;
   int sup[2],xaux[2],nsup,nxaux;
   int tblcomp[500],lstcomp[10],tempo,etblval[10000];
   char stnid[9], opvalc[9];
   int temps,flgs,idtyp,lati,longi,elev,drcv,date,oars,run,nblk,nele,nval,
       dx,dy,nt,bfam,bdesc,btyp,nbit,datyp,lonenr,lonmax;

   int *ptba, *ptbb, *ptbc, *ptbd, *ptbe, *ptla, *ptlb, *ptlc,  *ptle,
       *pttcmp, *ptlcmp, *pttval, *cptla, *cptlb, *cptlc;
   int *pttblburp;
   float *ptrval;
   char *ptstnid;
   int testit(), rempli(), intcar();
   float t1, t2, chrono;
   int tblusr[5][3], lsteusr[5];
   int bknat, bktyp, bkstp;

   int tblprm[30][10];

     ier = c_fnom(10,"brpfil1","RND",0);
     ier = c_fnom(20,"brpfil2","RND",0);

     ptba = bufa; ptbb = bufb; ptbc = bufc; ptbd = bufd;
     pttval = tblval; pttcmp = tblcomp; ptlcmp = lstcomp;
     ptstnid = stnid;
     ptrval = rval;
     pttblburp=tblburp;
     *ptba = 8000; *ptbb = 5000; *ptbc = 5000; *ptbd = 8000;




    ptla = lstelea; ptlb = lsteleb; ptlc = lstelec, ptle = lstelee;
    cptla = clistea; cptlb = clisteb; cptlc = clistec;
    *ptla     = 2;
    *(ptla+1) = 1001;
    *(ptla+2) = 2121;
    *(ptla+3) = 63201;
    *(ptla+4) = 2129;
    *(ptla+5) = 4031;
    *(ptla+6) = 6011;
    *(ptla+7) = 10002;
    *(ptla+8) = 10061;
    *(ptla+9) = 20009;

    *ptlb     =  2;
    *(ptlb+1) =  1001;
    *(ptlb+2) =  2121;
    *(ptlb+3) =  63201;

    *ptlc     =  2;
    *(ptlc+1) =  1001;
    *(ptlc+2) =  2121;
    *(ptlc+3) =  63201;
    *(ptlc+4) =  63230;
    *(ptlc+5) =  63243;

    *ptle       =  1001;
    *(ptle+129) =  2122;
    for(i=1; i < 129; i++)
         *(ptle+i) = 2121;

    lsteusr[0] = 63000; lsteusr[1] = 63200; lsteusr[2] = 63201;
    lsteusr[3] = 63230; lsteusr[4] = 63243;
    tblusr[0][0] = 63000; tblusr[1][0] = 63200; tblusr[2][0] = 63201;
    tblusr[3][0] = 63230; tblusr[4][0] = 63243;
    tblusr[0][1] = 2; tblusr[1][1] = 0; tblusr[2][1] = 0;
    tblusr[3][1] = -1; tblusr[4][1] = 1;
    tblusr[0][2] = 100; tblusr[1][2] = 0; tblusr[2][2] = 0;
    tblusr[3][2] = 10; tblusr[4][2] = -5;

    printf(" %3.1f  mrfopc tolerance mise a SYSTEM \n",0.0);
    ier = c_mrfopc("ERRTOLR","SYSTEM");
    testit(ier);
    printf(" %3.1f  mrfgoc valeur de ERRTOLR \n",0.0);
    ier = c_mrfgoc("ERRTOLR",opvalc);
    printf(" valeur de ERRTOLR %s\n",opvalc);
    testit(ier);

    printf(" %3.1f  mrfopn en mode create\n",1.0);
    ier = c_mrfopn(10,"CREATE");
    testit(ier);
    printf(" %3.1f  mrbini pour trois rapports \n",2.0);

    ier1 = c_mrbini(10,bufa,1123,0377,"station#1",2,1800,3600,
                    0,0,975,42,901024,65535,1,sup,0,xaux,0);
    ier2 = c_mrbini(10,bufb,222,037,"station#2",3,900,1200,
                     1500,1200,1500,30,901024,60,1,sup,0,xaux,0);
    ier3 = c_mrbini(10,bufc,333,07,"station#3",4,450,600,
                    0,0,2000,348,901024,90,1,sup,0,xaux,0);

    testit(ier1+ier2+ier3);

    printf(" %3.1f  mrbadd (20 fois pour rapport bufa \n",3.0);
    ier = 0; i = 0;

    while(i < 20)
    {
         rempli(tblval,10,5,7);
         ier += c_mrbadd(bufa,&blkno,10,5,7,5,12,++i,32,&bit0,2,lstelea,tblval);
         printf(" *** info blkno = %d  bit0 = %d \n",blkno,bit0);
     }
     testit(ier);

    printf(" %3.1f  mrbadd (10 fois pour rapport bufb \n",3.1);
    ier = 0; i = 0;

    while(i < 10)
    {
         rempli(tblval,4,5,7);
         ier += c_mrbadd(bufb,&blkno,4,5,7,5,12,++i,32,&bit0,2,lsteleb,tblval);
         printf(" *** info blkno = %d  bit0 = %d \n",blkno,bit0);
     }
     testit(ier);

    printf(" %3.1f  mrbadd (12 fois pour rapport bufc \n",3.2);
    ier = 0; i = 0;

    while(i < 12)
    {
         rempli(tblval,6,5,7);
         ier += c_mrbadd(bufc,&blkno,6,5,7,5,12,++i,32,&bit0,2,lstelec,tblval);
         printf(" *** info blkno = %d  bit0 = %d \n",blkno,bit0);
     }
     testit(ier);

     printf(" %3.1f  mrblen pour rapports bufa bufb bufc \n",4.0);
     ier = 0;
     ier += c_mrblen(bufa,&lbits,&left);
     printf(" *** info lbits = %d  left = %d\n",lbits,left);
     ier += c_mrblen(bufb,&lbits,&left);
     printf(" *** info lbits = %d  left = %d\n",lbits,left);
     ier += c_mrblen(bufc,&lbits,&left);
     printf(" *** info lbits = %d  left = %d\n",lbits,left);
     testit(ier);

     printf(" %3.1f  mrbdel bufb bloc2 + mrblen\n",5.0);

     ier = c_mrbdel(bufb,2);
     ier += c_mrblen(bufb,&lbits,&left);
     printf(" *** info lbits = %d  left = %d\n",lbits,left);
     testit(ier);

     printf(" %3.1f  mrbxtr bloc 5 de bufa\n",6.0);
     ier = 0; i = 351;

     while(i--)
         *pttcmp++ = 5 * i;

     ier = c_mrbxtr(bufa,5,lstcomp,tblval);

     ier = i =0 ;
     while(i < 10)
     {
          if(*(ptlcmp+i) != *(ptla+i))
               ier--;
          i++;
     }
     testit(ier);

     ier = i =0;  pttcmp = tblcomp;
     while(i < 350)
     {
          if(*(pttcmp+i) != *(pttval+i))
               ier--;
          i++;
     }
     testit(ier);


     printf(" %3.1  mrbhdr bufa \n",7.0);
     ier = c_mrbhdr(bufa,&temps,&flgs,stnid,&idtyp,&lati,&longi,&dx,&dy,
                    &elev,&drcv,&date,&oars,&run,&nblk,sup,0,xaux,0);

     printf(" temps  = %d\n",temps);
     printf(" flgs  = %d\n",flgs);
     printf(" stnid = ");
     i = 0;
     while(i < 9)
          printf("%c",*(ptstnid + i++));
     printf("\n");
     printf(" idtyp = %d\n",idtyp);
     printf(" lati  = %d\n",lati);
     printf(" longi = %d\n",longi);
     printf(" dx    = %d\n",dx);
     printf(" dy    = %d\n",dy);
     printf(" elev  = %d\n",elev);
     printf(" drcv  = %d\n",drcv);
     printf(" date  = %d\n",date);
     printf(" oars  = %d\n",oars);
     printf(" run   = %d\n",run);
     printf(" nblk  = %d\n",nblk);
     testit(ier);

     printf(" %3.1  mrbhdr bufb \n",7.1);
     ier = c_mrbhdr(bufb,&temps,&flgs,stnid,&idtyp,&lati,&longi,&dx,&dy,
                    &elev,&drcv,&date,&oars,&run,&nblk,sup,0,xaux,0);

     printf(" temps  = %d\n",temps);
     printf(" flgs  = %d\n",flgs);
     printf(" stnid = ");
     i = 0;
     while(i < 9)
          printf("%c",*(ptstnid + i++));
     printf("\n");
     printf(" idtyp = %d\n",idtyp);
     printf(" lati  = %d\n",lati);
     printf(" longi = %d\n",longi);
     printf(" dx    = %d\n",dx);
     printf(" dy    = %d\n",dy);
     printf(" elev  = %d\n",elev);
     printf(" drcv  = %d\n",drcv);
     printf(" date  = %d\n",date);
     printf(" oars  = %d\n",oars);
     printf(" run   = %d\n",run);
     printf(" nblk  = %d\n",nblk);
     testit(ier);

     printf(" %3.1  mrbhdr bufc \n",7.2);
     ier = c_mrbhdr(bufc,&temps,&flgs,stnid,&idtyp,&lati,&longi,&dx,&dy,
                    &elev,&drcv,&date,&oars,&run,&nblk,sup,0,xaux,0);

     printf(" temps  = %d\n",temps);
     printf(" flgs  = %d\n",flgs);
     printf(" stnid = ");
     i = 0;
     while(i < 9)
          printf("%c",*(ptstnid + i++));
     printf("\n");
     printf(" idtyp = %d\n",idtyp);
     printf(" lati  = %d\n",lati);
     printf(" longi = %d\n",longi);
     printf(" dx    = %d\n",dx);
     printf(" dy    = %d\n",dy);
     printf(" elev  = %d\n",elev);
     printf(" drcv  = %d\n",drcv);
     printf(" date  = %d\n",date);
     printf(" oars  = %d\n",oars);
     printf(" run   = %d\n",run);
     printf(" nblk  = %d\n",nblk);
     testit(ier);


     printf(" %3.1f  mrbloc bfam=5,bdesc=12,btyp=7 bufa-bufb-bufc\n",8.0);
     blkno = 0;
     ier = c_mrbloc(bufa,733,0,7,blkno);
     printf(" blkno = %d\n",ier);
     ier1 = c_mrbloc(bufb,5,12,7,blkno);
     printf(" blkno = %d\n",ier1);
     ier2 = c_mrbloc(bufc,733,-1,7,blkno);
     printf(" blkno = %d\n",ier2);
     testit(ier+ier1+ier2);

     printf(" %3.1f  mrbloc bfam=3,bdesc=-1,btyp=7 bufa-bufb-bufc\n",8.1);
/*   n'existe pas */
     blkno = 0;
     ier = c_mrbloc(bufa,3,-1,7,blkno);
     printf(" blkno = %d\n",ier);
     ier1 = c_mrbloc(bufb,5,-1,7,blkno);
     printf(" blkno = %d\n",ier1);
     ier2 = c_mrbloc(bufc,3,-1,7,blkno);
     printf(" blkno = %d\n",ier2);
     ier = -ier; ier1 = -ier1; ier2 = -ier2;
     testit(ier+ier1+ier2);

     printf(" %3.1f mrbloc de tous les blocs e bufa\n",8.2);
     ier = ier2 = 0;
     while(ier >=0)
     {
       ier = c_mrbloc(bufa,-1,-1,-1,ier);
       if(ier > 0)
       {
          printf(" numero du bloc = %d\n",ier);
          ier++;
          ier2++;
       }
     }
     testit(ier2);

     printf(" %3.1f mrbprm bloc no blkno bufa-bufb-bufc\n",9.0);

     ier = c_mrbprm(bufa,ier,&nele,&nval,&nt,&bfam,&bdesc,&btyp,&nbit,
                    &bit0,&datyp);
     printf(" nele = %d nval = %d nt = %d bfam = %d bdesc = %d btyp = %d \n",
              nele,nval,nt,bfam,bdesc,btyp);
     printf(" nbit = %d bit0 = %d datyp = %d\n",nbit,bit0,datyp);
     testit(ier);

     ier = c_mrbprm(bufb,ier1,&nele,&nval,&nt,&bfam,&bdesc,&btyp,&nbit,
                    &bit0,&datyp);
     printf(" nele = %d nval = %d nt = %d bfam = %d bdesc = %d btyp = %d \n",
              nele,nval,nt,bfam,bdesc,btyp);
     printf(" nbit = %d bit0 = %d datyp = %d\n",nbit,bit0,datyp);
     testit(ier);

     ier = c_mrbprm(bufc,ier2,&nele,&nval,&nt,&bfam,&bdesc,&btyp,&nbit,
                    &bit0,&datyp);
     printf(" nele = %d nval = %d nt = %d bfam = %d bdesc = %d btyp = %d \n",
              nele,nval,nt,bfam,bdesc,btyp);
     printf(" nbit = %d bit0 = %d datyp = %d\n",nbit,bit0,datyp);
     testit(ier);

     printf(" %4.1f mrbprml bufa tous les blocl\n",9.1);

     ier = c_mrbprml(bufa,0,tblprm,10,30);
     printf(" nombre de blocs retournes %d\n",ier);

     printf(" bkno    nele   nval  nt  bfam  bdesc  btyp  nbit  bit0  datyp\n");

     for (j = 0; j< ier ; j++)
     {
          for (i = 0; i <10 ; i++)
          {
              printf(" %d ",tblprm[j][i]);
          }
          printf("\n");
     
     }
     testit(ier);

     printf(" %4.1f mrbprml bufa 10 blocs a partir de bkno=3\n",9.1);

     ier = c_mrbprml(bufa,3,tblprm,10,10);
     printf(" nombre de blocs retournes %d\n",ier);

     printf(" bkno    nele   nval  nt  bfam  bdesc  btyp  nbit  bit0  datyp\n");

     for (j = 0; j< ier ; j++)
     {
          for (i = 0; i <10 ; i++)
          {
              printf(" %d ",tblprm[j][i]);
          }
          printf("\n");
     
     }
     testit(ier);
     
     
     printf(" %4.1f mrfput bufa-bufb-bufc\n",10.0);
     ier = c_mrfput(10,0,bufa);
     ier += c_mrfput(10,0,bufb);
     ier += c_mrfput(10,0,bufc);
     testit(ier);

     printf(" %4.1f  mrfvoi fichier 10\n",11.0);
     ier = c_mrfvoi(10);
     testit(ier);

     printf(" %4.1f remplacer bloc 8 par bloc 5 dans bufa\n",12.0);
     printf(" %4.1f mrbrep + mrbxtr pour comparaison\n",12.0);

     ier = c_mrbrep(bufa,8,tblcomp);
     ier = c_mrbxtr(bufa,8,lstcomp,tblval);
     ier = i =0 ;
     while(i < 10)
     {
          if(*(ptlcmp+i) != *(ptla+i))
               ier--;
          i++;
     }
     testit(ier);

     ier = i =0;
     while(i < 350)
     {
          if(*(pttcmp+i) != *(pttval+i))
               ier--;
          i++;
     }
     testit(ier);

     printf(" %4.1f  mrbupd & mrfput de bufa elev de 1 a 100\n",13.0);
     printf(" %4.1f  mrbupd station#4 a station#6\n",13.1);

     *ptstnid = 's';     *(ptstnid+1) = 't';  *(ptstnid+2) = '*';
     *(ptstnid+3) = 't'; *(ptstnid+4) = '*';  *(ptstnid+5) = '*';
     *(ptstnid+6) = 'n'; *(ptstnid+7) = '#';  *(ptstnid+8) = '4';

     ier = i = 0;
     while(i < 100)
     {
          ier += c_mrbupd(10,bufa,1123,255,stnid,3,4523,26300,-1,-1,++i,32,
                          901029,0,2,sup,0,xaux,0);
          ier += c_mrfput(10,0,bufa);
     }
     testit(ier);

     *(ptstnid+8) = '5';
     ier = i = 0;
     while(i < 100)
     {
          ier += c_mrbupd(10,bufa,1123,255,stnid,3,4523,26300,-1,-1,++i,32,
                          901029,0,2,sup,0,xaux,0);
          ier += c_mrfput(10,0,bufa);
     }
     testit(ier);

     *(ptstnid+8) = '6';
     ier = i = 0;
     while(i < 100)
     {
          ier += c_mrbupd(10,bufa,-1,255,stnid,3,4523,26300,-1,-1,++i,32,
                          901029,0,2,sup,0,xaux,0);
          ier += c_mrfput(10,0,bufa);
     }
     testit(ier);

     printf(" %4.1f  mrfcls  fichier 10\n",14.0);

     ier = c_mrfcls(10);
     testit(ier);

     printf(" %4.1f  mrfvoi fichier 10 ferme\n",15.0);
     ier = c_mrfvoi(10);
     testit(ier);

     printf(" %4.1f  mrfopn en mode read\n",16.0);
     ier = c_mrfopn(10,"READ");
     testit(ier);

     printf(" %4.1f  mrfloc station#6 date = 901029 idtyp = 3\n",17.0);

     ier1 = c_mrfloc(10,0,"station#6",3,4523,26300,901029,1119,sup,0);
     printf(" handle = %d\n",ier1);
     testit(ier1);

     printf(" %4.1f  mrfget du rapport a position handle\n",18.0);
     ier = c_mrfget(ier1,bufd);
     ier = 0; i = 21;
     while(i < 7180)
     {
          if(*(ptbd + i) != *(ptba + i))
               ier--;
          i++;
     }
     testit(ier);

    printf(" %4.1f  mrfprm de l'enregistrement lu\n",19.0);
    ier = c_mrfprm(ier1,stnid,&idtyp,&lati,&longi,&dx,&dy,&date,&temps,&flgs,
                   sup,0,&lonenr);

     testit(ier);

     printf(" stnid = ");
     i = 0;
     while(i < 9)
          printf("%c",*(ptstnid + i++));
     printf("\n");
     printf(" temps = %d, lati = %d long = %d dx = %d dy = %d flgs = %x date = %d idtyp = %d lonenr = %d\n",
              temps,lati,longi,dx,dy,flgs,date,idtyp,lonenr);

     printf(" %4.1  mrfloc prochain enregistrement identique\n",20.0);

     ier1 = c_mrfloc(10,ier1,"station#6",3,4523,26300,901029,1119,sup,0);
     printf(" handle = %d\n",ier1);
     testit(ier1);

     printf(" %4.1  mrfget du rapport a la position handle\n",21.0);
     ier = c_mrfget(ier1,bufd);
     ier = 0; i = 21;
     while(i < 7180)
     {
          if(*(ptbd + i) != *(ptba + i))
               ier--;
          i++;
     }
     testit(ier);


    printf(" %4.1f  mrfprm de l'enregistrement lu\n",22.0);
    ier = c_mrfprm(ier1,stnid,&idtyp,&lati,&longi,&dx,&dy,&date,&temps,&flgs,
                   sup,0,&lonenr);

     testit(ier);

     printf(" stnid = ");
     i = 0;
     while(i < 9)
          printf("%c",*(ptstnid + i++));
     printf("\n");
     printf(" temps = %d, lati = %d long = %d dx = %d dy = %dflgs = %x date = %d idtyp = %d,lonenr = %d\n",
              temps,lati,longi,dx,dy,flgs,date,idtyp,lonenr);

     printf(" %4.1f mrfmxl de fichier 10\n",22.1);
     lonmax = c_mrfmxl(10);
     printf(" lonmax = %d\n",lonmax);
     testit(lonmax);

     printf(" %4.1f  mrfopn fichier 20 en mode create\n",23.0);
     ier = c_mrfopn(20,"CREATE");
     testit(ier);

     printf(" 4.1f mrfopc msglvl mis a error\n",23.1);
     ier = c_mrfopc("MSGLVL","ERROR");
     testit(ier);

     printf(" %4.1f  mrfupd mrfput bufb date = 1 a 8000\n",24.0);
     ier = i = 0;
     while(i < 8000)
     {
          ier += c_mrbupd(20,bufb,20,122,"station#9",4,2300,18934,
                          -1,-1,3452,3,++i,0,2,sup,0,xaux,0);
          ier += c_mrfput(20,0,bufb);
     }
     testit(ier);


     printf(" %4.1f  mrfloc chronometre date = 100 a 8000 inc= 100\n",25.0);

     printf("  date      handle       chrono\n");

     for(i = 100; i <= 8000; i+=100)
     {
         t1 = second_();
         for(j = 1; j < 51; j++) 
              ier = c_mrfloc(20,0,"station#9",4,-1,-1,i,-1,sup,0);
         t2 = second_();
         chrono = (t2 - t1)/ 50.0;
     
         printf("  %d      %d            %10.7f\n",i,ier,chrono);
      }

      printf(" %4.1f  mrfdel dernier enregistrement trouve\n",25.1);
      ier = c_mrfdel(ier);
      testit(ier);

      printf(" %4.1f  mrfnbr fichier 10 ouvert\n",25.2);
      ier = c_mrfnbr(10);
      printf(" nombre d'enregistrements : \n",ier);
      testit(ier);

      printf(" %4.1f  mrfnbr fichier 20 ouvert\n",25.3);
      ier = c_mrfnbr(20);
      printf(" nombre d'enregistrements : \n",ier);
      testit(ier);

      printf(" %4.1f  mrfcls fichier 10\n",26.0);
      ier = c_mrfcls(10);
      testit(ier);
      printf(" %4.1f  mrfcls fichier 20\n",26.1);
      ier = c_mrfcls(20);
      testit(ier);

      printf(" %4.1f  mrfnbr fichier 10 ferme\n",26.2);
      ier = c_mrfnbr(10);
      printf(" nombre d'enregistrements : \n",ier);
      testit(ier);

      printf(" %4.1f  mrfnbr fichier 20 ferme\n",26.3);
      ier = c_mrfnbr(20);
      printf(" nombre d'enregistrements : \n",ier);
      testit(ier);
 
      printf(" %4.1f mrbcol lstelea \n",27.0);
      ier = c_mrbcol(lstelea,clistea,10);
      printf(" lstelea \n");
      printf("%d %d %d %d %d \n",lstelea[0],lstelea[1],lstelea[2],
                                 lstelea[3],lstelea[4]);
      printf("%d %d %d %d %d \n",lstelea[5],lstelea[6],lstelea[7],
                                 lstelea[8],lstelea[9]);
      printf(" clistea \n");
      printf("%d %d %d %d %d \n",clistea[0],clistea[1],clistea[2],
                                 clistea[3],clistea[4]);
      printf("%d %d %d %d %d \n",clistea[5],clistea[6],clistea[7],
                                 clistea[8],clistea[9]);

      testit(ier);
      printf(" %4.1f mrbdcl clistea \n",27.1);
      ier = c_mrbdcl(clistea,lstcomp,10);
      for(i=0,ier=0; i<10; i++)
      {
          if(*(ptlcmp+i) != *(ptla+i))
                 ier--;
      }
      testit(ier);

      printf(" %4.1f mrbcov lsteleb\n",27.2);
      for(i=0,ier=0; i<4;i++)
      {
            *(cptlb+i) = c_mrbcov(*(ptlb+i));
            if(*(cptlb+i) != *(cptla+i))
                 ier--;
      }
      testit(ier);

      printf(" %4.1f mrbdcv lsteleb\n",27.3);
      for(i=0,ier=0;i<4;i++)
      {
           *(ptlcmp+i) = c_mrbdcv(*(cptlb+i));
           if(*(ptlcmp+i) != *(ptlb+i))
                 ier--;
      }
      testit(ier);

      printf(" %4.1f mrfopr manquant mis a 1.0E25 \n",28.0);
      ier = c_mrfopr("MISSING",1.0E25);
      testit(ier);

      printf(" %4.1f mrfgor obtenir valeur de manquant \n",28.1);
      ier = c_mrfgor("MISSING",&opvalr);
      printf(" valeur de opvalr %E\n",opvalr);
      testit(ier);

      printf(" %4.1f mrbsct de lsteusr\n",29.0);
      ier = c_mrbcol(lsteusr,lsteusr,5);
      for(i=0;i<=4;i++)
      {
          tblusr[i][0] = lsteusr[i];
      }

      ier = c_mrbsct(tblusr,5);
      testit(ier);

      printf(" %4.1f mrbcvt lsteleb du bufr a reel\n",30.2);
      rempli(tblval,4,5,7);
      *(pttval+1) = *(pttval+2) = *(pttval +3) = -1;
      *(pttval+4) = *(pttval+5) = *(pttval +6) = -12;
      printf(" tblval\n");
      for(i=0;i<10;i++)
      {
         for(j=0;j<7;j++)
             printf(" %d ",*(pttval+(14*i)+j));
         printf("\n");
         for(j=7;j<14;j++)
             printf(" %d ",*(pttval+(14*i)+j));
         printf("\n");
      }

      ier = c_mrbcvt(clisteb,tblval,rval,4,5,7,0);
      printf(" rval\n");
      for(i=0;i<10;i++)
      {
         for(j=0;j<7;j++)
             printf(" %f ",*(ptrval+(14*i)+j));
         printf("\n");
         for(j=7;j<14;j++)
             printf(" %f ",*(ptrval+(14*i)+j));
         printf("\n");
      }

      printf(" %4.1f mrbcvt lsteleb de reel a bufr\n",30.3);
      for(i=0;i<140;i++)
            *(pttcmp+i) = 0;

      ier = c_mrbcvt(clisteb,tblcomp,rval,4,5,7,1);
      printf(" tblcomp\n");
      for(i=0;i<10;i++)
      {
         for(j=0;j<7;j++)
             printf(" %d ",*(pttcmp+(14*i)+j));
         printf("\n");
         for(j=7;j<14;j++)
             printf(" %d ",*(pttcmp+(14*i)+j));
         printf("\n");
      }
      testit(ier);
      
      printf(" %4.1f mrbini bufe\n",31.0);

      ier = c_mrfopn(10,"APPEND");
      ptbe = bufe;

      *ptbe = 20000;

      ier3 = c_mrbini(10,bufe,0333,07,"STATION#3",4,450,600,0,0,100,68,
                      910224,90,1,sup,0,xaux,0);
      testit(ier3);

      printf(" %4.1f mrbadd 3fois pour rapport bufe dim en 16 bits\n",31.1);
      
      ier = 0;
      rempli(tblval,130,5,7);
      ier += c_mrbadd(bufe,&blkno,130,5,7,5,12,1,32,&bit0,2,lstelee,tblval);
      printf(" *** info blkno = %d  bit0 = %d \n",blkno,bit0);
      rempli(tblval,4,268,7);
      ier += c_mrbadd(bufe,&blkno,4,268,7,5,12,2,32,&bit0,2,lstelee,tblval);
      printf(" *** info blkno = %d  bit0 = %d \n",blkno,bit0);
      rempli(tblval,4,5,270);
      printf(" bloc3 contient des vaeurs negatives");
      *(pttval+1) = 0; 
      *(pttval+2) = -1; 
      *(pttval+3) = -2; 
      *(pttval+4) = -3; 
      *(pttval+5) = -4; 
      *(pttval+6) = -5; 
      *(pttval+7) = -6; 
      ier += c_mrbadd(bufe,&blkno,4,5,270,5,12,3,24,&bit0,4,lstelee,tblval);
      printf(" *** info blkno = %d  bit0 = %d \n",blkno,bit0);
      testit(ier);

      printf(" %4.1f mrbxtr pour bloc3 de bufe\n",31.2);

      ier = c_mrbxtr(bufe,3,clistee,etblval);

      for (i = 0; i < 4; i++)
      {
         if(clistee[i] != lstelee[i])
                ier--;
      }
      testit(ier);


     printf(" %4.1f mrbprm pour bloc 1 bufe\n",31.3);
     ier = c_mrbprm(bufe,1,&nele,&nval,&nt,&bfam,&bdesc,&btyp,&nbit,
                    &bit0,&datyp);
     printf(" nele = %d nval = %d nt = %d bfam = %d bdesc = %d btyp = %d \n",
              nele,nval,nt,bfam,bdesc,btyp);
     printf(" nbit = %d bit0 = %d datyp = %d\n",nbit,bit0,datyp);
     testit(ier);

     printf(" %4.1f mrbprm pour bloc 2 bufe\n",31.4);
     ier = c_mrbprm(bufe,2,&nele,&nval,&nt,&bfam,&bdesc,&btyp,&nbit,
                    &bit0,&datyp);
     printf(" nele = %d nval = %d nt = %d bfam = %d bdesc = %d btyp = %d \n",
              nele,nval,nt,bfam,bdesc,btyp);
     printf(" nbit = %d bit0 = %d datyp = %d\n",nbit,bit0,datyp);
     testit(ier);

     printf(" %4.1f mrbprm pour bloc 3 bufe\n",31.5);
     ier = c_mrbprm(bufe,3,&nele,&nval,&nt,&bfam,&bdesc,&btyp,&nbit,
                    &bit0,&datyp);
     printf(" nele = %d nval = %d nt = %d bfam = %d bdesc = %d btyp = %d \n",
              nele,nval,nt,bfam,bdesc,btyp);
     printf(" nbit = %d bit0 = %d datyp = %d\n",nbit,bit0,datyp);
     testit(ier);


/*   verifier si les elements sont repetitifs ou non
*/

     printf(" %4.1f mrbrpt 20004 pas repetitif\n",32.0);
     relem = c_mrbcov(20004);
     printf(" relem = %d\n",relem);
     ier = c_mrbrpt(relem);
     printf(" repetitif = %d\n",ier);
     testit(ier);
 
     printf(" %4.1f mrbrpt 20003 repetitif\n",32.1);
     relem = c_mrbcov(20003);
     printf(" relem = %d\n",relem);
     ier = c_mrbrpt(relem);
     printf(" repetitif = %d\n",ier);
     testit(ier);
 
     printf(" %4.1f mrbrpt 14192 repetitif\n",32.2);
     relem = c_mrbcov(14192);
     printf(" relem = %d\n",relem);
     ier = c_mrbrpt(relem);
     printf(" repetitif = %d\n",ier);
     testit(ier);
 
     printf(" %4.1f mrbrpt 14193 repetitif\n",32.3);
     relem = c_mrbcov(14193);
     printf(" relem = %d\n",relem);
     ier = c_mrbrpt(relem);
     printf(" repetitif = %d\n",ier);
     testit(ier);
 
     printf(" %4.1f mrbrpt 14194 repetitif\n",32.4);
     relem = c_mrbcov(14194);
     printf(" relem = %d\n",relem);
     ier = c_mrbrpt(relem);
     printf(" repetitif = %d\n",ier);
     testit(ier);
 
     printf(" %4.1f mrbrpt 20192 pas repetitif\n",32.5);
     relem = c_mrbcov(20192);
     printf(" relem = %d\n",relem);
     ier = c_mrbrpt(relem);
     printf(" repetitif = %d\n",ier);
     testit(ier);
 
     printf(" %4.1f mrbrpt 20195 pas repetitif\n",32.6);
     relem = c_mrbcov(20195);
     printf(" relem = %d\n",relem);
     ier = c_mrbrpt(relem);
     printf(" repetitif = %d\n",ier);
     testit(ier);
 
     printf(" %4.1f mrbrpt 0 illegal\n",32.7);
     relem = c_mrbcov(0);
     printf(" relem = %d\n",relem);
     ier = c_mrbrpt(relem);
     printf(" repetitif = %d\n",ier);
     testit(-ier);
 
     printf(" %4.1f mrbrpt 65700 illegal\n",32.8);
     ier = c_mrbrpt(65700);
     printf(" repetitif = %d\n",ier);
     testit(-ier);

     ier = c_mrfcls(10);
     printf(" %4.1f mrfopn fichier 10 en mode ajout\n",33.0); 

     ier = c_mrfopn(10,"APPEND");
     testit(ier);

     printf(" %4.1f mrbini bufa avec zero blocs\n",33.1);
     for(i = 1; i < 8000; i++)
          bufa[i] = 0;

     ier1 = c_mrbini(10,bufa,1123,0377,"station12",2,1800,3600,
                    0,0,975,42,901024,65535,1,sup,0,xaux,0);

     ier = c_mrbhdr(bufa,&temps,&flgs,stnid,&idtyp,&lati,&longi,&dx,&dy,
                    &elev,&drcv,&date,&oars,&run,&nblk,sup,0,xaux,0);
     printf(" temps  = %d\n",temps);
     printf(" flgs  = %d\n",flgs);
     printf(" stnid = ");
     i = 0;
     while(i < 9)
          printf("%c",*(ptstnid + i++));
     printf("\n");
     printf(" idtyp = %d\n",idtyp);
     printf(" lati  = %d\n",lati);
     printf(" longi = %d\n",longi);
     printf(" dx    = %d\n",dx);
     printf(" dy    = %d\n",dy);
     printf(" elev  = %d\n",elev);
     printf(" drcv  = %d\n",drcv);
     printf(" date  = %d\n",date);
     printf(" oars  = %d\n",oars);
     printf(" run   = %d\n",run);
     printf(" nblk  = %d\n",nblk);
     testit(ier);
 

     ier = c_mrfput(10,0,bufa);

     ier1 = c_mrfloc(10,0,"st*tio*12",-1,-1,-1,-1,-1,sup,0);
     printf(" handle = %d\n",ier1);
     testit(ier1);


     printf(" %4.1f mrfget de l'enregistrement avec zero blocs\n",33.2);
     ier = c_mrfget(ier1,bufa);
   

     ier = c_mrbhdr(bufa,&temps,&flgs,stnid,&idtyp,&lati,&longi,&dx,&dy,
                    &elev,&drcv,&date,&oars,&run,&nblk,sup,0,xaux,0);
     printf(" temps  = %d\n",temps);
     printf(" flgs  = %d\n",flgs);
     printf(" stnid = ");
     i = 0;
     while(i < 9)
          printf("%c",*(ptstnid + i++));
     printf("\n");
     printf(" idtyp = %d\n",idtyp);
     printf(" lati  = %d\n",lati);
     printf(" longi = %d\n",longi);
     printf(" dx    = %d\n",dx);
     printf(" dy    = %d\n",dy);
     printf(" elev  = %d\n",elev);
     printf(" drcv  = %d\n",drcv);
     printf(" date  = %d\n",date);
     printf(" oars  = %d\n",oars);
     printf(" run   = %d\n",run);
     printf(" nblk  = %d\n",nblk);
     testit(ier);


     printf(" %4.1f mrbtyp en mode -1\n",34.0);
     bknat=3; bktyp=92; bkstp=13;
     printf(" bknat = %d , bktyp = %d , bkstp = %d\n",bknat,bktyp,bkstp);
     btyp = c_mrbtyp(&bknat,&bktyp,&bkstp,-1);
     printf(" BTYP = %d\n",btyp);
     testit(btyp);

     printf(" %4.1f mrbtyp en mode 1\n",34.1);
     bknat=0; bktyp=0; bkstp=0;
     ier = c_mrbtyp(&bknat,&bktyp,&bkstp,btyp);
     printf(" bknat = %d , bktyp = %d , bkstp = %d\n",bknat,bktyp,bkstp);
     if((bknat != 3) || (bktyp != 92) || (bkstp != 13))
     {
          testit(-1);
     }
     else
     {
          testit(1);
     }
     
     printf(" %4.1f mrbadd avec un btyp composite\n",35.0);
     ier1 =0; i = 1;
     bknat=3;  bkstp=13;
     while( i <= 20)
     {
       bktyp = i;
       rempli(tblval,10,5,7);
       ier1 += c_mrbadd(bufa,&blkno,10,5,7,5,12,c_mrbtyp(&bknat,&bktyp,&bkstp,-1),
                     30,&bit0,2,lstelea,tblval);
         printf(" *** info blkno = %d  bit0 = %d \n",blkno,bit0);
         i++;
     }
     testit(ier1);

     ier = c_mrfopc("MSGLVL","INFORMATIF");
     printf(" %4.1f mrbloc et mrblocx des memes blocs\n",35.1);
     ier2 =0; i = 1;
     bknat=3;  bkstp=13;
     while(i <= 20)
     {
        bktyp = i;
        btyp = c_mrbtyp(&bknat,&bktyp,&bkstp,-1);
        ier = c_mrbloc(bufa,5,12,btyp,0);
        ier1 = c_mrblocx(bufa,5,12,3,i,13,0);
        if(ier !=ier1)
              ier2--;
      
        i++;
     }
     testit(ier2);    

     printf(" %4.1f mrblocx, bknat = -1, bktyp = 10, bkstp = 13\n",35.2);
     bknat = -1;  bktyp = 10; bkstp = 13;
     ier1 = c_mrblocx(bufa,5,12,bknat,bktyp,bkstp,0);
     printf(" mrblocx numero du block %d\n",ier1);
     testit(ier1);
    
     printf(" %4.1f mrblocx, bknat = 3, bktyp = -1, bkstp = 13\n",35.3);
     bknat = 3; bktyp = -1; bkstp = 13;
     ier1 = c_mrblocx(bufa,5,12,bknat,bktyp,bkstp,0);
     printf(" mrblocx numero du block %d\n",ier1);
     testit(ier1);
    
     printf(" %4.1f mrblocx, bknat = 3, bktyp = 10, bkstp = -1\n",35.4);
     bknat = 3; bktyp = 10; bkstp = -1;
     ier1 = c_mrblocx(bufa,5,12,bknat,bktyp,bkstp,0);
     printf(" mrblocx numero du block %d\n",ier1);
     testit(ier1);
    
     printf(" %4.1f mrblocx, bknat = -1, bktyp = -1, bkstp = -1\n",35.5);
      bknat = -1;
     ier1 = c_mrblocx(bufa,5,12,bknat,bknat,bknat,0);
     printf(" mrblocx numero du block %d\n",ier1);
     testit(ier1);
    
     printf(" %4.1f mrblocx, tous a -1 sauf bktyp = 10\n",35.6);
     ier1 = c_mrblocx(bufa,-1,-1,-1,10,-1,0);
     printf(" mrblocx numero du block %d\n",ier1);
     testit(ier1);
    
     
     printf(" %4.1f mrbtyp tous a valeur max en mode -1\n", 36.0);
     bknat = 15; bktyp = 127; bkstp = 15;
     ier = c_mrbtyp(&bknat,&bktyp,&bkstp,-1);
     if(ier != 32767)
           testit(-1);
     else
           testit(ier);

    
     printf(" %4.1f mrbtyp tous a valeur max en mode un\n", 36.1);
     ier = c_mrbtyp(&bknat,&bktyp,&bkstp,ier);
     if((bknat != 15) || (bktyp != 127) || (bkstp != 15))
           testit(-1);
     else
           testit(ier);

     printf(" %4.1f mrfcls du fichier 10\n",37.0);
     ier = c_mrfcls(10);
     testit(ier);
 
      

     printf(" %4.1f mrbtbl avec lstelea \n",40.0);

     for(i=0; i <=9; i++)
          tblburp[i][0] = c_mrbcov(lstelea[i]);

     ier = c_mrbtbl(tblburp,4,10);
     printf(" contenu du vecteur tblburp \n");

     for (i=0; i<=39;i++)
            printf("%d ",*(pttblburp+i));
     
     printf("\n");

     printf(" element elem-code echelle reference transformable\n");
     for (i=0; i<=9; i++)
          printf(" %d    %d        %d        %d          %d\n",
                 lstelea[i],tblburp[i][0],tblburp[i][1],tblburp[i][2],
                 tblburp[i][3]);

     printf(" contenu du vecteur tblburp \n");

     for (i=0; i<=39;i++)
            printf("%d ",*(pttblburp+i));
     
     printf("\n");
     testit(ier);

      
     printf(" %4.1f mrbtbl avec lsteusr \n",40.1);

     for(i=0; i <=4; i++)
          tblburp[i][0] = lsteusr[i];

     printf(" contenu du vecteur tblburp \n");

     for (i=0; i<=39;i++)
            printf(" %d ",*(pttblburp+i));
     
     printf("\n");
     ier = c_mrbtbl(tblburp,4,5);
     printf(" element elem-code echelle reference transformable\n");
     for (i=0; i<=4; i++)
          printf(" %d    %d        %d        %d          %d\n",
                 lsteusr[i],tblburp[i][0],tblburp[i][1],tblburp[i][2],
                 tblburp[i][3]);

     testit(ier);
     printf(" contenu du vecteur tblburp \n");

     for (i=0; i<=39;i++)
            printf("%d ",*(pttblburp+i));
     
     printf("\n");

     /* fin des test */

   printf(" FIN DES TESTS\n");

   }
int rempli(tableau,ni,nj,nk)
int *tableau,ni,nj,nk;
{
    int ijk;

    static int seed = 1;

    ijk = ni*nj*nk + 1;

    while(ijk--)
         *tableau++ = ijk*seed;

    seed++;
    }

int testit(ier)
int ier;
{
     if(ier < 0)
     {
          printf(" <<< erreur >>> %d\n",ier);
          exit(-1);
     }
     else
          printf(" --- reussi --- \n");
}

int intcar(string)
char *string;
{
     register int shc, inv;
     shc = 4; inv = 0;
     while(shc-- && *string)
         inv = (inv << 8) | *string++;
     return(inv);
}
