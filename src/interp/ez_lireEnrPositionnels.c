#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <rmn/base.h>
#include <rmn/fstd98.h>
#include <rmn/ezscint.h>
#include "ez_funcdef.h"
#include "base/base.h"


void Lire_enrUvercode1(_Grille *gr, float *yy, int32_t nix) {
  int32_t ig1refyin, ig2refyin, ig3refyin, ig4refyin;
  int32_t ig1refyan, ig2refyan, ig3refyan, ig4refyan;
  int32_t yinsize, ndiv, ni, nj;
  int32_t sub_gdrow_id,sub_gdcol_id;
  char grtypZ[2], grrefE[2];
  float *ax, *ay;

  ndiv = (int)yy[2];  /* number of LAM grids is 2*/
  ni = (int)yy[5];    /* ni size of LAM grid */
  nj = (int)yy[6];    /* nj size of LAM grid */
  gr->ni = ni;      /* ni size of U grid */
  gr->nj = nj*ndiv; /* nj size of U grid */
  ax = (float *) malloc(ni*sizeof(float));
  ay = (float *) malloc(nj*sizeof(float));
  memcpy(ax, &yy[15], ni * sizeof(float));
  memcpy(ay, &yy[15+ni], nj * sizeof(float));
  yinsize = 15 + ni + nj;
  gr->nsubgrids = ndiv;
  gr->subgrid = (int32_t *) malloc(ndiv*sizeof(int32_t));
  strcpy(grtypZ,"Z"); strcpy(grrefE,"E");
  /*yin*/
  f77name(cxgaig)(grrefE,&ig1refyin,&ig2refyin,&ig3refyin,&ig4refyin,&yy[11], &yy[12], &yy[13], &yy[14],1);
  gr->subgrid[0] = c_ezgdef_fmem(ni,nj,grtypZ,grrefE,ig1refyin,ig2refyin,ig3refyin,ig4refyin,ax,ay);
  c_gdkey2rowcol(gr->subgrid[0],  &sub_gdrow_id,  &sub_gdcol_id);
  c_ezgdef_yymask(&(Grille[sub_gdrow_id][sub_gdcol_id]));

  /*yang*/
  f77name(cxgaig)(grrefE,&ig1refyan,&ig2refyan,&ig3refyan,&ig4refyan,&yy[yinsize+6], &yy[yinsize+7], &yy[yinsize+8], &yy[yinsize+9],1);
  gr->subgrid[1] = c_ezgdef_fmem(ni,nj,grtypZ,grrefE,ig1refyan,ig2refyan,ig3refyan,ig4refyan,ax,ay);
  c_gdkey2rowcol(gr->subgrid[1],  &sub_gdrow_id,  &sub_gdcol_id);
  c_ezgdef_yymask(&(Grille[sub_gdrow_id][sub_gdcol_id]));
  free(ax);
  free(ay);
}


void Lire_enrTicTac(_Grille *gr, float *ax, int32_t nixnjx, float *ay, int32_t niynjy, int32_t ip3, int32_t ip4) {
  int32_t i,j,offsetx,offsety;
  switch (gr->grtyp[0])
    {
    case 'Y':
    case 'Z':
      gr->ax = (float *) malloc(nixnjx*sizeof(float));
      gr->ay = (float *) malloc(niynjy*sizeof(float));
      memcpy(gr->ax,ax,nixnjx*sizeof(float));
      memcpy(gr->ay,ay,niynjy*sizeof(float));
      break;

    case '#':
      gr->ax = (float *) malloc(gr->ni*sizeof(float));
      gr->ay = (float *) malloc(gr->nj*sizeof(float));
      offsetx = ip3 - 1;
      offsety = ip4 - 1;
      for (j=0; j < gr->nj; j++)
        {
        gr->ay[j] = ay[j+offsety];
        }

      for (i=0; i < gr->ni; i++)
        {
        gr->ax[i] = ax[i+offsetx];
        }
      break;
    }
  if (gr->grref[0] == 'L')
      {
      for (i=0; i < gr->ni; i++)
          {
          if (gr->ax[i] < 0.0)
             {
             gr->ax[i] += 360.0;
             }
          }
      }
}
int32_t LireEnrPositionnels(_Grille *gr, int32_t iunit, int32_t ip1, int32_t ip2, int32_t ip3, int32_t ip4, int32_t read)
{
  int32_t moins1 = -1;
  int32_t niy, njy, nky, nix, njx, nkx;
  int32_t ier, ier1, ier2;
  int32_t bidon, ig1ref, ig2ref, ig3ref, ig4ref;
  char grref[2];
  float *ax,*ay;
  float *yy;

  char nomvarx[8], typvarx[4], etikx[16];
  char nomvary[8], typvary[4], etiky[16];
  int Cles_ax, Cles_ay;
  int trouve_x, trouve_y;

  int32_t dateo, deet, npas, nbits;
  int32_t intip1, intip2, intip3,intip4, tmpip3, intiunit;

  intip1 = ip1;
  intip2 = ip2;
  intip3 = ip3;
  intip4 = ip4;

  if (gr->grtyp[0] == '#')
    {
    tmpip3 = -1;
    }
  else
   {
   tmpip3 = ip3;
   }

  strcpy(etikx, "            ");
  strcpy(etiky, "            ");
  strcpy(typvarx, "  ");
  strcpy(typvary, "  ");
  intiunit = iunit;

  if (gr->grtyp[0] == 'U')
    {
  strcpy(nomvary, "^>  ");
  strcpy(nomvarx, "^>  ");
    }
  else
    {
    strcpy(nomvary, "^^  ");
    strcpy(nomvarx, ">>  ");
    }

  trouve_x = 0;
  if (gr->ni == 0 || gr->ni == -1)
   {
   gr->ni = nix;
   }
  Cles_ax = c_fstinf(intiunit, &nix, &njx, &nkx,moins1, etikx, ip1, ip2, tmpip3, typvarx, nomvarx);

  while (trouve_x == 0 && Cles_ax > 0)
    {
     if (Cles_ax > 0) {

        ier = c_fstprm(Cles_ax, &dateo, &deet, &npas, &nix, &njx, &nkx, &nbits,
        &bidon, &intip1, &intip2, &intip3, typvarx, nomvarx, etikx,
        grref, &ig1ref, &ig2ref, &ig3ref, &ig4ref, &bidon, &bidon, &bidon,
        &bidon, &bidon, &bidon, &bidon);

        if (gr->grtyp[0] == 'U')
           {
           if (nix > 0 && njx == 1 && ier >= 0)
              {
              gr->ni = nix;
              gr->nj = njx;
              trouve_x = 1;
              trouve_y = 1;
              ier2 = Cles_ax;
              }
           }
        else if (nix == gr->ni || gr->grtyp[0] == '#')
          {
          trouve_x = 1;
          ier2 = Cles_ax;
          }
        else
          {
          /* not found, look for next one */
          Cles_ax = c_fstsui(intiunit, &nix, &njx, &nkx);
          }
    }
  }

  if (gr->grtyp[0] == 'U')
  {
     if (trouve_x != 1)
        {
        fprintf(stderr,"<LireEnrPositionnels>: Positional records ^> not found. Exiting...\n\n");
        return -1;
        }
     else
        {
        gr->ni_ax        = nix;
        gr->nj_ay        = njx;
        }
     if (grref[0] != 'F')
        {
        fprintf(stderr,"<LireEnrPositionnels>: Unknown reference grid %c for grid type U. Exiting...\n",grref[0]);
        return -1;
        }
     gr->grref[0] = grref[0]; /* reference grid */
     if (read == 1)
        {
        /* read the ^> grid descriptor */
        yy = (float *)malloc(nix * sizeof(float));
        ier = f77name(fstluk)((uint32_t *)yy, &ier2, &nix, &njx, &nkx);
        /* vercode=ig1ref */
        switch (ig1ref)
          {
          case 1:
           Lire_enrUvercode1(gr,yy,nix);
           break;
          }
         free(yy);
        }
  }

  if (gr->grtyp[0] != 'U') /* not a U grid */
  { /* search for the second grid descriptor */
     if (gr->nj == 0 || gr->nj == -1)
        {
        gr->nj = njy;
        }
     trouve_y = 0;
     Cles_ay = c_fstinf(intiunit, &niy, &njy, &nky,moins1, etiky, ip1, ip2, tmpip3, typvary, nomvary);
     while (trouve_y == 0 && Cles_ay > 0)
       {
       ier = c_fstprm(Cles_ay, &dateo, &deet, &npas, &niy, &njy, &nky, &nbits,
           &bidon, &intip1, &intip2, &intip3, typvary, nomvary, etiky,
           grref, &ig1ref, &ig2ref, &ig3ref, &ig4ref, &bidon, &bidon, &bidon,
           &bidon, &bidon, &bidon, &bidon);
       if (njy == gr->nj || gr->grtyp[0] == '#')
         {
         trouve_y = 1;
         ier1 = Cles_ay;
         }
       else
         {
          Cles_ay = c_fstsui(intiunit, &nix, &njx, &nkx);
         }
       }
     if (trouve_x == 0 || trouve_y == 0)
        {
        fprintf(stderr,"<LireEnrPositionnels>: Positional records ^^ and >> not found. Exiting...\n\n");
        return -1;
        }
     gr->ni_ax        = nix;
     gr->nj_ay        = njy;
     if (niy == nix && njy == njx)
        {
        gr->grtyp[0] = 'Y';
        }
     else
        {
        if (gr->grtyp[0] != '#')
           {
           gr->grtyp[0] = 'Z';
           }
        }
     if (grref[0] != 'N' && grref[0] != 'S' &&  grref[0] != 'L' && grref[0] != 'E' && grref[0] != 'O')
        {
        fprintf(stderr,"<LireEnrPositionnels>: Unknown reference grid. Exiting...\n");
        return -1;
        }
     gr->grref[0] = grref[0]; /* reference grid */
     if (read == 1)
        {
         /* read the 2 grid descriptors */
         gr->grref[0] = grref[0];
         gr->ni_ax = nix;
         gr->nj_ay = njy;
         ay = (float *) malloc(niy*njy*sizeof(float));
         ier = f77name(fstluk)((uint32_t *)ay, &ier1, &niy, &njy, &nky);
         ax = (float *) malloc(nix*njx*sizeof(float));
         f77name(fstluk)((uint32_t*)ax, &ier2, &nix, &njx, &nkx);
         Lire_enrTicTac(gr,ax,nix*njx,ay,niy*njy,ip3,ip4);
         free(ax);
         free(ay);
         }
  }

  /* for ALL grid types */
  ier = f77name(fstprm)(&ier2, &dateo, &deet, &npas, &nix, &njx, &nkx, &nbits,
          &bidon, &intip1, &intip2, &intip3, typvarx, nomvarx, etikx,
          grref, &ig1ref, &ig2ref, &ig3ref, &ig4ref, &bidon, &bidon, &bidon,
          &bidon, &bidon, &bidon, &bidon,2,4,12,2);

  gr->fst.ig[IG1]    =  intip1;
  gr->fst.ig[IG2]    =  intip2;
  gr->fst.ig[IG3]    =  intip3;
  gr->fst.ig[IG4]    =  intip4;

  gr->fst.xg[IG1]    =  0.0;
  gr->fst.xg[IG2]    =  0.0;
  gr->fst.xg[IG3]    =  0.0;
  gr->fst.xg[IG4]    =  0.0;

  switch (gr->grtyp[0])
  {
     case 'Y':
     case 'Z':
     case 'U':
        gr->fst.ip1     = ig1ref;
        gr->fst.ip2     = ig2ref;
        gr->fst.ip3     = ig3ref;
        break;

     case '#':
        gr->fst.ip1     = ig1ref;
        gr->fst.ip2     = ig2ref;
        gr->fst.ip3     = -1;
        gr->fst.ig[IG3]    =  ip3;
        gr->fst.ig[IG4]    =  ip4;
        break;
  }

  if (gr->grref[0] == 'N') gr->fst.hemisphere = 1;
  if (gr->grref[0] == 'S') gr->fst.hemisphere = 2;

  gr->fst.igref[IG1]  = ig1ref;
  gr->fst.igref[IG2]  = ig2ref;
  gr->fst.igref[IG3]  = ig3ref;
  gr->fst.igref[IG4]  = ig4ref;

  if ( gr->grref[0] != 'O' && gr->grtyp[0] != 'U' )
     {
     f77name(cigaxg)(gr->grref,&gr->fst.xgref[XLAT1], &gr->fst.xgref[XLON1], &gr->fst.xgref[XLAT2], &gr->fst.xgref[XLON2],
         &gr->fst.igref[IG1], &gr->fst.igref[IG2], &gr->fst.igref[IG3], &gr->fst.igref[IG4],1);
     }

  gr->fst.deet    = 0;
  gr->fst.npas    = 0;
  gr->fst.nbits   = 0;
  gr->fst.date    = 0;

  strcpy(gr->fst.nomvarx, nomvarx);
  strcpy(gr->fst.typvarx, typvarx);
  strcpy(gr->fst.etiketx, etikx);
  strcpy(gr->fst.nomvary, nomvary);
  strcpy(gr->fst.typvary, typvary);
  strcpy(gr->fst.etikety, etiky);

  RemplirDeBlancs(gr->fst.nomvarx, 5);
  RemplirDeBlancs(gr->fst.typvarx, 3);
  RemplirDeBlancs(gr->fst.etiketx, 13);
  RemplirDeBlancs(gr->fst.nomvary, 5);
  RemplirDeBlancs(gr->fst.typvary, 3);
  RemplirDeBlancs(gr->fst.etikety, 13);

  if (read == 1)
     {
     gr->flags  |= AX;
     }
   return 0;
}


void RemplirDeBlancs(char str[], int32_t longueur) {
    for (int i = strlen(str); i < longueur; i++) {
        str[i] = ' ';
    }
    str[longueur - 1] = '\0';
}
