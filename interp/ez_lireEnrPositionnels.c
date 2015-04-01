#include "ezscint.h"
#include "ez_funcdef.h"

void RemplirDeBlancs(char str[],wordint longueur);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint LireEnrPositionnels(_Grille *gr, wordint iunit, wordint ip1, wordint ip2, wordint ip3, wordint ip4)
{
  wordint moins1 = -1;
  wordint cle;
  wordint niy, njy, nky, nix, njx, nkx;
  wordint ier, ier1, ier2;
  wordint clex, bidon,ig1ref,ig2ref,ig3ref,ig4ref;
  char grref[2];
  ftnfloat *ax,*ay;

  char nomvarx[8], typvarx[4], etikx[16];
  char nomvary[8], typvary[4], etiky[16];
  wordint i,j;
  int listeCles_ax[32], listeCles_ay[32];
  int nbMaxCles = 32;
  int nbCles, trouve_x, trouve_y;

  wordint dateo, deet, npas, nbits;
  wordint intip1, intip2, intip3,intip4, tmpip3, intiunit,offsetx,offsety;

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

  strcpy(nomvary, "^^  ");
  strcpy(nomvarx, ">>  ");
  strcpy(etikx, "            ");
  strcpy(etiky, "            ");
  strcpy(typvarx, "  ");
  strcpy(typvary, "  ");

  intiunit = iunit;

  ier = c_fstinl(intiunit, &nix, &njx, &nkx,moins1, etikx, ip1, ip2, tmpip3, typvarx, nomvarx,listeCles_ax, &nbCles, nbMaxCles);
  trouve_x = 0;
  i = 0;
  while (trouve_x == 0 && i < nbCles)
    {
    ier = c_fstprm(listeCles_ax[i], &dateo, &deet, &npas, &nix, &njx, &nkx, &nbits,
        &bidon, &intip1, &intip2, &intip3, typvarx, nomvarx, etikx,
        grref, &ig1ref, &ig2ref, &ig3ref, &ig4ref, &bidon, &bidon, &bidon,
        &bidon, &bidon, &bidon, &bidon);
    if (nix == gr->ni || gr->grtyp[0] == '#')
      {
      trouve_x = 1;
      ier2 = listeCles_ax[i];
      }
    else
      {
      i++;
      }
    }

  ier = c_fstinl(intiunit, &niy, &njy, &nky,moins1, etiky, ip1, ip2, tmpip3, typvary, nomvary,listeCles_ay, &nbCles, nbMaxCles);
  trouve_y = 0;
  i = 0;
  while (trouve_y == 0 && i < nbCles)
    {
    ier = c_fstprm(listeCles_ay[i], &dateo, &deet, &npas, &niy, &njy, &nky, &nbits,
        &bidon, &intip1, &intip2, &intip3, typvary, nomvary, etiky,
        grref, &ig1ref, &ig2ref, &ig3ref, &ig4ref, &bidon, &bidon, &bidon,
        &bidon, &bidon, &bidon, &bidon);
    if (njy == gr->nj || gr->grtyp[0] == '#')
      {
      trouve_y = 1;
      ier1 = listeCles_ay[i];
      }
    else
      {
      i++;
      }
    }

  if (trouve_x == 0 || trouve_y == 0)
    {
    fprintf(stderr,"<LireEnrPositionnels>: Positional records ^^ and >> not found. Exiting...\n\n");
    return -1;
    }
  else
    {
    if (niy == nix && njy == njx)
      {
      gr->grtyp[0] = 'Y';
      }

    if (grref[0] != 'N' && grref[0] != 'S' &&  grref[0] != 'L' && grref[0] != 'E' && grref[0] != 'O')
      {
      fprintf(stderr,"<LireEnrPositionnels>: Unknown reference grid. Exiting...\n");
      return -1;
      }
    else
      {
      ay = (ftnfloat *) malloc(niy*njy*sizeof(ftnfloat));
      ier = f77name(fstluk)(ay, &ier1, &niy, &njy, &nky);

      ax = (ftnfloat *) malloc(nix*njx*sizeof(ftnfloat));
      clex = f77name(fstluk)(ax, &ier2, &nix, &njx, &nkx);

      switch (gr->grtyp[0])
        {
        case 'Y':
        case 'Z':
          gr->ax = (ftnfloat *) malloc(nix*njx*sizeof(ftnfloat));
          gr->ay = (ftnfloat *) malloc(niy*njy*sizeof(ftnfloat));
          memcpy(gr->ax,ax,nix*njx*sizeof(ftnfloat));
          memcpy(gr->ay,ay,niy*njy*sizeof(ftnfloat));
          break;

        case '#':
          gr->ax = (ftnfloat *) malloc(gr->ni*sizeof(ftnfloat));
          gr->ay = (ftnfloat *) malloc(gr->nj*sizeof(ftnfloat));
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

      if (grref[0] == 'L')
        {
        for (i=0; i < gr->ni; i++)
          {
          if (gr->ax[i] < 0.0)
            {
            gr->ax[i] += 360.0;
            }
          }
        }

      ier = f77name(fstprm)(&ier2, &dateo, &deet, &npas, &nix, &njx, &nkx, &nbits,
          &bidon, &intip1, &intip2, &intip3, typvarx, nomvarx, etikx,
          grref, &ig1ref, &ig2ref, &ig3ref, &ig4ref, &bidon, &bidon, &bidon,
          &bidon, &bidon, &bidon, &bidon,2,4,12,2);

      switch (gr->grtyp[0])
        {
        case 'Y':
        case 'Z':
          gr->fst.ip1     = ig1ref;
          gr->fst.ip2     = ig2ref;
          gr->fst.ip3     = ig3ref;
         break;

        case '#':
          gr->fst.ip1     = ig1ref;
          gr->fst.ip2     = ig2ref;
          gr->fst.ip3     = -1;
          break;
        }

      gr->fst.ig[IG1]    =  intip1;
      gr->fst.ig[IG2]    =  intip2;
      gr->fst.ig[IG3]    =  intip3;
      gr->fst.ig[IG4]    =  intip4;

      gr->fst.xg[IG1]    =  0.0;
      gr->fst.xg[IG2]    =  0.0;
      gr->fst.xg[IG3]    =  0.0;
      gr->fst.xg[IG4]    =  0.0;

      gr->grref[0]   = grref[0];
      if (gr->grref[0] == 'N') gr->fst.hemisphere = 1;
      if (gr->grref[0] == 'S') gr->fst.hemisphere = 2;

      gr->fst.igref[IG1]  = ig1ref;
      gr->fst.igref[IG2]  = ig2ref;
      gr->fst.igref[IG3]  = ig3ref;
      gr->fst.igref[IG4]  = ig4ref;

      if (gr->grref[0] != 'O')
         {
         f77name(cigaxg)(&(gr->grref),&gr->fst.xgref[XLAT1], &gr->fst.xgref[XLON1], &gr->fst.xgref[XLAT2], &gr->fst.xgref[XLON2],
             &gr->fst.igref[IG1], &gr->fst.igref[IG2], &gr->fst.igref[IG3], &gr->fst.igref[IG4]);
         }

      gr->fst.deet    = deet;
      gr->fst.npas    = npas;
      gr->fst.nbits   = nbits;
      gr->fst.date    = dateo;

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

      gr->flags  |= AX;
      }
    free(ax);
    free(ay);
    }
  return 0;
}

wordint LirePrmEnrPositionnels(_Grille *gr, wordint iunit, wordint ip1, wordint ip2, wordint ip3, wordint ip4)
{
  wordint moins1 = -1;
  wordint cle;
  wordint niy, njy, nky, nix, njx, nkx;
  wordint ier, ier_ax, ier_ay;
  wordint clex, bidon,ig1ref,ig2ref,ig3ref,ig4ref;
  char grref[2];
  ftnfloat *ax,*ay;

  char nomvarx[8], typvarx[4], etikx[16];
  char nomvary[8], typvary[4], etiky[16];
  wordint i,j;
  int listeCles_ax[32], listeCles_ay[32];
  int nbMaxCles = 32;
  int nbCles, trouve_x, trouve_y;

  wordint dateo, deet, npas, nbits;
  wordint intip1, intip2, intip3,intip4, tmpip3, intiunit,offsetx,offsety;

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

  strcpy(nomvary, "^^  ");
  strcpy(nomvarx, ">>  ");
  strcpy(etikx, "            ");
  strcpy(etiky, "            ");
  strcpy(typvarx, "  ");
  strcpy(typvary, "  ");

  intiunit = iunit;

  ier = c_fstinl(intiunit, &nix, &njx, &nkx,moins1, etikx, ip1, ip2, tmpip3, typvarx, nomvarx,listeCles_ax, &nbCles, nbMaxCles);
  trouve_x = 0;
  i = 0;
  if (gr->ni == 0 || gr->ni == -1)
   {
   gr->ni = nix;
   }
  while (trouve_x == 0 && i < nbCles)
    {
    ier = c_fstprm(listeCles_ax[i], &dateo, &deet, &npas, &nix, &njx, &nkx, &nbits,
        &bidon, &intip1, &intip2, &intip3, typvarx, nomvarx, etikx,
        grref, &ig1ref, &ig2ref, &ig3ref, &ig4ref, &bidon, &bidon, &bidon,
        &bidon, &bidon, &bidon, &bidon);
    if (nix == gr->ni || gr->grtyp[0] == '#')
      {
      trouve_x = 1;
      ier_ax = listeCles_ax[i];
      }
    else
      {
      i++;
      }
    }

  ier = c_fstinl(intiunit, &niy, &njy, &nky,moins1, etiky, ip1, ip2, tmpip3, typvary, nomvary,listeCles_ay, &nbCles, nbMaxCles);
  if (gr->nj == 0 || gr->nj == -1)
   {
   gr->nj = njy;
   }
  trouve_y = 0;
  i = 0;
  while (trouve_y == 0 && i < nbCles)
    {
    ier = c_fstprm(listeCles_ay[i], &dateo, &deet, &npas, &niy, &njy, &nky, &nbits,
        &bidon, &intip1, &intip2, &intip3, typvary, nomvary, etiky,
        grref, &ig1ref, &ig2ref, &ig3ref, &ig4ref, &bidon, &bidon, &bidon,
        &bidon, &bidon, &bidon, &bidon);
    if (njy == gr->nj || gr->grtyp[0] == '#')
      {
      trouve_y = 1;
      ier_ay = listeCles_ay[i];
      }
    else
      {
      i++;
      }
    }

  if (trouve_x == 0 || trouve_y == 0)
    {
    fprintf(stderr,"<LireEnrPositionnels>: Positional records ^^ and >> not found. Exiting...\n\n");
    return -1;
    }
  else
    {
   gr->ni          = nix;
   gr->nj          = njy;
    if (niy == nix && njy == njx)
      {
      gr->grtyp[0] = 'Y';
      }
    else
      {
      gr->grtyp[0] = 'Z';
      }

    if (grref[0] != 'N' && grref[0] != 'S' &&  grref[0] != 'L' && grref[0] != 'E' && grref[0] != 'O')
      {
      fprintf(stderr,"<LireEnrPositionnels>: Unknown reference grid. Exiting...\n");
      return -1;
      }

      ier = f77name(fstprm)(&ier_ax, &dateo, &deet, &npas, &nix, &njx, &nkx, &nbits,
          &bidon, &intip1, &intip2, &intip3, typvarx, nomvarx, etikx,
          grref, &ig1ref, &ig2ref, &ig3ref, &ig4ref, &bidon, &bidon, &bidon,
          &bidon, &bidon, &bidon, &bidon,2,4,12,2);

      switch (gr->grtyp[0])
        {
        case 'Y':
        case 'Z':
          gr->fst.ip1     = ig1ref;
          gr->fst.ip2     = ig2ref;
          gr->fst.ip3     = ig3ref;
         break;

        case '#':
          gr->fst.ip1     = ig1ref;
          gr->fst.ip2     = ig2ref;
          gr->fst.ip3     = -1;
          break;
        }

      gr->fst.ig[IG1]    =  intip1;
      gr->fst.ig[IG2]    =  intip2;
      gr->fst.ig[IG3]    =  intip3;
      gr->fst.ig[IG4]    =  intip4;

      gr->fst.xg[IG1]    =  0.0;
      gr->fst.xg[IG2]    =  0.0;
      gr->fst.xg[IG3]    =  0.0;
      gr->fst.xg[IG4]    =  0.0;

      gr->grref[0]   = grref[0];
      if (gr->grref[0] == 'N') gr->fst.hemisphere = 1;
      if (gr->grref[0] == 'S') gr->fst.hemisphere = 2;

      gr->fst.igref[IG1]  = ig1ref;
      gr->fst.igref[IG2]  = ig2ref;
      gr->fst.igref[IG3]  = ig3ref;
      gr->fst.igref[IG4]  = ig4ref;

      if (gr->grref[0] != 'O')
         {
         f77name(cigaxg)(&(gr->grref),&gr->fst.xgref[XLAT1], &gr->fst.xgref[XLON1], &gr->fst.xgref[XLAT2], &gr->fst.xgref[XLON2],
             &gr->fst.igref[IG1], &gr->fst.igref[IG2], &gr->fst.igref[IG3], &gr->fst.igref[IG4]);
         }

      gr->fst.deet    = deet;
      gr->fst.npas    = npas;
      gr->fst.nbits   = nbits;
      gr->fst.date    = dateo;

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
      }
  return 0;
}


void RemplirDeBlancs(char str[],wordint longueur)
{
  wordint i;

  for (i=strlen(str);i < longueur; i++)
    {
    str[i] = ' ';
    }
  str[longueur - 1] = '\0';
}
