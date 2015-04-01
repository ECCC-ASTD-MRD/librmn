#include "ezscint.h"
#include "ez_funcdef.h"

wordint RemplirDeBlancs(char str[],wordint longueur);

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

  if (gr->grtyp == '#') 
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
    if (nix == gr->ni || gr->grtyp == '#') 
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
    if (njy == gr->nj || gr->grtyp == '#') 
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
    fprintf(stderr,"<LireEnrPositionnels>: Positional records ^^ and >> not found... Impossible to define grid...\n\n");
    return -1;
    }
  else
    {
    if (niy == nix && njy == njx)
      {
      gr->grtyp = 'Y';
      }

    if (grref[0] != 'N' && grref[0] != 'S' &&  grref[0] != 'L' && grref[0] != 'E')
      {
      fprintf(stderr,"<LireEnrPositionnels>: Unknown reference grid. Impossible to define grid...\n");
      return -1;
      }
    else
      {
      ay = (ftnfloat *) malloc(niy*njy*sizeof(ftnfloat));
      ier = f77name(fstluk)(ay, &ier1, &niy, &njy, &nky);

      ax = (ftnfloat *) malloc(nix*njx*sizeof(ftnfloat));
      clex = f77name(fstluk)(ax, &ier2, &nix, &njx, &nkx);

      switch (gr->grtyp)
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

      switch (gr->grtyp)
        {
        case 'Y':
        case 'Z':
          gr->ip1     = ig1ref;
          gr->ip2     = ig2ref;
          gr->ip3     = ig3ref;
         break;

        case '#':
          gr->ip1     = ig1ref;
          gr->ip2     = ig2ref;
          gr->ip3     = -1;
          break;
        }

      gr->ig[IG1]    =  intip1;
      gr->ig[IG2]    =  intip2;
      gr->ig[IG3]    =  intip3;
      gr->ig[IG4]    =  intip4;

      gr->xg[IG1]    =  0.0;
      gr->xg[IG2]    =  0.0;
      gr->xg[IG3]    =  0.0;
      gr->xg[IG4]    =  0.0;

      gr->grref   = grref[0];
          if (gr->grref == 'N') gr->hemisphere = 1;
          if (gr->grref == 'S') gr->hemisphere = 2;

      gr->igref[IG1]  = ig1ref;      
      gr->igref[IG2]  = ig2ref;      
      gr->igref[IG3]  = ig3ref;      
      gr->igref[IG4]  = ig4ref;

      f77name(cigaxg)(&(gr->grref),&gr->xgref[XLAT1], &gr->xgref[XLON1], &gr->xgref[XLAT2], &gr->xgref[XLON2],
          &gr->igref[IG1], &gr->igref[IG2], &gr->igref[IG3], &gr->igref[IG4]);

      gr->deet    = deet;
      gr->npas    = npas;
      gr->nbits   = nbits;
      gr->date    = dateo;

      strcpy(gr->nomvarx, nomvarx);
      strcpy(gr->typvarx, typvarx);
      strcpy(gr->etiketx, etikx);
      strcpy(gr->nomvary, nomvary);
      strcpy(gr->typvary, typvary);
      strcpy(gr->etikety, etiky);

      RemplirDeBlancs(gr->nomvarx, 5);
      RemplirDeBlancs(gr->typvarx, 3);
      RemplirDeBlancs(gr->etiketx, 13);
      RemplirDeBlancs(gr->nomvary, 5);
      RemplirDeBlancs(gr->typvary, 3);
      RemplirDeBlancs(gr->etikety, 13);

      gr->flags  |= AX;
      }
    free(ax);
    free(ay);
    }
  return 0;
}

wordint RemplirDeBlancs(char str[],wordint longueur)
{
  wordint i;

  for (i=strlen(str);i < longueur; i++)
    {
    str[i] = ' ';
    }
  str[longueur - 1] = '\0';
  return 0;
}
