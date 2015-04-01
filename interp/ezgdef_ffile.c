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

#include "ezscint.h"
#include "ez_funcdef.h"


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(ezgdef_ffile)(wordint *ni, wordint *nj, char *grtyp,
            wordint *ig1, wordint *ig2, wordint *ig3, wordint *ig4, 
            wordint *iunit, wordint lengrtyp)
{
  wordint icode;
  char lgrtyp[2],lgrref[2];

  lgrtyp[0] = grtyp[0];
  lgrtyp[1] = '\0';

  icode = c_ezgdef_ffile(*ni, *nj, lgrtyp, *ig1, *ig2, *ig3, *ig4, *iunit);
  return icode;
}

wordint c_ezgdef_ffile(wordint ni, wordint nj, char *grtyp,
          wordint ig1, wordint ig2, wordint ig3, wordint ig4, wordint iunit)
{
  wordint i;
  wordint found;
  char typeGrille;
  char grref[2];
  ftnfloat *bidon = NULL;
  int ok, ier, n, newsize;

  _Grille *gr;

  found = -1;
  typeGrille = (char)grtyp[0];

  if (typeGrille != '#' && typeGrille != 'Y' && typeGrille != 'Z')
    {
    strcpy(grref, " ");
    return c_ezgdef_fmem(ni, nj, grtyp, grref, ig1, ig2, ig3, ig4, bidon, bidon);
    }

  if (Grille == NULL)
    {
    Grille = (_Grille *) calloc(sizeof(_Grille),NMAXGRIDS);
    }
  
  ok = 0;
  i= 0;
  while (ok == 0 && i < nGrilles)
    {
    gr = (_Grille *) &(Grille[i]);
    if (ni  == gr->ni   && nj  == gr->nj   && ig1 == gr->ig[IG1]  && ig2 == gr->ig[IG2]  && ig3 == gr->ig[IG3]  && ig4 == gr->ig[IG4])
      {
      ok = 1;
      }

    if (ok)
      {
      switch (typeGrille)
        {
        case '#':
          if (typeGrille == gr->grtyp || typeGrille == 'Z') found = i;
          break;

        default:
          if (typeGrille == gr->grtyp) found = i;
          break;
        }
      }
    i++;
    }


  if (found == -1)
    {
    found = i;
    Grille[found].count = 1;
    nGrilles++;
    }
  else
    {
    Grille[found].count++;
    return found;
    }

  if (0 == (nGrilles % (NMAXGRIDS-1)))
    {
    newsize = sizeof(_Grille)*(nGrilles+NMAXGRIDS+4);
    Grille = (_Grille *) realloc((void *)Grille, newsize);
    for (n=nGrilles-1;n<(nGrilles+NMAXGRIDS+4);n++)
      {
      memset(&Grille[n], (int) NULL, sizeof(_Grille));
      }
    fprintf(stderr, "<ezgdef_ffile> : Reallocating ngrids to %d\n", (nGrilles+NMAXGRIDS+4));
/*    fprintf(stderr, "<ezgdef_ffile> : Too many defined grids. \n");
    fprintf(stderr, "<ezgdef_ffile> : No way but to abort. \n");
    exit(13);*/
    }
  else
    {
    Grille[found].flags = 0;
    }

  Grille[found].grtyp = typeGrille;
  Grille[found].ni  = ni;
  Grille[found].nj  = nj;
  Grille[found].ig[IG1] = ig1;
  Grille[found].ig[IG2] = ig2;
  Grille[found].ig[IG3] = ig3;
  Grille[found].ig[IG4] = ig4;
  Grille[found].axe_y_inverse = 0;

  switch(grtyp[0])
    {
    case '#':
      ier = LireEnrPositionnels(&(Grille[found]),iunit, ig1, ig2, ig3, ig4);
      if (ier < 0) return -1;
      break;

    default:
      ier = LireEnrPositionnels(&(Grille[found]),iunit, ig1, ig2, ig3, 0);
      if (ier < 0) return -1;
      break;
    }

  ez_calcxpncof(found);
  Grille[found].i1 = 1;
  Grille[found].i2 = ni;
  Grille[found].j1 = 1;
  Grille[found].j2 = nj;
  if (*grtyp != 'Y')
    {
    ier = ez_calcntncof(found);
    if (ier < 0) return -1;
    }

  if (groptions.verbose > 0)
    {
    printf("Found = %02d\n", found);
    printf("Grille[%02d].grtyp = '%c'\n", found, Grille[found].grtyp);
    printf("Grille[%02d].ni    = %d\n",   found, Grille[found].ni);
    printf("Grille[%02d].nj    = %d\n",   found, Grille[found].nj);
    printf("Grille[%02d].ig1   = %d\n",   found, Grille[found].ig[IG1]);
    printf("Grille[%02d].ig2   = %d\n",   found, Grille[found].ig[IG2]);
    printf("Grille[%02d].ig3   = %d\n",   found, Grille[found].ig[IG3]);
    printf("Grille[%02d].ig4   = %d\n",   found, Grille[found].ig[IG4]);
    }

  return found;
}

