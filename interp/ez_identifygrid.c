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

wordint c_ezidentifygrid(wordint ni, wordint nj, char *grtyp, char *grref,
	     wordint ig1, wordint ig2, wordint ig3, wordint ig4, ftnfloat *ax, ftnfloat *ay)
{
  wordint i;
  wordint gdid;
  wordint res1, res2;
  char typeGrille;
  _Grille *gr;
  
  gdid = -1;
  typeGrille = grtyp[0];
  
  if (Grille == NULL)
    {
    Grille = (_Grille *) malloc(sizeof(_Grille)*NMAXGRIDS);
    }
    
  for (i=0; i < nGrilles; i++)
    {
    gr = &Grille[i];
    switch (typeGrille)
      {
      case '#':
      case 'Z':
      if (typeGrille == gr->grtyp && grref[0] == gr->grref &&
          ni == gr->ni && nj == gr->nj &&
          ig1 == gr->igref[IG1]  && ig2 == gr->igref[IG2] && 
          ig3 == gr->igref[IG3]  && ig4 == gr->igref[IG4])
        {
        if (gr->ax != NULL)
          {
          res1 = memcmp(ay, gr->ay, nj*sizeof(ftnfloat));
          res2 = memcmp(ax, gr->ax, ni*sizeof(ftnfloat));
          if (res1 == 0 && res2 == 0)
            {
            gdid = i;
            }
          }
        }
      break;
	
      case 'Y':
      if (typeGrille == gr->grtyp && grref[0] == gr->grref &&
          ni == gr->ni && nj == gr->nj &&
          ig1 == gr->igref[IG1]  && ig2 == gr->igref[IG2] && 
          ig3 == gr->igref[IG3]  && ig4 == gr->igref[IG4])
        {
        if (gr->lat != NULL)
          {
          res1 = memcmp(ay, gr->lat, ni*nj*sizeof(ftnfloat));
          res2 = memcmp(ax, gr->lon, ni*nj*sizeof(ftnfloat));
          if (res1 == 0 && res2 == 0)
            {
            gdid = i;
            }
          }
        }
      break;
	
      default:
      if (typeGrille == gr->grtyp && ni == gr->ni && nj == gr->nj &&
          ig1 == gr->ig[IG1]  && ig2 == gr->ig[IG2]  &&
          ig3 == gr->ig[IG3]  && ig4 == gr->ig[IG4])
          {
          gdid = i;
          }
      break;
      }
    }
  
  if (gdid == -1)
    {
    gdid = i;
    memset(&Grille[gdid],(int)NULL, sizeof(_Grille));
    Grille[gdid].count++;
    nGrilles++;
    }
  else
    {
    Grille[gdid].count++;
    return gdid;
    }

  if (0 == (nGrilles % NMAXGRIDS))
    {
    Grille = (_Grille *) realloc(Grille, sizeof(_Grille)*(nGrilles+32));
    memset(&Grille[gdid],(int)NULL, sizeof(_Grille));
    gr = &Grille[gdid];
/*    fprintf(stderr, "<ezgdef_ffile> : Too many defined grids. \n");
    fprintf(stderr, "<ezgdef_ffile> : No way but to abort. \n");
    exit(13);*/
    }
    
    gr = &Grille[gdid];
    gr->flags = 0;
    gr->count = 1;
  
/*  if (nGrilles > NMAXGRIDS)
    {
    fprintf(stderr, "<ezgdef_fmem> : Too many defined grids. \n");
    fprintf(stderr, "<ezgdef_fmem> : No way but to abort. \n");
    exit(13);
    }
  else
    {
    gr->flags = 0;
    }*/
  
  gr->grtyp = typeGrille;
  if (grref != NULL)
    {
    gr->grref = grref[0];
    gr->igref[IG1] = ig1;
    gr->igref[IG2] = ig2;
    gr->igref[IG3] = ig3;
    gr->igref[IG4] = ig4;
    }
  gr->ni  = ni;
  gr->nj  = nj;
  gr->ig[IG1] = ig1;
  gr->ig[IG2] = ig2;
  gr->ig[IG3] = ig3;
  gr->ig[IG4] = ig4;
  gr->axe_y_inverse = 0;
  gr->needs_expansion = NON;
  
  return gdid;
}

