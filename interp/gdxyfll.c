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
wordint f77name(gdxyfll)(wordint *gdid, ftnfloat *x, ftnfloat *y, ftnfloat *lat, ftnfloat *lon, wordint *n)
{
  return c_gdxyfll(*gdid, x, y, lat, lon, *n);
}

wordint c_gdxyfll(wordint gdid, ftnfloat *x, ftnfloat *y, ftnfloat *lat, ftnfloat *lon, wordint n)
{
  ftnfloat *tmplons;
  
  wordint ni_in, nj_in;
  wordint sym=groptions.symmetrie;
  
  
  _Grille gr;
  wordint npts;
  wordint coordonnee;
  
  gr =  Grille[gdid];
  npts = n;
  
  ni_in =  gr.ni;
  nj_in =  gr.nj;

  switch(gr.grtyp)
    {
    case 'A':
    case 'B':
    case 'E':
    case 'L':
    case 'N':
    case 'S':
    case 'T':
    case '!':
      tmplons = (ftnfloat *)malloc(npts * sizeof(ftnfloat));
      memcpy(tmplons,lon,sizeof(ftnfloat)*npts);
      
      f77name(ez_ll2rgd)(x, y,
			 lat, tmplons, &npts,
			 &ni_in, &nj_in, &gr.grtyp,
			 &gr.ig[IG1], &gr.ig[IG2], &gr.ig[IG3], &gr.ig[IG4],
			 &sym, gr.ay);
      free(tmplons);
      break;

    case '#':
    case 'Z':
    case 'G':
      coordonnee = RELATIF;
      nj_in =  gr.j2;
      f77name(ez_ll2igd)(x, y, lat, lon, &npts,
			 &ni_in,&nj_in,&gr.grtyp, &gr.grref,
			 &gr.igref[IG1], &gr.igref[IG2], 
			 &gr.igref[IG3], &gr.igref[IG4],
			 gr.ax, gr.ay,&coordonnee);
      break;
      
      
    default:
      break;
    }
  
  
  return 0;
}
