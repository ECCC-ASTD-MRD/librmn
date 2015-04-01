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

wordint c_ezdefxg(wordint gdid)
{
  wordint i,j, ier, offsetx, offsety;
  wordint found;
  wordint res1, res2;
  static wordint ncalls = 0;
  char typeGrille;
  ftnfloat *temp, *axx, *ayy, *newaxex, *newaxey;
  wordint naxex, naxey;
  wordint nay, zero, deuxnj;
  
  _Grille *gr;
  
  gr = &Grille[gdid];
  
  switch (gr->grtyp)
    {
    case 'A':
    case 'G':
      gr->xg[DLON]  = 360. /gr->ni;
      gr->xg[SWLON] = 0.0; 
      switch (gr->ig[IG1])
		{
		case 0:
		  gr->xg[DLAT] = 180./gr->nj;
		  gr->xg[SWLAT] = -90. + 0.5*gr->xg[DLAT];
		  break;
		  
		case 1:
		  gr->xg[DLAT] = 90./gr->nj;
		  gr->xg[SWLAT] = 0.5*gr->xg[DLAT];
		  gr->needs_expansion = OUI;
		  break;
		  
		case 2:
		  gr->xg[DLAT] = 90./gr->nj;
		  gr->xg[SWLAT] = -90. + 0.5*gr->xg[DLAT];
		  gr->needs_expansion = OUI;
		  break;
		  
		default:
		  fprintf(stderr, "<ez_gdef_fmem> 'A' grid has to be Global/North/South\n");
		  break;
		}
      break;
      
    case 'B':
      gr->xg[DLON] = 360. /(gr->ni-1);
      gr->xg[SWLON] = 0.0;
      switch (gr->ig[IG1])
		{
		case 0:
		  gr->xg[DLAT] = 180./(gr->nj-1);
		  gr->xg[SWLAT] = -90.;
		  break;
		  
		case 1:
		  gr->xg[DLAT] = 90./(gr->nj-1);
		  gr->xg[SWLAT] = 0.;
		  gr->needs_expansion = OUI;
		  break;
		  
		case 2:
		  gr->xg[DLAT] = 90./(gr->nj-1);
		  gr->xg[SWLAT] = -90.;
		  gr->needs_expansion = OUI;
		  break;
		  
		default:
		  fprintf(stderr, "<ezgdef_fmem> 'B' grid has to be Global/North/South\n");
		  break;
		}
      break;
      
    case 'E':
      f77name(cigaxg)(&gr->grtyp,&gr->xg[XLAT1],&gr->xg[XLON1],&gr->xg[XLAT2],&gr->xg[XLON2],
		      &gr->ig[IG1],&gr->ig[IG2],&gr->ig[IG3],&gr->ig[IG4],1);
      gr->xg[DLAT] = 180./gr->nj;
      gr->xg[DLON] = 360./(gr->ni-1);
      gr->xg[SWLON] = 0.0;
      gr->xg[SWLAT] = -90. + 0.5*gr->xg[DLAT];
      break;

    case 'H':
    case 'Y':
    case '#':
    case 'Z':
    case '!':
      break;

    case 'L':
      f77name(cigaxg)(&gr->grtyp,&gr->xg[SWLAT], &gr->xg[SWLON], &gr->xg[DLAT], &gr->xg[DLON],
		      &gr->ig[IG1], &gr->ig[IG2], &gr->ig[IG3], &gr->ig[IG4],1);
      break;

    case 'N':
      f77name(cigaxg)(&gr->grtyp,&gr->xg[PI], &gr->xg[PJ], &gr->xg[D60], &gr->xg[DGRW],
		      &gr->ig[IG1], &gr->ig[IG2], &gr->ig[IG3], &gr->ig[IG4],1);
      gr->hemisphere = 1;
      break;

    case 'S':
      f77name(cigaxg)(&gr->grtyp,&gr->xg[PI], &gr->xg[PJ], &gr->xg[D60], &gr->xg[DGRW],
		      &gr->ig[IG1], &gr->ig[IG2], &gr->ig[IG3], &gr->ig[IG4],1);
      gr->hemisphere = 2;
      break;
      
    case 'T':
      f77name(cigaxg)(&gr->grtyp,&gr->xg[TD60], &gr->xg[TDGRW], &gr->xg[CLAT], &gr->xg[CLON],
		      &gr->ig[IG1], &gr->ig[IG2], &gr->ig[IG3], &gr->ig[IG4],1);
      break;
      
    case 'X':
      fprintf(stderr,"<c_ezgdef> There is no support for grid type 'X'\n");
      return -1;
      
    default:
      fprintf(stderr,"<c_ezgdef> Grid type not supported\n");
      return -1;
    }
  
  return gdid;

}
