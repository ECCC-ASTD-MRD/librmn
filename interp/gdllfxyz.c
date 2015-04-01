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
wordint f77name(gdllfxyz)(wordint *gdid, ftnfloat *lat, ftnfloat *lon, ftnfloat *x, ftnfloat *y, wordint *n)
{
  return c_gdllfxyz(*gdid, lat, lon, x, y, *n);
}

wordint c_gdllfxyz(wordint gdid, ftnfloat *lat, ftnfloat *lon, ftnfloat *x, ftnfloat *y, wordint n)
{
  ftnfloat xlat00, xlon00, dlat, dlon;
  ftnfloat xlat1, xlon1, xlat2, xlon2;
  ftnfloat pi, pj, d60, dgrw;
  wordint i,j,ni, nj, npts, hem, un;
  
  _Grille grEntree;
  
  grEntree = Grille[gdid];
  npts = n;
  
  switch(grEntree.grtyp)
    {
    case 'A':
    case 'B':
    case 'G':
    case 'L':
    case 'N':
    case 'S':
    case 'T':
    case '!':
      c_gdllfxy(gdid, lat, lon, x, y, n);
      break;
      
    case 'Y':
      fprintf(stderr, "********************************************************\n");
      fprintf(stderr, "<gdllfxy>: This operation is not supported for 'Y' grids\n");
      fprintf(stderr, "********************************************************\n");
      break;
      
    case '#':
    case 'Z':
      switch (grEntree.grref)
	{
	case 'E':
	  f77name(ez_gfllfxy)(lon,lat,x,y,&npts,&grEntree.xgref[XLAT1],&grEntree.xgref[XLON1],
			      &grEntree.xgref[XLAT2],&grEntree.xgref[XLON2]);
	  break;
	  
	case 'S':
	case 'N':
	  if (grEntree.grref == 'N') 
	    hem = 1;
	  else
	    hem = 2;

	  un = 1;
	  f77name(ez_vllfxy)(lat,lon,x,y,&npts,&un,&grEntree.xgref[D60],
			  &grEntree.xgref[DGRW], &grEntree.xgref[PI], &grEntree.xgref[PJ],&grEntree.hemisphere);
	  break;
	  
	case 'L':
	  for (i=0; i < n; i++)
	    {
	      lat[i] = (x[i])*grEntree.xgref[DLAT]+ grEntree.xgref[SWLAT];
	      lon[i] = (y[i])*grEntree.xgref[DLON]+ grEntree.xgref[SWLON];
	      lon[i] = lon[i] < 0.0 ? lon[i] + 360.0 : lon[i];
	    }
	  break;
	  
	default:
	  fprintf(stderr,"<gdllfxy> Errrrrrrrrrrreur!\n");
	  break;
	}
      break;
    }
  
  return 0;
  
}
