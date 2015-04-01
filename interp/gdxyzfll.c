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
wordint f77name(gdxyzfll)(wordint *gdid, ftnfloat *x, ftnfloat *y, ftnfloat *lat, ftnfloat *lon, wordint *n)
{
   return c_gdxyzfll(*gdid, x, y, lat, lon, *n);
}

wordint c_gdxyzfll(wordint gdid, ftnfloat *x, ftnfloat *y, ftnfloat *lat, ftnfloat *lon, wordint n)
{
   wordint ni_in, nj_in;
   
   wordint coordonnee;

   _Grille grEntree;
   wordint npts;

   grEntree =  Grille[gdid];
   npts = n;
              
   ni_in =  grEntree.ni;
   nj_in =  grEntree.nj;

   
   switch(grEntree.grtyp)
      {
      case 'A':
      case 'B':
      case 'E':
      case 'G':
      case 'L':
      case 'N':
      case 'S':
      case 'T':
      case '!':
	c_gdxyfll(gdid, x, y, lat, lon, n);
        break;
        
      case 'Y':
	fprintf(stderr, "********************************************************\n");
	fprintf(stderr, "<gdllfxy>: This operation is not supported for 'Y' grids\n");
	fprintf(stderr, "********************************************************\n");
	break;
	
      case '#':
      case 'Z':
	coordonnee = ABSOLU;
        f77name(ez_ll2igd)(x, y, lat, lon, &npts,
			    &ni_in,&nj_in,&grEntree.grtyp, &grEntree.grref,
			    &grEntree.igref[IG1], &grEntree.igref[IG2], 
			    &grEntree.igref[IG3], &grEntree.igref[IG4],
			    grEntree.ax, grEntree.ay, &coordonnee);
        break;
        
        
      default:
        break;
      }


   return 0;
}
