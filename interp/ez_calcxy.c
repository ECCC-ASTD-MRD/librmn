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
wordint ez_calcxy(wordint iset)
{
   wordint coordonnee, ni_in, nj_in, ni_out, nj_out, ninj_out, gdin, gdout;
   wordint i,j;

   if (gridset[iset].flags & XXX)
      {
      return 0;
      }

   /* Dans un premier temps on calcule la position x-y de tous les points sur la grille */

   gdin  = gridset[iset].gdin;
   gdout = gridset[iset].gdout;
   
   ni_in =  Grille[gdin].ni;
   nj_in =  Grille[gdin].nj;
   
   ni_out = Grille[gdout].ni;
   nj_out = Grille[gdout].nj;
   ninj_out = ni_out * nj_out;
   
   gridset[iset].x = (ftnfloat *) malloc(ninj_out*sizeof(ftnfloat));
   gridset[iset].y = (ftnfloat *) malloc(ninj_out*sizeof(ftnfloat));
   
   switch(Grille[gdin].grtyp)
      {
      case 'A':
      case 'B':
      case 'E':
      case 'L':
      case 'N':
      case 'S':
      case 'T':
      case '!':
        f77name(ez_ll2rgd)(gridset[iset].x, gridset[iset].y,
                           Grille[gdout].lat, Grille[gdout].lon, &ninj_out,
                           &ni_in, &nj_in, &Grille[gdin].grtyp,
                           &Grille[gdin].ig[IG1], &Grille[gdin].ig[IG2], 
			   &Grille[gdin].ig[IG3], &Grille[gdin].ig[IG4],
                           &groptions.symmetrie, Grille[gdin].ay);
        break;
        
        
      case '#':
      case 'Z':
      case 'G':
	coordonnee = RELATIF;
        f77name(ez_ll2igd)(gridset[iset].x, gridset[iset].y,
			   Grille[gdout].lat,Grille[gdout].lon,&ninj_out,
			   &ni_in,&nj_in,&Grille[gdin].grtyp, &Grille[gdin].grref,
			   &Grille[gdin].igref[IG1], &Grille[gdin].igref[IG2], 
			   &Grille[gdin].igref[IG3], &Grille[gdin].igref[IG4],
			   Grille[gdin].ax, Grille[gdin].ay, 
			   &coordonnee);
	if (Grille[gdin].grtyp == 'G')
	  {
	  if (Grille[gdin].ig[IG1] == NORD)
	    {
	    for (j=0; j < ni_out*nj_out; j++)
	      {
	      gridset[iset].y[j] -= nj_in;
	      }
	    }
          }
        break;
        
        
      default:
        break;
      }

   gridset[iset].flags |= XXX;
   
   return 0;
}

