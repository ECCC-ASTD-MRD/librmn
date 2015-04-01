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
wordint ez_interp(ftnfloat *zout, ftnfloat *zin, wordint iset)
{
   wordint ni_in, nj_in, ni_out, nj_out, ninj_out ,gdin, gdout;
   
   if (gridset[iset].flags & XXX)
      {
      gdin  = gridset[iset].gdin;
      gdout = gridset[iset].gdout;
      
      ni_in =  Grille[gdin].ni;
      nj_in =  Grille[gdin].nj;
      
      ni_out = Grille[gdout].ni;
      nj_out = Grille[gdout].nj;
      ninj_out = ni_out * nj_out;
      
      c_gdinterp(zout, zin, gdin, gridset[iset].x,gridset[iset].y, ninj_out);
      }
   return 0;   
}

