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
wordint f77name(ezsint)(ftnfloat *zout, ftnfloat *zin)
{
   wordint icode;
   
   icode = c_ezsint(zout, zin);
   return icode;
}

wordint c_ezsint(ftnfloat *zout, ftnfloat *zin)
{
   wordint gdin, gdout;
   wordint ier;
   wordint npts;
   ftnfloat *lzin, *lxzin;

   lzin  = NULL;
   lxzin = NULL;

   if (iset == UNDEFINED)
      {
      fprintf(stderr,"<c_ezsint> Source and target grid undefined! Aborting...\n");
      return -1;
      }

   
   gdin = gridset[iset].gdin;
   gdout = gridset[iset].gdout;

   if (gridset[iset].gdin == gridset[iset].gdout)
      {
      memcpy(zout, zin, Grille[gdin].ni*Grille[gdin].nj*sizeof(ftnfloat));
      return 1;
      }

   if (Grille[gdin].axe_y_inverse == 1)
      {
      lzin = (ftnfloat *) malloc(Grille[gdin].ni*Grille[gdin].nj*sizeof(ftnfloat));
      memcpy(lzin, zin, Grille[gdin].ni*Grille[gdin].nj*sizeof(ftnfloat));
      f77name(permut)(lzin, &Grille[gdin].ni, &Grille[gdin].nj);
      }
   else
     {
     lzin = zin;
     }

   if (Grille[gdin].needs_expansion == OUI)
     {
     lxzin = (ftnfloat *) malloc(2*Grille[gdin].ni*Grille[gdin].nj*sizeof(ftnfloat));
     ez_xpnsrcgd(gdin, lxzin, lzin);
     }
   else
     {
     lxzin = lzin;
     }

   ier = ez_calclatlon(gridset[iset].gdout);
   ier = ez_calcxy(iset);
   npts = Grille[gdout].ni*Grille[gdout].nj;

   ier = ez_interp(zout, lxzin, iset);

   if (groptions.polar_correction == OUI)
     {
     ier = ez_defzones(&gridset[iset]);
     ier = ez_corrval(zout, lxzin, &gridset[iset]);
     }
   
   if (lzin != zin && lzin != NULL)
      {
      free(lzin);
      }

   if (lxzin != lzin && lxzin != zin && lxzin != NULL)
      {
      free(lxzin);
      }

   return 0;
}
