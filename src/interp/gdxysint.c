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


int32_t c_gdxysint(float *zout, float *zin, int32_t gdin, float *x, float *y, int32_t npts);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
int32_t f77name(gdxysint)(float *zout, float *zin, int32_t *gdin, float *x, float *y, int32_t *npts)
{
   int32_t icode;

   icode = c_gdxysint(zout, zin, *gdin, x, y, *npts);
   return icode;
}

int32_t c_gdxysint(float *zout, float *zin, int32_t gdin, float *x, float *y, int32_t npts)
{
   int32_t ier;
   float *lzin, *lxzin;
  _zone zones[NZONES];
  int32_t gdout;
  _gridset tmpset;
  int32_t gdrow_in, gdcol_in;

   lzin  = NULL;
   lxzin = NULL;


  c_gdkey2rowcol(gdin,  &gdrow_in,  &gdcol_in);

   if (Grille[gdrow_in][gdcol_in].fst.axe_y_inverse == 1)
      {
      lzin = (float *) malloc(Grille[gdrow_in][gdcol_in].ni*Grille[gdrow_in][gdcol_in].nj*sizeof(float));
      memcpy(lzin, zin, Grille[gdrow_in][gdcol_in].ni*Grille[gdrow_in][gdcol_in].nj*sizeof(float));
      f77name(permut)(lzin, &Grille[gdrow_in][gdcol_in].ni, &Grille[gdrow_in][gdcol_in].nj);
      }
   else
     {
     lzin = zin;
     }

   if (Grille[gdrow_in][gdcol_in].needs_expansion == OUI)
     {
     lxzin = (float *) malloc(2*Grille[gdrow_in][gdcol_in].ni*Grille[gdrow_in][gdcol_in].nj*sizeof(float));
     ez_xpnsrcgd(gdin, lxzin, lzin);
     }
   else
     {
     lxzin = lzin;
     }

   ier = c_gdinterp(zout, lxzin, gdin, x, y, npts);

   gdout = NMAXGRIDS-1;
/*   tmpset.gdin = gdin;
   tmpset.gdout = gdout;*/
   tmpset.x = x;
   tmpset.y = y;
/*
   Grille[gdout].ni = npts;
   Grille[gdout].nj = 1;
   Grille[gdout].grtyp = 'L';
*/

/*   if (groptions.polar_correction == OUI && groptions.vecteur != VECTEUR)
     {
     ier = ez_defzones(&tmpset);
     ier = ez_corrval(zout, lxzin, &tmpset);
     }*/


/*   if (lzin != zin && lzin != NULL)
      {
      free(lzin);
      }

   if (lxzin != lzin && lxzin != zin && lxzin != NULL)
      {
      free(lxzin);
      }*/

   return 0;
}
