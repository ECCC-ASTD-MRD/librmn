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

wordint ez_freezones(_gridset *gdset);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(gdxysval)(wordint *gdin, ftnfloat *zout, ftnfloat *zin, ftnfloat *x, ftnfloat *y, wordint *n)
{
   wordint icode;
   
   icode = c_gdxysval(*gdin, zout, zin, x, y, *n);
   return icode;
}

wordint c_gdxysval(wordint gdin, ftnfloat *zout, ftnfloat *zin, ftnfloat *x, ftnfloat *y, wordint n)
{
   wordint ninj_out;
   wordint ier;
   wordint local_gdout,local_iset;
   
   ninj_out = n;

   ier = c_gdxysint(zout, zin, gdin, x, y, n);

   return 0;
}


wordint ez_freezones(_gridset *gdset)
{
  wordint i;

  for (i=0; i < NZONES; i++)
    {
    if (gdset->zones[i].npts > 0)
      {
      free(gdset->zones[i].idx);
      free(gdset->zones[i].x);
      free(gdset->zones[i].y);
      gdset->zones[i].npts = 0;
      gdset->zones[i].idx  = NULL;
      gdset->zones[i].x  = NULL;
      gdset->zones[i].y = NULL;
      }
    }
 return 0;
}
