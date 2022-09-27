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

#include <stdlib.h>

#include <rmn/ezscint.h>
#include "ez_funcdef.h"


int32_t f77name(ezgenpole)(float *vpolnor, float *vpolsud, float *fld,
                           int32_t *ni, int32_t *nj, int32_t *vecteur,
                           char *grtyp, int32_t *hem, F2Cl lengrtyp)
{
   return c_ezgenpole(vpolnor, vpolsud, fld, *ni, *nj, *vecteur, grtyp, *hem);

}

int32_t c_ezgenpole(float *vpolnor, float *vpolsud, float *fld,
                           int32_t ni, int32_t nj, int32_t vecteur,
                           char *grtyp, int32_t hem)
{
   int32_t lni, lnj, lvecteur, lhem;
   float *x, *y, *z, *lat, *lon, *gausslat;

   lni = ni;
   lnj = nj;
   lvecteur = vecteur;
   lhem = hem;

   x =    (float *) malloc(2*lni*sizeof(float));
   y =    (float *) malloc(2*lni*sizeof(float));
   z =    (float *) malloc(2*lni*sizeof(float));
   lat =  (float *) malloc(2*lni*sizeof(float));
   lon =  (float *) malloc(2*lni*sizeof(float));
   gausslat = (float *) malloc(2*lnj*sizeof(float));

   f77name(ez_genpole)(vpolnor, vpolsud, fld, &lni, &lnj, &lvecteur, grtyp, &lhem,
                       x,y,z,lat,lon,gausslat, &groptions.degre_interp,1);

   free(x);
   free(y);
   free(z);
   free(lat);
   free(lon);
   free(gausslat);

   return 0;
}
