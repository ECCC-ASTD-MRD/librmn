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

#include <string.h>

#include <rmn/ezscint.h>
#include "ez_funcdef.h"


int32_t f77name(ezgdef_fll)(int32_t *ni, int32_t *nj, float *lat, float *lon)
{
    return c_ezgdef_fll(*ni, *nj, lat, lon);
}


int32_t c_ezgdef_fll(int32_t ni, int32_t nj,float *lat, float *lon)
{
   float swlat = 0.0;
   float swlon = 0.0;
   float dlat  = 1.0;
   float dlon  = 1.0;

   char grtyp[2], grref[2];
   strcpy(grtyp, "Y");
   strcpy(grref, "L");

   int32_t ig1, ig2, ig3, ig4;
   f77name(cxgaig)(grref, &ig1, &ig2, &ig3, &ig4, &swlat, &swlon, &dlat, &dlon);

   return c_ezgdef_fmem(ni, nj, grtyp, grref, ig1, ig2, ig3, ig4, lon, lat);
}

