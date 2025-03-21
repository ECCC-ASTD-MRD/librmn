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

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <math.h>

#include <rmn/ezscint.h>
#include "ez_funcdef.h"


int32_t c_ezsincoslatlon(
    float *lat, float *lon,
    float *sinlat, float *sinlon, float *coslat, float *coslon, int32_t npts)
{
    float dar = M_PI / 180.0;

    for (int32_t i = 0; i < npts; i++) {
        sinlat[i]  = sin(dar*lat[i]);
        coslat[i]  = cos(dar*lat[i]);
        sinlon[i]  = sin(dar*lon[i]);
        coslon[i]  = cos(dar*lon[i]);
    }

    return 0;
}
