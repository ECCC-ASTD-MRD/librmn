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


void f77name(ezgfllfxy)(
    float *lonp, float *latp,
    float *lon, float *lat,
    float *r, float *ri, int32_t *npts,
    float *xlat1, float *xlon1, float *xlat2, float *xlon2)
{
    c_ezgfllfxy(lonp, latp, lon, lat, r, ri, npts, xlat1, xlon1, xlat2, xlon2);
}



void  c_ezgfllfxy(float *lonp, float *latp,
                 float *lon, float *lat,
                 float *r, float *ri, int32_t *npts,
                 float *xlat1, float *xlon1, float *xlat2, float *xlon2)
{
    float *cart, *carot;
    int32_t trois = 3;

    cart = (float *) malloc(3 * *npts * sizeof(float));
    carot = (float *) malloc(3 * *npts * sizeof(float));

    f77name(ez_crot)(r, ri, xlon1, xlat1, xlon2, xlat2);
    f77name(ez_lac)(cart, lon, lat, npts);
    f77name(mxm)(ri, &trois, cart, &trois, carot, npts);
    f77name(ez_cal)(lonp, latp, carot, npts);

    free(cart);
    free(carot);
}
