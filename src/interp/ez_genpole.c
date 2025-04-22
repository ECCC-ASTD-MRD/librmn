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

#include "rmn/ezscint.h"
#include "rmn/f_ezscint.h"
#include "ez_funcdef.h"


int32_t f77name(ezgenpole)(
    float * const vpolnor,
    float * const vpolsud,
    const float * const fld,
    const int32_t * const ni,
    const int32_t * const nj,
    const int32_t * const vecteur,
    const char * const grtyp,
    const int32_t * const hem,
    F2Cl lengrtyp
) {
    return c_ezgenpole(vpolnor, vpolsud, fld, *ni, *nj, *vecteur, grtyp, *hem);
}

int32_t c_ezgenpole(
    float * const vpolnor,
    float * const vpolsud,
    const float * const fld,
    const int32_t ni,
    const int32_t nj,
    const int32_t vecteur,
    const char * const grtyp,
    const int32_t hem
) {
    const int32_t lni = ni;
    const int32_t lnj = nj;
    const int32_t lvecteur = vecteur;
    const int32_t lhem = hem;

    float * const x =    (float *) malloc(2 * lni * sizeof(float));
    float * const y =    (float *) malloc(2 * lni * sizeof(float));
    float * const z =    (float *) malloc(2 * lni * sizeof(float));
    float * const lat =  (float *) malloc(2 * lni * sizeof(float));
    float * const lon =  (float *) malloc(2 * lni * sizeof(float));
    float * const gausslat = (float *) malloc(2 * lnj * sizeof(float));

    f77name(ez_genpole)(vpolnor, vpolsud, fld, &lni, &lnj, &lvecteur, grtyp, &lhem,
                        x, y, z, lat, lon, gausslat, &groptions.degre_interp, 1);

    free(x);
    free(y);
    free(z);
    free(lat);
    free(lon);
    free(gausslat);

    return 0;
}
