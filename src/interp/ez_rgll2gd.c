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
#include <string.h>

#include <rmn/rpnmacros.h>
#include <rmn/ezscint.h>
#include "ez_funcdef.h"

int f77name(ez_rgll2gd)(float *z1, float *z2, float *xlon, int32_t *ni, int32_t *nj,
                    char *grtyp, int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4,F2Cl lengrtyp)
{
    float *tmplat;
    char lgrtyp[2];

    int32_t npts = *ni * *nj;
    tmplat = (float *)malloc(npts*sizeof(float));
    for (int32_t n = 0; n < npts; n++) {
        tmplat[n] = 0.0;
    }

    ftnstrclean(grtyp,lengrtyp);
    strcpy(lgrtyp, grtyp);

    int32_t gdid = c_ezqkdef(*ni, *nj, lgrtyp, *ig1,  *ig2,  *ig3,  *ig4, 0);

    c_gduvfwd(gdid, z1, z2, z1, z2, tmplat, xlon, npts);

    free(tmplat);
    return 0;
}
