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

#include <stdio.h>
#include <string.h>

#include "ez_funcdef.h"


int32_t f77name(ezgdef)(
    const int32_t * const ni,
    const int32_t * const nj,
    const char * const grtyp,
    const char * const grref,
    const int32_t * const ig1,
    const int32_t * const ig2,
    const int32_t * const ig3,
    const int32_t * const ig4,
    float * const ax,
    float * const ay,
    const F2Cl lengrtyp,
    const F2Cl lengrref
) {
    const char lgrtyp[2] = {grtyp[0], '\0'};
    const char lgrref[2] = {grref[0], '\0'};

    return c_ezgdef(*ni, *nj, lgrtyp, lgrref, *ig1, *ig2, *ig3, *ig4, ax, ay);
}


int32_t c_ezgdef(
    const int32_t ni,
    const int32_t nj,
    const char * const grtyp,
    const char * const grref,
    const int32_t ig1,
    const int32_t ig2,
    const int32_t ig3,
    const int32_t ig4,
    float * const ax,
    float * const ay
) {
    char typeGrille = grtyp[0];

    if (grtyp[0] == '#') {
        fprintf(stderr, "The '#' grid type is not supported with ezgdef.\nPlease use ezgdef_ffile or ezgdef_fmem\n");
        return -1;
    }

    int32_t source;
    switch(typeGrille) {
        case 'Y':
        case 'Z':
        case '#':
            if ((0 == strcmp(grref, "FILE")) || (0 == strcmp(grref, "file"))) {
                source = FICHIER;
            } else {
                source = MEMOIRE;
            }
            break;

        default:
            source = MEMOIRE;
            break;
    }

    int32_t found = -1;
    switch (source) {
        case MEMOIRE:
            found = c_ezgdef_fmem(ni, nj, grtyp, grref, ig1, ig2, ig3, ig4, ax, ay);
            break;

        case FICHIER:
            found = c_ezgdef_ffile(ni, nj, grtyp, ig1, ig2, ig3, ig4, ig4);
            break;
    }

    return found;
}

