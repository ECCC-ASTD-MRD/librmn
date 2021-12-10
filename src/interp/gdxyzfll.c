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
#include "ez_funcdef.h"


int32_t f77name(gdxyzfll)(int32_t *gdid, float *x, float *y, float *lat, float *lon, int32_t *n) {
    return c_gdxyzfll(*gdid, x, y, lat, lon, *n);
}


int32_t c_gdxyzfll(int32_t gdid, float *x, float *y, float *lat, float *lon, int32_t n) {
    int32_t ni_in, nj_in;

    int32_t coordonnee;

    _Grille grEntree;
    int32_t npts;

    int32_t gdrow_id, gdcol_id;

    c_gdkey2rowcol(gdid,  &gdrow_id,  &gdcol_id);
    grEntree =  Grille[gdrow_id][gdcol_id];
    npts = n;

    ni_in =  grEntree.ni;
    nj_in =  grEntree.nj;


    switch(grEntree.grtyp[0]) {
        case 'A':
        case 'B':
        case 'E':
        case 'G':
        case 'L':
        case 'N':
        case 'S':
        case 'T':
        case '!':
            c_gdxyfll_orig(gdid, x, y, lat, lon, n);
            break;
        case 'Y':
            fprintf(stderr, "********************************************************\n");
            fprintf(stderr, "<gdxyzfll>: This operation is not supported for 'Y' grids\n");
            fprintf(stderr, "********************************************************\n");
            break;
        case '#':
        case 'Z':
            coordonnee = ABSOLU;
            f77name(ez_ll2igd)(x, y, lat, lon, &npts,
                &ni_in,&nj_in,&grEntree.grtyp, &grEntree.grref,
                &grEntree.fst.igref[IG1], &grEntree.fst.igref[IG2],
                &grEntree.fst.igref[IG3], &grEntree.fst.igref[IG4],
                grEntree.ax, grEntree.ay, &coordonnee);
            break;

        default:
            break;
    }

    return 0;
}
