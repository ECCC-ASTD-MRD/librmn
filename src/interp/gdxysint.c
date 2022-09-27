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

#include <rmn/ezscint.h>
#include "ez_funcdef.h"


int32_t c_gdxysint(float *zout, float *zin, int32_t gdin, float *x, float *y, int32_t npts) {
    int32_t gdrow_in, gdcol_in;
    c_gdkey2rowcol(gdin,  &gdrow_in,  &gdcol_in);

    float * lzin = NULL;
    if (Grille[gdrow_in][gdcol_in].fst.axe_y_inverse == 1) {
        lzin = (float *) malloc(Grille[gdrow_in][gdcol_in].ni*Grille[gdrow_in][gdcol_in].nj*sizeof(float));
        memcpy(lzin, zin, Grille[gdrow_in][gdcol_in].ni*Grille[gdrow_in][gdcol_in].nj*sizeof(float));
        f77name(permut)(lzin, &Grille[gdrow_in][gdcol_in].ni, &Grille[gdrow_in][gdcol_in].nj);
    } else {
        lzin = zin;
    }

    float * lxzin = NULL;
    if (Grille[gdrow_in][gdcol_in].needs_expansion == 1) {
        lxzin = (float *) malloc(2*Grille[gdrow_in][gdcol_in].ni*Grille[gdrow_in][gdcol_in].nj*sizeof(float));
        ez_xpnsrcgd(gdin, lxzin, lzin);
    } else {
        lxzin = lzin;
    }

    c_gdinterp(zout, lxzin, gdin, x, y, npts);

    return 0;
}


int32_t f77name(gdxysint)(float *zout, float *zin, int32_t *gdin, float *x, float *y, int32_t *npts) {
    return c_gdxysint(zout, zin, *gdin, x, y, *npts);
}
