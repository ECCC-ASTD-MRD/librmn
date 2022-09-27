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


void ez_calcntncof(int32_t gdid)
{
    int32_t gdcol, gdrow;

    c_gdkey2rowcol(gdid, &gdrow, &gdcol);
    // printf("ez_calcntncof(%d) - gdrow = %d, gdcol = %d\n", gdid, gdrow, gdcol);
    if (Grille[gdrow][gdcol].flags & NEWTON) return;

    int32_t nni = Grille[gdrow][gdcol].ni;
    int32_t nnj = Grille[gdrow][gdcol].j2 - Grille[gdrow][gdcol].j1 + 1;

    if (Grille[gdrow][gdcol].grtyp[0] == (char)'Y') return;

    Grille[gdrow][gdcol].ncx = (float *) malloc(nni * 6 * sizeof(float));
    Grille[gdrow][gdcol].ncy = (float *) malloc(nnj * 6 * sizeof(float));
    f77name(ez_nwtncof)(
        Grille[gdrow][gdcol].ncx, Grille[gdrow][gdcol].ncy,
        Grille[gdrow][gdcol].ax, Grille[gdrow][gdcol].ay,
        &Grille[gdrow][gdcol].ni, &Grille[gdrow][gdcol].nj,
        &Grille[gdrow][gdcol].i1, &Grille[gdrow][gdcol].i2,
        &Grille[gdrow][gdcol].j1, &Grille[gdrow][gdcol].j2,
        &Grille[gdrow][gdcol].extension
    );

    Grille[gdrow][gdcol].flags |= NEWTON;
}
