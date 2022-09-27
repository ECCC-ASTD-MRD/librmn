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
#include <stdlib.h>
#include <math.h>

#include <rmn/ezscint.h>
#include "ez_funcdef.h"


int32_t ez_defzone_polenord(int32_t gdin, float *x, float *y, int32_t npts, _zone *zone) {
    float *tmpx, *tmpy;
    float latpolenord, lonpolenord, xpolenord, ypolenord;
    int32_t nhits;
    int32_t *tmpidx;

    int32_t gdrow_in, gdcol_in;

    c_gdkey2rowcol(gdin,  &gdrow_in,  &gdcol_in);

    /* On commence par trouver les points au pole nord */
    tmpx =   (float *) malloc(npts*sizeof(float));
    tmpy =   (float *) malloc(npts*sizeof(float));
    tmpidx = (int32_t  *) malloc(npts*sizeof(int32_t));

    nhits = 0;
    if (Grille[gdrow_in][gdcol_in].grtyp[0] == 'Z' && Grille[gdrow_in][gdcol_in].grref[0] == 'E') {
        xpolenord = 0.5 * Grille[gdrow_in][gdcol_in].ni;
        ypolenord = Grille[gdrow_in][gdcol_in].nj+0.5;
    } else {
        latpolenord = 90.0;
        lonpolenord = 0.0;
        c_gdxyfll_orig(gdin, &xpolenord, &ypolenord,  &latpolenord, &lonpolenord, 1);
    }

    for (int32_t i = 0; i < npts; i++) {
        if (fabs(y[i]-ypolenord) < 1.0e-3) {
            tmpx[nhits] = x[i];
            tmpy[nhits] = y[i];
            tmpidx[nhits]=i;
            nhits++;
        }
    }

    zone->npts = nhits;
    if (nhits > 0) {
        zone->x = (float *) malloc(nhits*sizeof(float));
        zone->y = (float *) malloc(nhits*sizeof(float));
        zone->idx = (int32_t *) malloc(nhits*sizeof(int32_t));
        if (groptions.verbose > 0) {
            fprintf(stderr, "Nombre de points au pole nord: %d\n", nhits);
        }

        for (int32_t i = 0; i < zone->npts; i++) {
            zone->x[i]   = tmpx[i];
            zone->y[i]   = tmpy[i];
            zone->idx[i] = tmpidx[i];
        }
    }

    free(tmpx);
    free(tmpy);
    free(tmpidx);
    return 0;
}
