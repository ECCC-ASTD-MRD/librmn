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
#include <stdio.h>
#include <stdint.h>

#include <rmn/rpnmacros.h>

#include <rmn/ezscint.h>
#include "ez_funcdef.h"

static float *eziglat = NULL;
static float *eziglon = NULL;

int32_t f77name(ez_igscint)(float *zo, int32_t *li, int32_t *lj, float *xlat, float *xlon,
                    float *zi, int32_t *ni, int32_t *nj,
                    char* grtyp, char *grref, int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4,
                    float *sym, float *ax, float *ay, F2Cl lengrtyp, F2Cl lengrref)
{
    ftnstrclean(grtyp,lengrtyp);
    ftnstrclean(grref,lengrref);

    int32_t npts = *li * *lj;
    float * tmplon = (float *) malloc(npts * sizeof(float));
    for (int32_t  i = 0; i < npts; i++) {
        tmplon[i] = xlon[i] < 0.0 ? xlon[i] + 360.0 : xlon[i];
    }

    int32_t gdin = c_ezgdef_fmem(*ni, *nj, grtyp, grref,  *ig1,  *ig2,  *ig3,  *ig4, ax,   ay);
    c_gdllsval(gdin, zo, zi, xlat, tmplon, npts);
    free(tmplon);
    return 0;
}


int32_t f77name(ez_rgscint)(float *zo, int32_t *li, int32_t *lj, float *xlat, float *xlon,
                    float *zi, int32_t *ni, int32_t *nj,
                    char* grtyp, int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4, float *sym,
                    F2Cl lengrtyp)
{
    int32_t npts = *li * *lj;
    float * tmplon = (float *) malloc(npts * sizeof(float));
    for (int32_t i = 0; i < npts; i++) {
        tmplon[i] = xlon[i] < 0.0 ? xlon[i] + 360.0 : xlon[i];
    }

    int32_t gdin = c_ezgdef_fmem(*ni, *nj, grtyp, NULL,  *ig1,  *ig2,  *ig3,  *ig4, NULL, NULL);
    c_gdllsval(gdin, zo, zi, xlat, tmplon, npts);
    free(tmplon);
    return 0;
}


int32_t f77name(ez_iguvint)(float *spdo, float *psio, int32_t *li, int32_t *lj, float *xlat, float *xlon,
                    float *ui, float *vi, int32_t *ni, int32_t *nj,
                    char* grtyp, char *grref, int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4,
                    float *sws, float *ax, float *ay, F2Cl lengrtyp, F2Cl lengrref)
{
    int32_t npts = *li * *lj;
    float * tmplon = (float *) malloc(npts * sizeof(float));
    for (int32_t i = 0; i < npts; i++) {
        tmplon[i] = xlon[i] < 0.0 ? xlon[i] + 360.0 : xlon[i];
    }

    ftnstrclean(grtyp,lengrtyp);
    ftnstrclean(grref,lengrref);
    int32_t gdin = c_ezgdef_fmem(*ni,*nj, grtyp, grref,  *ig1,  *ig2,  *ig3,  *ig4, ax, ay);
    c_gdllwdval(gdin, spdo, psio, ui, vi, xlat, tmplon, npts);
    free(tmplon);
}


int32_t f77name(ez_rguvint)(float *spdo, float *psio, int32_t *li, int32_t *lj, float *xlat, float *xlon,
                    float *ui, float *vi, int32_t *ni, int32_t *nj,
                    char* grtyp, int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4, float *sws, F2Cl lengrtyp)
{
    int32_t npts = *li * *lj;
    float * tmplon = (float *) malloc(npts * sizeof(float));
    for (int32_t i = 0; i < npts; i++) {
        tmplon[i] = xlon[i] < 0.0 ? xlon[i] + 360.0 : xlon[i];
    }

    ftnstrclean(grtyp,lengrtyp);
    int32_t gdin  = c_ezgdef(*ni,*nj, grtyp, NULL,  *ig1,  *ig2,  *ig3,  *ig4, NULL, NULL);
    c_gdllwdval(gdin, spdo, psio, ui, vi, xlat, tmplon, npts);
    free(tmplon);
}

