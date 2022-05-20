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
#include <string.h>

#include "ez_funcdef.h"


int32_t c_gduvfwd_orig(int32_t gdid,  float *uugdout, float *vvgdout, float *uullin, float *vvllin,
              float *latin, float *lonin, int32_t npts) {
    int32_t gdrow_id, gdcol_id;
    c_gdkey2rowcol(gdid,  &gdrow_id,  &gdcol_id);

    int32_t ni = npts;
    int32_t nj = 1;

    memcpy(uugdout, uullin, npts*sizeof(float));
    memcpy(vvgdout, vvllin, npts*sizeof(float));

    float * lat_true;
    float * lon_true;

    switch (Grille[gdrow_id][gdcol_id].grtyp[0]) {
        case 'E':
            lat_true = (float *)(malloc(npts * sizeof(float)));
            lon_true = (float *)(malloc(npts * sizeof(float)));
            f77name(ez_gfxyfll)(lon_true, lat_true, lonin, latin, &ni,
                                &Grille[gdrow_id][gdcol_id].fst.xg[XLAT1], &Grille[gdrow_id][gdcol_id].fst.xg[XLON1],
                                &Grille[gdrow_id][gdcol_id].fst.xg[XLAT2], &Grille[gdrow_id][gdcol_id].fst.xg[XLON2]);

            c_ezgfwfllw(uugdout, vvgdout, latin, lonin, lat_true, lon_true,
                        &ni, &nj, Grille[gdrow_id][gdcol_id].grtyp,
                        &Grille[gdrow_id][gdcol_id].fst.ig[IG1], &Grille[gdrow_id][gdcol_id].fst.ig[IG2],
                        &Grille[gdrow_id][gdcol_id].fst.ig[IG3], &Grille[gdrow_id][gdcol_id].fst.ig[IG4]);
            free(lat_true);
            free(lon_true);
            return 0;
            break;


        case '#':
        case 'Y':
        case 'Z':
            switch (Grille[gdrow_id][gdcol_id].grref[0]) {
                case 'E':
                    lat_true = (float *)(malloc(npts*sizeof(float)));
                    lon_true = (float *)(malloc(npts*sizeof(float)));
                    f77name(ez_gfxyfll)(lonin, latin, lon_true, lat_true, &ni,
                                        &Grille[gdrow_id][gdcol_id].fst.xgref[XLAT1], &Grille[gdrow_id][gdcol_id].fst.xgref[XLON1],
                                        &Grille[gdrow_id][gdcol_id].fst.xgref[XLAT2], &Grille[gdrow_id][gdcol_id].fst.xgref[XLON2]);

                    c_ezgfwfllw(uugdout, vvgdout, latin, lonin, lat_true, lon_true,
                                &ni, &nj, Grille[gdrow_id][gdcol_id].grref,
                                &Grille[gdrow_id][gdcol_id].fst.igref[IG1], &Grille[gdrow_id][gdcol_id].fst.igref[IG2],
                                &Grille[gdrow_id][gdcol_id].fst.igref[IG3], &Grille[gdrow_id][gdcol_id].fst.igref[IG4]);
                    free(lat_true);
                    free(lon_true);
                    return 0;
                    break;

                default:
                    f77name(ez_gdwfllw)(uugdout, vvgdout, lonin, &ni, &nj, &Grille[gdrow_id][gdcol_id].grref,
                                        &Grille[gdrow_id][gdcol_id].fst.igref[IG1], &Grille[gdrow_id][gdcol_id].fst.igref[IG2],
                                        &Grille[gdrow_id][gdcol_id].fst.igref[IG3], &Grille[gdrow_id][gdcol_id].fst.igref[IG4], 1);
                    break;
            }

        default:
            f77name(ez_gdwfllw)(uugdout, vvgdout, lonin, &ni, &nj, &Grille[gdrow_id][gdcol_id].grtyp,
                                &Grille[gdrow_id][gdcol_id].fst.ig[IG1], &Grille[gdrow_id][gdcol_id].fst.ig[IG2],
                                &Grille[gdrow_id][gdcol_id].fst.ig[IG3], &Grille[gdrow_id][gdcol_id].fst.ig[IG4], 1);
            break;
    }

    return 0;
}


int32_t c_gduvfwd(int32_t gdid,  float *uugdout, float *vvgdout, float *uullin, float *vvllin,
              float *latin, float *lonin, int32_t npts) {
    int32_t gdrow_id, gdcol_id;
    c_gdkey2rowcol(gdid,  &gdrow_id,  &gdcol_id);
    if (Grille[gdrow_id][gdcol_id].nsubgrids > 0 ) {
        fprintf(stderr, "<gduvfwd>: This operation is not supported for 'U' grids\n");
        return -1;
    } else {
        return c_gduvfwd_orig(gdid, uugdout, vvgdout, uullin, vvllin, latin, lonin, npts);;
    }
}


int32_t f77name(gduvfwd)(int32_t *gdid, float *uugdout, float *vvgdout,
                         float *uullin, float *vvllin, float *latin, float *lonin, int32_t *npts) {
    return c_gduvfwd(*gdid, uugdout, vvgdout, uullin, vvllin, latin, lonin, *npts);
}
