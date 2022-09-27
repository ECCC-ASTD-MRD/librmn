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


int32_t ez_corrval_aunord(float *zout, float *zin, int32_t gdin, int32_t gdout) {
    const int32_t un = 1;
    const int32_t quatre = 4;

    int32_t gdrow_in, gdrow_out, gdcol_in, gdcol_out;
    c_gdkey2rowcol(gdin,  &gdrow_in,  &gdcol_in);
    c_gdkey2rowcol(gdout, &gdrow_out, &gdcol_out);
    int32_t idx_gdin = c_find_gdin(gdin, gdout);

    _gridset * gset = &(Grille[gdrow_out][gdcol_out].gset[idx_gdin]);
    int32_t npts = gset->zones[AU_NORD].npts;
    _Grille * lgd = (_Grille *) &(Grille[gdrow_in][gdcol_in]);
    if (npts > 0) {
        int32_t ni = lgd->ni;
        int32_t nj = lgd->j2 - lgd->j1 + 1;
        int32_t j1 = lgd->j2-2;
        int32_t j2 = j1 + 3;

        float * temp = (float *) malloc(4 * ni * sizeof(float));
        float * vals = (float *) malloc(npts * sizeof(float));

        float poleval;
        f77name(ez_calcpoleval)(&poleval, &zin[(nj-1)*ni], &ni, lgd->ax, &(lgd->grtyp), &(lgd->grref),1,1);
        f77name(ez_fillnpole)(temp, zin, &ni, &(lgd->j1), &(lgd->j2), &poleval);

        switch (groptions.degre_interp) {
            case CUBIQUE:
            switch (lgd->grtyp[0]) {
                case 'Z':
                case 'E':
                case 'G': {
                    float ay[4];
                    if  (lgd->ay[lgd->j2-1] == 90.0) {
                        ay[0] = lgd->ay[lgd->j2-4];
                        ay[1] = lgd->ay[lgd->j2-3];
                        ay[2] = lgd->ay[lgd->j2-2];
                        ay[3] = lgd->ay[lgd->j2-1];
                        f77name(ez_irgdint_3_wnnc)(vals,gset->zones[AU_NORD].x, gset->zones[AU_NORD].y,&npts,
                                    lgd->ax, ay, temp,&ni, &j1, &j2, &(lgd->extension));
                    } else {
                        ay[0] = lgd->ay[lgd->j2-3];
                        ay[1] = lgd->ay[lgd->j2-2];
                        ay[2] = lgd->ay[lgd->j2-1];
                        ay[3] = 90.0;
                        f77name(ez_irgdint_3_wnnc)(vals,gset->zones[AU_NORD].x,
                                    gset->zones[AU_NORD].y,&npts,
                                    lgd->ax, ay, temp,
                                    &ni, &j1, &j2, &(lgd->extension));
                    }
                    break;
                }

                default:
                    f77name(ez_rgdint_3_wnnc)(vals,gset->zones[AU_NORD].x,
                                gset->zones[AU_NORD].y,&npts,
                                temp,&ni, &j1, &j2, &(lgd->extension));
                    break;
                }
                break;

            case LINEAIRE: {
                float * temp_y = (float *) malloc(npts*sizeof(float));
                for (int32_t i = 0; i < npts; i++) {
                    temp_y[i] = gset->zones[AU_NORD].y[i] - (1.0 * (lgd->j2-3));
                }
                f77name(ez_rgdint_1_w)(vals,gset->zones[AU_NORD].x,temp_y,&npts,temp,&ni, &un, &quatre, &(lgd->extension));
                free(temp_y);
                break;
            }

            case VOISIN: {
                float * temp_y = (float *) malloc(npts*sizeof(float));
                for (int32_t i = 0; i < npts; i++) {
                    temp_y[i] = gset->zones[AU_NORD].y[i] - (1.0 * (lgd->j2-3));
                }
                f77name(ez_rgdint_0)(vals,gset->zones[AU_NORD].x,temp_y,&npts,temp,&ni, &un, &quatre);
                free(temp_y);
                break;
            }
        }

        for (int32_t i = 0; i < gset->zones[AU_NORD].npts; i++) {
            zout[gset->zones[AU_NORD].idx[i]] = vals[i];
        }

        free(vals);
        free(temp);
    }
    return 0;
}
