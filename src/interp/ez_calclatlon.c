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

#include <rmn/ezscint.h>
#include "ez_funcdef.h"
#include "base/base.h"
#include "interp/f_ezscint.h"


int32_t ez_calclatlon(int32_t gdid) {
    int32_t gdrow, gdcol;
    c_gdkey2rowcol(gdid, &gdrow, &gdcol);

    const int32_t ni = Grille[gdrow][gdcol].ni;
    const int32_t nj = Grille[gdrow][gdcol].nj;

    if (!(Grille[gdrow][gdcol].flags & LAT)) {
        const int32_t npts = ni * nj;

        Grille[gdrow][gdcol].lat = (float *) malloc(ni * nj * sizeof(float));
        Grille[gdrow][gdcol].lon = (float *) malloc(ni * nj * sizeof(float));

        switch(Grille[gdrow][gdcol].grtyp[0]) {
            case 'A':
            case 'B':
                f77name(grll)(Grille[gdrow][gdcol].lat, Grille[gdrow][gdcol].lon, &ni, &nj,
                    &Grille[gdrow][gdcol].fst.xg[SWLAT], &Grille[gdrow][gdcol].fst.xg[SWLON], &Grille[gdrow][gdcol].fst.xg[DLAT], &Grille[gdrow][gdcol].fst.xg[DLON]);
                break;

            case 'E': {
                float dlon = 360.0 / (ni - 1);
                float dlat = 180.0 / (nj);
                float xlon00 = 0.0;
                float xlat00 = -90. + 0.5 * dlat;

                f77name(grll)(Grille[gdrow][gdcol].lat, Grille[gdrow][gdcol].lon, &ni, &nj, &xlat00, &xlon00, &dlat, &dlon);

                f77name(cigaxg)(Grille[gdrow][gdcol].grtyp, &Grille[gdrow][gdcol].fst.xg[XLAT1], &Grille[gdrow][gdcol].fst.xg[XLON1],
                    &Grille[gdrow][gdcol].fst.xg[XLAT2], &Grille[gdrow][gdcol].fst.xg[XLON2],
                    &Grille[gdrow][gdcol].fst.ig[IG1],  &Grille[gdrow][gdcol].fst.ig[IG2], &Grille[gdrow][gdcol].fst.ig[IG3], &Grille[gdrow][gdcol].fst.ig[IG4], 1);
                float * const latp = (float *) malloc(ni * nj * sizeof(float));
                float * const lonp = (float *) malloc(ni * nj * sizeof(float));
                f77name(ez_gfllfxy)(lonp, latp, Grille[gdrow][gdcol].lon, Grille[gdrow][gdcol].lat, &npts,
                        &Grille[gdrow][gdcol].fst.xg[XLAT1], &Grille[gdrow][gdcol].fst.xg[XLON1], &Grille[gdrow][gdcol].fst.xg[XLAT2],
                        &Grille[gdrow][gdcol].fst.xg[XLON2]);
                memcpy(Grille[gdrow][gdcol].lat, latp, npts * sizeof(float));
                memcpy(Grille[gdrow][gdcol].lon, lonp, npts * sizeof(float));
                free(latp);
                free(lonp);
                break;
            }

            case 'L':
                f77name(grll)(Grille[gdrow][gdcol].lat, Grille[gdrow][gdcol].lon, &ni, &nj,
                    &Grille[gdrow][gdcol].fst.xg[SWLAT], &Grille[gdrow][gdcol].fst.xg[SWLON],
                    &Grille[gdrow][gdcol].fst.xg[DLAT], &Grille[gdrow][gdcol].fst.xg[DLON]);
                break;

            case 'N':
            case 'S': {
                int32_t hemisphere = Grille[gdrow][gdcol].grtyp[0] == 'N' ? 1 : 2;
                f77name(grps)(Grille[gdrow][gdcol].lat, Grille[gdrow][gdcol].lon, &ni, &nj,
                    &Grille[gdrow][gdcol].fst.xg[PI], &Grille[gdrow][gdcol].fst.xg[PJ],
                    &Grille[gdrow][gdcol].fst.xg[D60], &Grille[gdrow][gdcol].fst.xg[DGRW], &hemisphere);
                break;
            }

            case 'T': {
                float * const latp = (float *) malloc(npts * sizeof(float));
                float * const lonp = (float *) malloc(npts * sizeof(float));
                float * const xp = (float *) malloc(npts * sizeof(float));
                float * const yp = (float *) malloc(npts * sizeof(float));
                for (int32_t j = 0; j < nj; j++) {
                    for (int32_t i = 0; i < ni; i++) {
                        int32_t k = j * ni + i;
                        yp[k] = 1.0 * (j + 1);
                        xp[k] = 1.0 * (i + 1);
                    }
                }

                f77name(ez_vtllfxy)(latp, lonp, xp, yp,
                    &Grille[gdrow][gdcol].fst.xg[CLAT], &Grille[gdrow][gdcol].fst.xg[CLON],
                    &Grille[gdrow][gdcol].fst.xg[TD60], &Grille[gdrow][gdcol].fst.xg[TDGRW],
                    &ni, &nj, &npts);

                memcpy(Grille[gdrow][gdcol].lon, lonp, ni * nj * sizeof(float));
                memcpy(Grille[gdrow][gdcol].lat, latp, ni * nj * sizeof(float));
                free(lonp);
                free(latp);
                free(xp);
                free(yp);
                break;
            }

            case 'Y':
                switch (Grille[gdrow][gdcol].grref[0]) {
                    case 'N':
                    case 'S':
                        fprintf(stderr, "<ez_calclatlon> Operation not supported - Y grid on PS Grid\n");
                        return -1;
                        break;

                    case 'L':
                    case 'O':
                        memcpy(Grille[gdrow][gdcol].lon, Grille[gdrow][gdcol].ax, Grille[gdrow][gdcol].ni*Grille[gdrow][gdcol].nj*sizeof(float));
                        memcpy(Grille[gdrow][gdcol].lat, Grille[gdrow][gdcol].ay, Grille[gdrow][gdcol].ni*Grille[gdrow][gdcol].nj*sizeof(float));
                        for (int32_t i = 0; i < Grille[gdrow][gdcol].ni*Grille[gdrow][gdcol].nj; i++) {
                            if (Grille[gdrow][gdcol].lon[i] < 0.0) {
                                Grille[gdrow][gdcol].lon[i] = Grille[gdrow][gdcol].ax[i] + 360.0;
                            }
                        }
                        break;

                    case 'E':
                        fprintf(stderr, "<ez_calclatlon> Operation not supported - Y grid on E Grid\n");
                        return -1;
                        break;
                }
                break;

            case '#':
            case 'Z':
            case 'G':
                for (int32_t j = 0; j < nj; j++) {
                    for (int32_t i = 0; i < ni; i++) {
                        Grille[gdrow][gdcol].lat[C_TO_FTN(i, j, ni)] = Grille[gdrow][gdcol].ay[j];
                        Grille[gdrow][gdcol].lon[C_TO_FTN(i, j, ni)] = Grille[gdrow][gdcol].ax[i];
                    }
                }

                if (Grille[gdrow][gdcol].grtyp[0] == 'G' && Grille[gdrow][gdcol].fst.ig[IG1] == NORD) {
                    for (int32_t j = 0; j < nj; j++) {
                        for (int32_t i = 0; i < ni; i++) {
                            Grille[gdrow][gdcol].lat[C_TO_FTN(i, j, ni)] = Grille[gdrow][gdcol].ay[j+nj];
                        }
                    }
                }

                switch (Grille[gdrow][gdcol].grref[0]) {
                    case 'N':
                    case 'S': {
                        float * const latp = (float *) malloc(ni * nj * sizeof(float));
                        float * const lonp = (float *) malloc(ni * nj * sizeof(float));
                        f77name(ez_vllfxy)(latp, lonp,
                                Grille[gdrow][gdcol].lon, Grille[gdrow][gdcol].lat, &ni, &nj,
                                &Grille[gdrow][gdcol].fst.xgref[D60], &Grille[gdrow][gdcol].fst.xgref[DGRW],
                                &Grille[gdrow][gdcol].fst.xgref[PI], &Grille[gdrow][gdcol].fst.xgref[PJ], &Grille[gdrow][gdcol].fst.hemisphere);

                        for (int32_t i = 0; i < ni * nj; i++) {
                            if (lonp[i] < 0.0) lonp[i] += 360.0;
                        }

                        memcpy(Grille[gdrow][gdcol].lon, lonp, ni*nj*sizeof(float));
                        memcpy(Grille[gdrow][gdcol].lat, latp, ni*nj*sizeof(float));
                        free(lonp);
                        free(latp);
                        break;
                    }

                    case 'L':
                        for (int32_t j = 0; j < nj; j++) {
                            for (int32_t i = 0; i < ni; i++) {
                                Grille[gdrow][gdcol].lat[C_TO_FTN(i, j, ni)] += 1.0;
                                Grille[gdrow][gdcol].lon[C_TO_FTN(i, j, ni)] += 1.0;
                            }
                        }
                        c_llfgr(Grille[gdrow][gdcol].lat, Grille[gdrow][gdcol].lon, Grille[gdrow][gdcol].lon, Grille[gdrow][gdcol].lat, ni*nj,
                            Grille[gdrow][gdcol].fst.xgref[SWLAT], Grille[gdrow][gdcol].fst.xgref[SWLON], Grille[gdrow][gdcol].fst.xgref[DLAT], Grille[gdrow][gdcol].fst.xgref[DLON]);
                        break;

                    case 'E': {
                        float * const latp = (float *) malloc(ni * nj * sizeof(float));
                        float * const lonp = (float *) malloc(ni * nj * sizeof(float));
                        f77name(ez_gfllfxy)(lonp, latp, Grille[gdrow][gdcol].lon, Grille[gdrow][gdcol].lat, &npts,
                            &Grille[gdrow][gdcol].fst.xgref[XLAT1], &Grille[gdrow][gdcol].fst.xgref[XLON1], &Grille[gdrow][gdcol].fst.xgref[XLAT2], &Grille[gdrow][gdcol].fst.xgref[XLON2]);
                        memcpy(Grille[gdrow][gdcol].lon, lonp, ni * nj * sizeof(float));
                        memcpy(Grille[gdrow][gdcol].lat, latp, ni * nj * sizeof(float));
                        free(lonp);
                        free(latp);
                        break;
                    }
                }
                break;

            case '!': {
                float * const x = (float *) malloc(ni * nj * sizeof(float));
                float * const y = (float *) malloc(ni * nj * sizeof(float));
                for (int32_t j = 0; j < nj; j++) {
                    for (int32_t i = 0; i < ni; i++) {
                        x[C_TO_FTN(i, j, ni)] = (float) (i+1.0);
                        y[C_TO_FTN(i, j, ni)] = (float) (j+1.0);
                    }
                }
                f77name(ez_llflamb)(Grille[gdrow][gdcol].lat, Grille[gdrow][gdcol].lon, x, y, &npts,
                    Grille[gdrow][gdcol].grtyp, &Grille[gdrow][gdcol].fst.ig[IG1], &Grille[gdrow][gdcol].fst.ig[IG2],
                    &Grille[gdrow][gdcol].fst.ig[IG3], &Grille[gdrow][gdcol].fst.ig[IG4], 1);
                for (int32_t i = 0; i < npts; i++) {
                    if (Grille[gdrow][gdcol].lon[i] < 0.0) {
                        Grille[gdrow][gdcol].lon[i] += 360.0;
                    }
                }
                break;
            }
        }

        switch(Grille[gdrow][gdcol].grtyp[0]) {
            case 'G':
            case 'B':
            case 'A':
                if (Grille[gdrow][gdcol].fst.ig[IG2] == 1) {
                    f77name(permut)(Grille[gdrow][gdcol].lat, &Grille[gdrow][gdcol].ni, &Grille[gdrow][gdcol].nj);
                }
                break;

            default:
                break;
        }

        Grille[gdrow][gdcol].flags |= LAT;
    }

    if (groptions.verbose == 2) {
        fprintf(stderr, "gdid: %d\n", gdid);
        for (int32_t j = 0; j < nj; j++) {
            for (int32_t i = 0; i < ni; i++) {
                fprintf(stderr, "%d %d %f %f\n", i, j, Grille[gdrow][gdcol].lat[i], Grille[gdrow][gdcol].lon[i]);
            }
        }
    }
    return 0;
}
