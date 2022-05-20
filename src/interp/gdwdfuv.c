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


int32_t c_gdwdfuv_orig(int32_t gdid, float *spd_out, float *wd_out, float *uuin, float *vvin,
              float *latin, float *lonin, int32_t npts)
{
    int32_t gdrow_id, gdcol_id;
    c_gdkey2rowcol(gdid,  &gdrow_id,  &gdcol_id);

   int32_t ni = npts;
   int32_t nj = 1;

    memcpy(spd_out, uuin, npts*sizeof(float));
    memcpy(wd_out, vvin, npts*sizeof(float));

    float * lat_rot;
    float * lon_rot;
    switch (Grille[gdrow_id][gdcol_id].grtyp[0]) {
        case 'E':
            lat_rot = (float *)(malloc(npts*sizeof(float)));
            lon_rot = (float *)(malloc(npts*sizeof(float)));
            f77name(ez_gfxyfll)(lonin, latin, lon_rot,lat_rot, &ni,
                                &Grille[gdrow_id][gdcol_id].fst.xg[XLAT1],&Grille[gdrow_id][gdcol_id].fst.xg[XLON1],
                                &Grille[gdrow_id][gdcol_id].fst.xg[XLAT2],&Grille[gdrow_id][gdcol_id].fst.xg[XLON2]);

            c_ezllwfgfw(spd_out,wd_out, latin,lonin, lat_rot,lon_rot,
                        &ni,&nj,Grille[gdrow_id][gdcol_id].grtyp,
                        &Grille[gdrow_id][gdcol_id].fst.ig[IG1],&Grille[gdrow_id][gdcol_id].fst.ig[IG2],
                        &Grille[gdrow_id][gdcol_id].fst.ig[IG3],&Grille[gdrow_id][gdcol_id].fst.ig[IG4]);
            free(lat_rot);
            free(lon_rot);
            return 0;
            break;

        case '#':
        case 'Y':
        case 'Z':
            switch (Grille[gdrow_id][gdcol_id].grref[0]) {
                case 'E':
                    lat_rot = (float *)(malloc(npts*sizeof(float)));
                    lon_rot = (float *)(malloc(npts*sizeof(float)));
                    f77name(ez_gfxyfll)(lonin,latin,lon_rot, lat_rot, &ni,
                        &Grille[gdrow_id][gdcol_id].fst.xgref[XLAT1],&Grille[gdrow_id][gdcol_id].fst.xgref[XLON1],
                        &Grille[gdrow_id][gdcol_id].fst.xgref[XLAT2],&Grille[gdrow_id][gdcol_id].fst.xgref[XLON2]);

                    c_ezllwfgfw(spd_out,wd_out,latin,lonin,lat_rot, lon_rot,
                    &ni,&nj,Grille[gdrow_id][gdcol_id].grref,
                    &Grille[gdrow_id][gdcol_id].fst.igref[IG1],&Grille[gdrow_id][gdcol_id].fst.igref[IG2],
                    &Grille[gdrow_id][gdcol_id].fst.igref[IG3],&Grille[gdrow_id][gdcol_id].fst.igref[IG4]);
                    free(lat_rot);
                    free(lon_rot);
                    return 0;
                    break;

                default:
                    f77name(ez_llwfgdw)(spd_out,wd_out,lonin,
                                &ni,&nj,
                                &Grille[gdrow_id][gdcol_id].grref,
                                &Grille[gdrow_id][gdcol_id].fst.igref[IG1],&Grille[gdrow_id][gdcol_id].fst.igref[IG2],
                                &Grille[gdrow_id][gdcol_id].fst.igref[IG3],&Grille[gdrow_id][gdcol_id].fst.igref[IG4]);
                    break;
            }
            break;

            default:
                f77name(ez_llwfgdw)(spd_out,wd_out,lonin,
                        &ni,&nj,
                        &Grille[gdrow_id][gdcol_id].grtyp,
                        &Grille[gdrow_id][gdcol_id].fst.ig[IG1],&Grille[gdrow_id][gdcol_id].fst.ig[IG2],
                        &Grille[gdrow_id][gdcol_id].fst.ig[IG3],&Grille[gdrow_id][gdcol_id].fst.ig[IG4]);
                break;
    }

    return 0;
}


int32_t c_gdwdfuv(int32_t gdid, float *spd_out, float *wd_out, float *uuin, float *vvin,
              float *latin, float *lonin, int32_t npts)
{
    int32_t gdrow_id, gdcol_id;
    c_gdkey2rowcol(gdid,  &gdrow_id,  &gdcol_id);
    if (Grille[gdrow_id][gdcol_id].nsubgrids > 0 ) {
        fprintf(stderr, "<gdwdfuv>: This operation is not supported for 'U' grids\n");
        return -1;
    } else {
        return c_gdwdfuv_orig(gdid, spd_out, wd_out, uuin, vvin, latin, lonin, npts);
    }
}


int32_t f77name(gdwdfuv)(int32_t *gdid, float *spd_out, float *wd_out, float *uuin, float *vvin,
                     float *latin, float *lonin, int32_t *npts)
{
    return c_gdwdfuv(*gdid, spd_out, wd_out, uuin, vvin,latin, lonin, *npts);
}
