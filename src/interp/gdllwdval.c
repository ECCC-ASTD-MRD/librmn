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


int32_t f77name(gdllwdval)(int32_t *gdid, float *uuout, float *vvout, float *uuin, float *vvin,
                      float *lat, float *lon, int32_t *n)
{
    int32_t icode;

    icode = c_gdllwdval(*gdid, uuout,vvout, uuin, vvin, lat, lon, *n);
    return icode;
}

int32_t c_gdllwdval(int32_t gdid, float *uuout, float *vvout, float *uuin, float *vvin,
               float *lat, float *lon, int32_t n)
{
    int32_t ier, yin_gdid, yan_gdid;
    int32_t gdrow_id, gdcol_id,yin_gdrow_id,yin_gdcol_id;
    float *x, *y;
    float *uuyin, *vvyin, *uuyan, *vvyan;

    c_gdkey2rowcol(gdid,  &gdrow_id,  &gdcol_id);
    if (Grille[gdrow_id][gdcol_id].nsubgrids > 0) {
        x = (float *) malloc(n * sizeof(float));
        y = (float *) malloc(n * sizeof(float));
        uuyin = (float *) malloc(n*sizeof(float));
        vvyin = (float *) malloc(n*sizeof(float));
        uuyan = (float *) malloc(n*sizeof(float));
        vvyan = (float *) malloc(n*sizeof(float));
        ier = c_gdxyfll(gdid, x, y, lat, lon, n);
        ier = c_gdxyvval(gdid, uuout, vvout, uuin, vvin, x, y, n);
        yin_gdid=Grille[gdrow_id][gdcol_id].subgrid[0];
        yan_gdid=Grille[gdrow_id][gdcol_id].subgrid[1];
        ier = c_gdwdfuv_orig(yin_gdid,uuyin,vvyin,uuout,vvout,lat,lon,n);
        ier = c_gdwdfuv_orig(yan_gdid,uuyan,vvyan,uuout,vvout,lat,lon,n);
        c_gdkey2rowcol(yin_gdid,  &yin_gdrow_id,  &yin_gdcol_id);
        for (int32_t j = 0; j < n; j++) {
            if (y[j] > Grille[yin_gdrow_id][yin_gdcol_id].nj) {
                uuout[j]=uuyan[j];
                vvout[j]=vvyan[j];
            } else {
                uuout[j]=uuyin[j];
                vvout[j]=vvyin[j];
            }
        }
        free(uuyin); free(vvyin);
        free(uuyan); free(vvyan);

    } else {
        ier = c_gdllvval(gdid, uuout, vvout, uuin, vvin, lat, lon, n);
        ier = c_gdwdfuv(gdid, uuout, vvout, uuout, vvout, lat, lon, n);
    }
    return 0;
}
