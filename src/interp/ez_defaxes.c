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


void c_ezdefaxes(int32_t gdid, float *ax, float *ay)
{
    float *temp, dlon;
    int32_t zero, deuxnj;

    _Grille *gr;

    int32_t gdrow_id, gdcol_id;

    c_gdkey2rowcol(gdid,  &gdrow_id,  &gdcol_id);

    gr = &Grille[gdrow_id][gdcol_id];
    switch (gr->grtyp[0]) {
        case '#':
        case 'Z':
            f77name(cigaxg)(&gr->grref,&gr->fst.xgref[XLAT1], &gr->fst.xgref[XLON1], &gr->fst.xgref[XLAT2], &gr->fst.xgref[XLON2],
                &gr->fst.igref[IG1], &gr->fst.igref[IG2], &gr->fst.igref[IG3], &gr->fst.igref[IG4],1);

            Grille[gdrow_id][gdcol_id].ax = (float *) malloc(gr->ni*sizeof(float));
            Grille[gdrow_id][gdcol_id].ay = (float *) malloc(gr->nj*sizeof(float));

            memcpy(Grille[gdrow_id][gdcol_id].ax,ax,gr->ni*sizeof(float));
            memcpy(Grille[gdrow_id][gdcol_id].ay,ay,gr->nj*sizeof(float));
            ez_calcxpncof(gdid);
            ez_calcntncof(gdid);
            break;

        case 'Y':
            Grille[gdrow_id][gdcol_id].ax = (float *) malloc(gr->ni*gr->nj*sizeof(float));
            Grille[gdrow_id][gdcol_id].ay = (float *) malloc(gr->ni*gr->nj*sizeof(float));
            memcpy(Grille[gdrow_id][gdcol_id].ax,ax,gr->ni*gr->nj*sizeof(float));
            memcpy(Grille[gdrow_id][gdcol_id].ay,ay,gr->ni*gr->nj*sizeof(float));

            ez_calcxpncof(gdid);
            break;

        case 'G':
            gr->grref[0] = 'L';
            gr->fst.xgref[SWLAT] = 0.0;
            gr->fst.xgref[SWLON] = 0.0;
            gr->fst.xgref[DLAT] = 1.0;
            gr->fst.xgref[DLON] = 1.0;
            f77name(cxgaig)(&gr->grref,&gr->fst.igref[IG1], &gr->fst.igref[IG2], &gr->fst.igref[IG3], &gr->fst.igref[IG4],
                &gr->fst.xgref[SWLAT], &gr->fst.xgref[SWLON], &gr->fst.xgref[DLAT], &gr->fst.xgref[DLON],1);

            Grille[gdrow_id][gdcol_id].ax = (float *) malloc(gr->ni*sizeof(float));
            dlon = 360. / (float) gr->ni;
            for (int32_t i = 0; i < gr->ni; i++) {
                Grille[gdrow_id][gdcol_id].ax[i] = (float)i * dlon;
            }

            zero = 0;
            ez_calcxpncof(gdid);

            switch (Grille[gdrow_id][gdcol_id].fst.ig[IG1]) {
                case GLOBAL:
                    Grille[gdrow_id][gdcol_id].ay = (float *) malloc(gr->nj*sizeof(float));
                    temp    = (float *) malloc(gr->nj*sizeof(float));
                    f77name(ez_glat)(Grille[gdrow_id][gdcol_id].ay,temp,&gr->nj,&zero);
                    free(temp);
                    break;

                case NORD:
                case SUD:
                    deuxnj = 2 * gr->nj;
                    Grille[gdrow_id][gdcol_id].ay = (float *) malloc(deuxnj*sizeof(float));
                    temp    = (float *) malloc(deuxnj*sizeof(float));
                    f77name(ez_glat)(Grille[gdrow_id][gdcol_id].ay,temp,&deuxnj,&zero);
                    free(temp);
                    break;
                }


            ez_calcntncof(gdid);
            Grille[gdrow_id][gdcol_id].flags |= AX;
        break;

        default:
            ez_calcxpncof(gdid);
            break;
    }

    Grille[gdrow_id][gdcol_id].flags |= AX;
}
