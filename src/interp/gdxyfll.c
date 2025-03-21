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
#include "f_ezscint.h"


int32_t c_gdxyfll_new(int32_t gdid, float *x, float *y, float *lat, float *lon, int32_t n) {
    float *tmplons;

    int32_t j,ni_in, nj_in;
    int32_t sym=groptions.symmetrie;


    _Grille gr;
    int32_t npts;
    int32_t coordonnee;

    int32_t gdrow_id, gdcol_id;

    c_gdkey2rowcol(gdid,  &gdrow_id,  &gdcol_id);

    gr =  Grille[gdrow_id][gdcol_id];
    npts = n;

    ni_in =  gr.ni;
    nj_in =  gr.nj;

    switch(gr.grtyp[0])
        {
        case 'A':
        case 'B':
        case 'E':
        case 'L':
        case 'N':
        case 'S':
        case 'T':
        case '!':
            tmplons = (float *)malloc(npts * sizeof(float));
            memcpy(tmplons,lon,sizeof(float)*npts);

            f77name(ez_ll2rgd)(x, y,
                lat, tmplons, &npts,
                &ni_in, &nj_in, gr.grtyp,
                &gr.fst.ig[IG1], &gr.fst.ig[IG2], &gr.fst.ig[IG3], &gr.fst.ig[IG4],
                &sym, gr.ay, 1);
            free(tmplons);
            break;

        case '#':
        case 'Z':
        case 'G':
            coordonnee = RELATIF;
            nj_in =  gr.j2;
            f77name(ez_ll2igd)(x, y, lat, lon, &npts,
                &ni_in,&nj_in,gr.grtyp, gr.grref,
                &gr.fst.igref[IG1], &gr.fst.igref[IG2],
                &gr.fst.igref[IG3], &gr.fst.igref[IG4],
                gr.ax, gr.ay,&coordonnee, 1, 1);
            if (gr.grtyp[0] == 'G' && gr.fst.ig[IG1] == 1)
                {
                for  (j=0; j < npts; j++)
                        {
                    y[j] = y[j] - nj_in;
                    }
                }
            if (gr.grtyp[0] == 'G' && gr.fst.ig[IG2] == 1)
        {
                for  (j=0; j < npts; j++)
                        {
                    y[j] = nj_in +1.0 - y[j];
                    }
                }
        break;


    default:
        break;
    }


    return 0;
}


int32_t c_gdxyfll_orig(int32_t gdid, float *x, float *y, float *lat, float *lon, int32_t n) {
    int32_t gdrow_id, gdcol_id;
    c_gdkey2rowcol(gdid,  &gdrow_id,  &gdcol_id);

    _Grille gr = Grille[gdrow_id][gdcol_id];
    int32_t npts = n;

    int32_t ni_in = gr.ni;
    int32_t nj_in = gr.nj;

    float * tmplons;
    int32_t coordonnee = RELATIF;
    switch(gr.grtyp[0]) {
        case 'A':
        case 'B':
        case 'E':
        case 'L':
        case 'N':
        case 'S':
        case 'T':
        case '!':
            tmplons = (float *)malloc(npts * sizeof(float));
            memcpy(tmplons, lon, sizeof(float) * npts);

            int32_t sym = groptions.symmetrie;
            f77name(ez_ll2rgd)(x, y,
                lat, tmplons, &npts,
                &ni_in, &nj_in, gr.grtyp,
                &gr.fst.ig[IG1], &gr.fst.ig[IG2], &gr.fst.ig[IG3], &gr.fst.ig[IG4],
                &sym, gr.ay, 1);
            free(tmplons);
            break;

        case '#':
        case 'Z':
        case 'G':
            nj_in =  gr.j2;
            f77name(ez_ll2igd)(x, y, lat, lon, &npts,
                &ni_in,&nj_in,gr.grtyp, gr.grref,
                &gr.fst.igref[IG1], &gr.fst.igref[IG2],
                &gr.fst.igref[IG3], &gr.fst.igref[IG4],
                gr.ax, gr.ay, &coordonnee, 1, 1);
            if (gr.grtyp[0] == 'G' && gr.fst.ig[IG1] == 1) {
                for (int32_t j = 0; j < npts; j++) {
                    y[j] = y[j] - nj_in;
                }
            }
            break;

    }

    return 0;
}


int32_t c_gdxyfll(int32_t gdid, float *x, float *y, float *lat, float *lon, int32_t n) {
    int32_t icode;

    int32_t gdrow_id, gdcol_id;
    c_gdkey2rowcol(gdid,  &gdrow_id,  &gdcol_id);

    if (Grille[gdrow_id][gdcol_id].nsubgrids > 0 ) {
        int32_t yin_gdid = Grille[gdrow_id][gdcol_id].subgrid[0];
        int32_t yan_gdid = Grille[gdrow_id][gdcol_id].subgrid[1];
        int32_t yin_gdrow_id, yin_gdcol_id;
        int32_t yan_gdrow_id, yan_gdcol_id;
        c_gdkey2rowcol(yin_gdid,  &yin_gdrow_id,  &yin_gdcol_id);
        c_gdkey2rowcol(yan_gdid,  &yan_gdrow_id,  &yan_gdcol_id);
        int32_t maxni = Grille[yin_gdrow_id][yin_gdcol_id].ni;
        int32_t maxnj = Grille[yin_gdrow_id][yin_gdcol_id].nj;
        float * xyin = (float *) malloc(n*sizeof(float));
        float * xyan = (float *) malloc(n*sizeof(float));
        float * yyin = (float *) malloc(n*sizeof(float));
        float * yyan = (float *) malloc(n*sizeof(float));
        icode = c_gdxyfll_orig(yin_gdid, xyin, yyin, lat, lon, n);
        icode = c_gdxyfll_orig(yan_gdid, xyan, yyan, lat, lon, n);
        for (int32_t j = 0; j < n; j++) {
            if (xyin[j] > maxni || xyin[j] < 1 || yyin[j] > maxnj || yyin[j] < 1) {
                // point is no good, take from YAN eventhough it may not be good
                x[j] = xyan[j];
                y[j] = yyan[j] + maxnj;
            } else {
                x[j] = xyin[j];
                y[j] = yyin[j];
            }
            if (xyan[j] >= Grille[yan_gdrow_id][yan_gdcol_id].mymaskgridi0 &&
                xyan[j] <= Grille[yan_gdrow_id][yan_gdcol_id].mymaskgridi1 &&
                yyan[j] >= Grille[yan_gdrow_id][yan_gdcol_id].mymaskgridj0 &&
                yyan[j] <= Grille[yan_gdrow_id][yan_gdcol_id].mymaskgridj1) {
                    x[j] = xyan[j];
                    y[j] = yyan[j] + maxnj;
            }
            if (xyin[j] >= Grille[yin_gdrow_id][yin_gdcol_id].mymaskgridi0 &&
                xyin[j] <= Grille[yin_gdrow_id][yin_gdcol_id].mymaskgridi1 &&
                yyin[j] >= Grille[yin_gdrow_id][yin_gdcol_id].mymaskgridj0 &&
                yyin[j] <= Grille[yin_gdrow_id][yin_gdcol_id].mymaskgridj1) {
                    x[j] = xyin[j];
                    y[j] = yyin[j];
            }
        }
        free(xyin); free(xyan); free(yyin); free(yyan);
    } else {
        icode = c_gdxyfll_new(gdid,x,y,lat,lon,n);
    }
    return icode;
}


int32_t f77name(gdxyfll)(int32_t *gdid, float *x, float *y, float *lat, float *lon, int32_t *n)
{
    return c_gdxyfll(*gdid, x, y, lat, lon, *n);
}
