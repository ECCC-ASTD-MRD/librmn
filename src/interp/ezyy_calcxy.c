/*
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


int32_t c_ezyy_calcxy(int32_t gdout,int32_t gdin)
{
    /*  need only access to either yin or Yang info for the lat and lon val */

    int32_t yyout = 0;
    int32_t gdrow_in, gdcol_in;
    c_gdkey2rowcol(gdin, &gdrow_in, &gdcol_in);
    int32_t gdrow_out, gdcol_out;
    c_gdkey2rowcol(gdout, &gdrow_out, &gdcol_out);
    int idx_gdin = c_find_gdin(gdin, gdout);
    /* Mettre du code au cas ou gdx_gdin == -1 */

    if (Grille[gdrow_out][gdcol_out].gset[idx_gdin].yyflags & XXX) {
        return 0;
    }

    /* Dans un premier temps on calcule la position x-y de tous les points sur la grille */

    /* To be in this routine, input source grid should be Yin-Yang */
    int32_t yin_gdin = Grille[gdrow_in][gdcol_in].subgrid[0];
    int32_t yan_gdin = Grille[gdrow_in][gdcol_in].subgrid[1];
    int32_t yin_gdrow_in, yin_gdcol_in;
    c_gdkey2rowcol(yin_gdin, &yin_gdrow_in, &yin_gdcol_in);
    int32_t yan_gdrow_in, yan_gdcol_in;
    c_gdkey2rowcol(yan_gdin, &yan_gdrow_in, &yan_gdcol_in);

    /* Check what the destination grid is */
    int32_t ni, nj;
    int32_t yin_gdout, yan_gdout;
    int32_t yin_gdrow_out, yin_gdcol_out;
    int32_t yan_gdrow_out, yan_gdcol_out;
    if (Grille[gdrow_out][gdcol_out].nsubgrids > 0) {
        yyout = 1;
        yin_gdout = Grille[gdrow_out][gdcol_out].subgrid[0];
        yan_gdout = Grille[gdrow_out][gdcol_out].subgrid[1];
        c_gdkey2rowcol(yin_gdout, &yin_gdrow_out, &yin_gdcol_out);
        c_gdkey2rowcol(yan_gdout, &yan_gdrow_out, &yan_gdcol_out);
        ni = Grille[yin_gdrow_out][yin_gdcol_out].ni;
        nj = Grille[yin_gdrow_out][yin_gdcol_out].nj;
    } else {
        yin_gdout = gdout;
        yin_gdrow_out = gdrow_out;
        yin_gdcol_out = gdcol_out;
        ni = Grille[gdrow_out][gdcol_out].ni;
        nj = Grille[gdrow_out][gdcol_out].nj;
    }

    &(Grille[gdrow_in ][gdcol_in ]);
    _Grille * lgdout = &(Grille[gdrow_out][gdcol_out]);

    /* Masquer les grilles YY input pour enlever overlap si OUI */
    float * yin2yin_lat = (float *) malloc(ni*nj*sizeof(float));
    float * yin2yin_lon = (float *) malloc(ni*nj*sizeof(float));
    float * yan2yin_lat = (float *) malloc(ni*nj*sizeof(float));
    float * yan2yin_lon = (float *) malloc(ni*nj*sizeof(float));
    float * yin2yan_lat = (float *) malloc(ni*nj*sizeof(float));
    float * yin2yan_lon = (float *) malloc(ni*nj*sizeof(float));
    float * yan2yan_lat = (float *) malloc(ni*nj*sizeof(float));
    float * yan2yan_lon = (float *) malloc(ni*nj*sizeof(float));
    int32_t yancount_yin = 0;
    int32_t yincount_yin = 0;
    int32_t icode;
    if (yyout == 0)
        /* destination grid is one grid */
        {
        /* create mask with Yin as a priority choice and store x,y,lat,lon pos */
        lgdout->gset[idx_gdin].yin_maskout = (float *) malloc(ni*nj*sizeof(float));
        lgdout->gset[idx_gdin].yinlat = (float *) malloc(ni*nj*sizeof(float));
        lgdout->gset[idx_gdin].yinlon = (float *) malloc(ni*nj*sizeof(float));
        icode = c_gdll(yin_gdout,lgdout->gset[idx_gdin].yinlat,lgdout->gset[idx_gdin].yinlon);
        icode = c_ezyymint(yin_gdout,yin_gdin,ni,nj,lgdout->gset[idx_gdin].yin_maskout,lgdout->gset[idx_gdin].yinlat,lgdout->gset[idx_gdin].yinlon,yin2yin_lat,yin2yin_lon,&yincount_yin,yan2yin_lat,yan2yin_lon,&yancount_yin);
        /* store the lats and lons */
        lgdout->gset[idx_gdin].yincount_yin = yincount_yin;
        lgdout->gset[idx_gdin].yancount_yin = yancount_yin;
        lgdout->gset[idx_gdin].yin2yin_lat = (float *) malloc(yincount_yin*sizeof(float));
        lgdout->gset[idx_gdin].yin2yin_lon = (float *) malloc(yincount_yin*sizeof(float));
        lgdout->gset[idx_gdin].yan2yin_lat = (float *) malloc(yancount_yin*sizeof(float));
        lgdout->gset[idx_gdin].yan2yin_lon = (float *) malloc(yancount_yin*sizeof(float));
        memcpy(lgdout->gset[idx_gdin].yin2yin_lat,yin2yin_lat,yincount_yin*sizeof(float));
        memcpy(lgdout->gset[idx_gdin].yin2yin_lon,yin2yin_lon,yincount_yin*sizeof(float));
        memcpy(lgdout->gset[idx_gdin].yan2yin_lat,yan2yin_lat,yancount_yin*sizeof(float));
        memcpy(lgdout->gset[idx_gdin].yan2yin_lon,yan2yin_lon,yancount_yin*sizeof(float));

        /* store the Xs and Ys */
        lgdout->gset[idx_gdin].yin2yin_x = (float *) malloc(yincount_yin*sizeof(float));
        lgdout->gset[idx_gdin].yin2yin_y = (float *) malloc(yincount_yin*sizeof(float));
        lgdout->gset[idx_gdin].yan2yin_x = (float *) malloc(yancount_yin*sizeof(float));
        lgdout->gset[idx_gdin].yan2yin_y = (float *) malloc(yancount_yin*sizeof(float));
        icode = c_gdxyfll_orig(yin_gdin,lgdout->gset[idx_gdin].yin2yin_x,lgdout->gset[idx_gdin].yin2yin_y,yin2yin_lat,yin2yin_lon,yincount_yin);
        icode = c_gdxyfll_orig(yan_gdin,lgdout->gset[idx_gdin].yan2yin_x,lgdout->gset[idx_gdin].yan2yin_y,yan2yin_lat,yan2yin_lon,yancount_yin);
        }

    if (yyout == 1) {
        // destination grid is a U grid
        // create mask (Yin priority)with src Yin,src Yang onto dest Yin and store x,y pos
        lgdout->gset[idx_gdin].yin_maskout = (float *) malloc(ni*nj*sizeof(float));
        lgdout->gset[idx_gdin].yinlat = (float *) malloc(ni*nj*sizeof(float));
        lgdout->gset[idx_gdin].yinlon = (float *) malloc(ni*nj*sizeof(float));
        icode = c_gdll(yin_gdout,lgdout->gset[idx_gdin].yinlat,lgdout->gset[idx_gdin].yinlon);
        icode = c_ezyymint(yin_gdout,yin_gdin,ni,nj,lgdout->gset[idx_gdin].yin_maskout,lgdout->gset[idx_gdin].yinlat,lgdout->gset[idx_gdin].yinlon,yin2yin_lat,yin2yin_lon,&yincount_yin,yan2yin_lat,yan2yin_lon,&yancount_yin);
        lgdout->gset[idx_gdin].yincount_yin = yincount_yin;
        lgdout->gset[idx_gdin].yancount_yin = yancount_yin;
        lgdout->gset[idx_gdin].yin2yin_lat = (float *) malloc(yincount_yin*sizeof(float));
        lgdout->gset[idx_gdin].yin2yin_lon = (float *) malloc(yincount_yin*sizeof(float));
        lgdout->gset[idx_gdin].yan2yin_lat = (float *) malloc(yancount_yin*sizeof(float));
        lgdout->gset[idx_gdin].yan2yin_lon = (float *) malloc(yancount_yin*sizeof(float));
        memcpy(lgdout->gset[idx_gdin].yin2yin_lat,yin2yin_lat,yincount_yin*sizeof(float));
        memcpy(lgdout->gset[idx_gdin].yin2yin_lon,yin2yin_lon,yincount_yin*sizeof(float));
        memcpy(lgdout->gset[idx_gdin].yan2yin_lat,yan2yin_lat,yancount_yin*sizeof(float));
        memcpy(lgdout->gset[idx_gdin].yan2yin_lon,yan2yin_lon,yancount_yin*sizeof(float));
        lgdout->gset[idx_gdin].yin2yin_x = (float *) malloc(yincount_yin*sizeof(float));
        lgdout->gset[idx_gdin].yin2yin_y = (float *) malloc(yincount_yin*sizeof(float));
        lgdout->gset[idx_gdin].yan2yin_x = (float *) malloc(yancount_yin*sizeof(float));
        lgdout->gset[idx_gdin].yan2yin_y = (float *) malloc(yancount_yin*sizeof(float));
        icode = c_gdxyfll_orig(yin_gdin,lgdout->gset[idx_gdin].yin2yin_x,lgdout->gset[idx_gdin].yin2yin_y,yin2yin_lat,yin2yin_lon,yincount_yin);
        icode = c_gdxyfll_orig(yan_gdin,lgdout->gset[idx_gdin].yan2yin_x,lgdout->gset[idx_gdin].yan2yin_y,yan2yin_lat,yan2yin_lon,yancount_yin);

        // create mask (Yin priority)with src Yin,src Yang onto dest Yang and store x,y pos
        lgdout->gset[idx_gdin].yan_maskout = (float *) malloc(ni*nj*sizeof(float));
        lgdout->gset[idx_gdin].yanlat = (float *) malloc(ni*nj*sizeof(float));
        lgdout->gset[idx_gdin].yanlon = (float *) malloc(ni*nj*sizeof(float));
        icode = c_gdll(yan_gdout,lgdout->gset[idx_gdin].yanlat,lgdout->gset[idx_gdin].yanlon);
        int32_t yancount_yan, yincount_yan;
        icode = c_ezyymint(yan_gdout,yin_gdin,ni,nj,lgdout->gset[idx_gdin].yan_maskout,lgdout->gset[idx_gdin].yanlat,lgdout->gset[idx_gdin].yanlon,yin2yan_lat,yin2yan_lon,&yincount_yan,yan2yan_lat,yan2yan_lon,&yancount_yan);
        lgdout->gset[idx_gdin].yincount_yan = yincount_yan;
        lgdout->gset[idx_gdin].yancount_yan = yancount_yan;
        lgdout->gset[idx_gdin].yin2yan_lat = (float *) malloc(yincount_yan*sizeof(float));
        lgdout->gset[idx_gdin].yin2yan_lon = (float *) malloc(yincount_yan*sizeof(float));
        lgdout->gset[idx_gdin].yan2yan_lat = (float *) malloc(yancount_yan*sizeof(float));
        lgdout->gset[idx_gdin].yan2yan_lon = (float *) malloc(yancount_yan*sizeof(float));
        memcpy(lgdout->gset[idx_gdin].yin2yan_lat,yin2yan_lat,yincount_yan*sizeof(float));
        memcpy(lgdout->gset[idx_gdin].yin2yan_lon,yin2yan_lon,yincount_yan*sizeof(float));
        memcpy(lgdout->gset[idx_gdin].yan2yan_lat,yan2yan_lat,yancount_yan*sizeof(float));
        memcpy(lgdout->gset[idx_gdin].yan2yan_lon,yan2yan_lon,yancount_yan*sizeof(float));
        lgdout->gset[idx_gdin].yin2yan_x = (float *) malloc(yincount_yan*sizeof(float));
        lgdout->gset[idx_gdin].yin2yan_y = (float *) malloc(yincount_yan*sizeof(float));
        lgdout->gset[idx_gdin].yan2yan_x = (float *) malloc(yancount_yan*sizeof(float));
        lgdout->gset[idx_gdin].yan2yan_y = (float *) malloc(yancount_yan*sizeof(float));
        icode = c_gdxyfll_orig(yin_gdin,lgdout->gset[idx_gdin].yin2yan_x,lgdout->gset[idx_gdin].yin2yan_y,yin2yan_lat,yin2yan_lon,yincount_yan);
        icode = c_gdxyfll_orig(yan_gdin,lgdout->gset[idx_gdin].yan2yan_x,lgdout->gset[idx_gdin].yan2yan_y,yan2yan_lat,yan2yan_lon,yancount_yan);
    }

    free(yin2yin_lat);
    free(yin2yin_lon);
    free(yan2yin_lat);
    free(yan2yin_lon);
    free(yin2yan_lat);
    free(yin2yan_lon);
    free(yan2yan_lat);
    free(yan2yan_lon);
    Grille[gdrow_out][gdcol_out].gset[idx_gdin].yyflags |= XXX;
    return icode;
}
