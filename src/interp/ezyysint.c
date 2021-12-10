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

#include <stdio.h>
#include <stdlib.h>

#include "ez_funcdef.h"

int32_t c_ezyysint(float *zout, float *zin,int32_t gdout,int32_t gdin) {
    int32_t icode;
    int32_t yancount_yin, yincount_yin, yancount_yan, yincount_yan;
    int32_t yin_gdin, yan_gdin, yin_gdout, yan_gdout;
    int32_t yin_gdrow_in, yin_gdcol_in, yin_gdrow_out, yin_gdcol_out;
    int32_t yan_gdrow_in, yan_gdcol_in, yan_gdrow_out, yan_gdcol_out;
    int32_t     gdrow_in,     gdcol_in,     gdrow_out,     gdcol_out;
    int32_t ni, nj;
    int idx_gdin;
    float *yin2yin_zvals, *yan2yin_zvals;
    float *yin2yan_zvals, *yan2yan_zvals;

    _Grille *lgdin, *lgdout;
    /*  need only access to either yin or Yang info for the lat and lon val */

    int32_t yyin = 0;
    int32_t yyout = 0;
    int32_t ierc = 0;
    int32_t ierc1 = 0;
    int32_t ierc2 = 0;

    c_gdkey2rowcol(gdin,  &gdrow_in,  &gdcol_in);
    c_gdkey2rowcol(gdout, &gdrow_out, &gdcol_out);
    idx_gdin = c_find_gdin(gdin, gdout);

    /* setup for input grid */
    if (Grille[gdrow_in][gdcol_in].nsubgrids > 0) {
        yyin=1;
        yin_gdin = Grille[gdrow_in][gdcol_in].subgrid[0];
        yan_gdin = Grille[gdrow_in][gdcol_in].subgrid[1];
        /*fprintf(stderr,"<c_ezyysint> finds subgrid %d and %d in src Yin-Yang grid\n",yin_gdin,yan_gdin);*/
        c_gdkey2rowcol(yin_gdin,  &yin_gdrow_in,  &yin_gdcol_in);
        c_gdkey2rowcol(yan_gdin,  &yan_gdrow_in,  &yan_gdcol_in);
    } else {
        yin_gdin = gdin;
        yin_gdrow_in = gdrow_in;
        yin_gdcol_in = gdcol_in;
    }

    /* setup for output grid */
    if (Grille[gdrow_out][gdcol_out].nsubgrids > 0) {
        yyout=1;
        yin_gdout = Grille[gdrow_out][gdcol_out].subgrid[0];
        yan_gdout = Grille[gdrow_out][gdcol_out].subgrid[1];
        c_gdkey2rowcol(yin_gdout,  &yin_gdrow_out,  &yin_gdcol_out);
        c_gdkey2rowcol(yan_gdout,  &yan_gdrow_out,  &yan_gdcol_out);
    } else {
        yin_gdout = gdout;
        yin_gdrow_out = gdrow_out;
        yin_gdcol_out = gdcol_out;
    }

    lgdin = &(Grille[yin_gdrow_in ][yin_gdcol_in ]);
    lgdout= &(Grille[gdrow_out][gdcol_out]);

    ni = Grille[yin_gdrow_out][yin_gdcol_out].ni;
    nj = Grille[yin_gdrow_out][yin_gdcol_out].nj;

    /* interp input one grid to yygrid - no masking needed*/
    if (yyin == 0 && yyout == 1) {
        icode = c_ezdefset(yin_gdout, gdin);
        ierc1 = c_ezsint_orig(zout, zin);
        icode = c_ezdefset(yan_gdout, gdin);
        ierc2 = c_ezsint_orig(&zout[ni * nj], zin);
        if (ierc1 == 2 || ierc2 == 2) {
            ierc = 2;
        }
        return ierc;
    }

    /* check if one input sub grid is identical to dest sub grid or dest single grid */

    if (yin_gdin == gdout) {
        icode = c_ezdefset(gdout,yin_gdin);
        icode = c_ezsint_orig(zout, zin);
        return icode;
    }
    if (yan_gdin == gdout) {
        icode = c_ezdefset(gdout, yan_gdin);
        icode = c_ezsint_orig(zout, &zin[(lgdin->ni) * (lgdin->nj)]);
        return icode;
    }

    /* User specifies to use 1 subgrid for interpolation ezsetopt(USE_1SUBGRID) */
    /* User must specify the sub grid value in ezsetival(SUBGRIDID) */
    /* This is only appropriate if the destination grid is non yin-yang grid */
    if (groptions.use_1subgrid == 1)  {
        /* User specifies to use 1 subgrid only */
        if (yyout == 1)  {
            /* output is a Yin-Yang grid */
            fprintf(stderr,"<c_ezyysint> cannot use 1 subgrid to interpolate to a Yin-Yang grid  Aborting...\n");
            return -1;
        }
        if (groptions.valeur_1subgrid != yin_gdin && groptions.valeur_1subgrid != yan_gdin) {
            /* chosen subgrid is neither Yin or Yang in source grid */
            fprintf(stderr,"<c_ezyysint> define src subgridid in ezsetival(subgridid)! Aborting...\n");
            return -1;
        }
        if (groptions.valeur_1subgrid == yin_gdin) {
            /*User specifies to use yin input grid */
            icode = c_ezdefset(yin_gdout, groptions.valeur_1subgrid);
            ierc = c_ezsint_orig(zout, zin);
            return ierc;
        }
        if (groptions.valeur_1subgrid == yan_gdin) {
            /* User specifies to use Yang input grid */
            icode = c_ezdefset(yin_gdout, groptions.valeur_1subgrid);
            ierc = c_ezsint_orig(zout, &zin[(lgdin->ni) * (lgdin->nj)]);
            return ierc;
        }

    }

    /* TO USE both Yin and Yang grids in Yin-yang input grid */
    /* Masquer les grilles YY input pour enlever overlap et calculer les X,Y */
    icode = c_ezyy_calcxy(gdout, gdin);

    /* interp yinyang to one grid */
    if (yyin == 1 && yyout == 0) {
        yincount_yin = lgdout->gset[idx_gdin].yincount_yin;
        yancount_yin = lgdout->gset[idx_gdin].yancount_yin;
        yin2yin_zvals = (float *) malloc(yincount_yin * sizeof(float));
        yan2yin_zvals = (float *) malloc(yancount_yin * sizeof(float));

        icode = c_gdxysval(yin_gdin, yin2yin_zvals, zin,
            lgdout->gset[idx_gdin].yin2yin_x, lgdout->gset[idx_gdin].yin2yin_y,
            lgdout->gset[idx_gdin].yincount_yin);

        icode = c_gdxysval(yan_gdin, yan2yin_zvals,
            &zin[(lgdin->ni)*(lgdin->nj)],
            lgdout->gset[idx_gdin].yan2yin_x, lgdout->gset[idx_gdin].yan2yin_y,
            lgdout->gset[idx_gdin].yancount_yin);

        yincount_yin = 0;
        yancount_yin = 0;
        for (int j = 0; j < nj; j++) {
            for (int i = 0; i < ni; i++) {
                int k = (j * ni) + i;
                if (lgdout->gset[idx_gdin].yin_maskout[k] == 1.0) {
                    zout[k] = yan2yin_zvals[yancount_yin];
                    yancount_yin++;
                } else {
                    zout[k] = yin2yin_zvals[yincount_yin];
                    yincount_yin++;
                }
            }
        }
        free(yin2yin_zvals);
        free(yan2yin_zvals);
        return icode;
    }

    /* interp yinyang to yinyang*/
    if (yyout == 1 && yyin == 1) {
        /* interp input YY grid to YY grid */
        yincount_yin = lgdout->gset[idx_gdin].yincount_yin;
        yancount_yin = lgdout->gset[idx_gdin].yancount_yin;
        yincount_yan = lgdout->gset[idx_gdin].yincount_yan;
        yancount_yan = lgdout->gset[idx_gdin].yancount_yan;
        yin2yin_zvals = (float *) malloc(yincount_yin*sizeof(float));
        yan2yin_zvals = (float *) malloc(yancount_yin*sizeof(float));
        yin2yan_zvals = (float *) malloc(yincount_yan*sizeof(float));
        yan2yan_zvals = (float *) malloc(yancount_yan*sizeof(float));

        icode = c_gdxysval(yin_gdin, yin2yin_zvals, zin,
                lgdout->gset[idx_gdin].yin2yin_x, lgdout->gset[idx_gdin].yin2yin_y,
                lgdout->gset[idx_gdin].yincount_yin);
        icode = c_gdxysval(yan_gdin, yan2yin_zvals, &zin[(lgdin->ni)*(lgdin->nj)],
                lgdout->gset[idx_gdin].yan2yin_x, lgdout->gset[idx_gdin].yan2yin_y,
                lgdout->gset[idx_gdin].yancount_yin);

        icode = c_gdxysval(yin_gdin, yin2yan_zvals, zin,
                lgdout->gset[idx_gdin].yin2yan_x, lgdout->gset[idx_gdin].yin2yan_y,
                lgdout->gset[idx_gdin].yincount_yan);
        icode = c_gdxysval(yan_gdin, yan2yan_zvals, &zin[(lgdin->ni)*(lgdin->nj)],
                lgdout->gset[idx_gdin].yan2yan_x, lgdout->gset[idx_gdin].yan2yan_y,
        lgdout->gset[idx_gdin].yancount_yan);

        /* interp input YY grid to Yin grid */
        yincount_yin = 0;
        yancount_yin = 0;
        for(int j = 0; j < nj; j++) {
            for (int i = 0; i < ni; i++) {
                int k = (j * ni) + i;
                if (lgdout->gset[idx_gdin].yin_maskout[k] == 1.0) {
                    zout[k] = yan2yin_zvals[yancount_yin];
                    yancount_yin++;
                } else {
                    zout[k] = yin2yin_zvals[yincount_yin];
                    yincount_yin++;
                }
            }
        }
        /* interp input YY grid to Yang grid */
        yincount_yan = 0;
        yancount_yan = 0;
        for(int j = 0; j < nj; j++) {
            for (int i = 0; i < ni; i++) {
                int k = (j * ni) + i;
                if (lgdout->gset[idx_gdin].yan_maskout[k] == 1.0) {
                    zout[k + ni * nj] = yan2yan_zvals[yancount_yan];
                    yancount_yan++;
                } else {
                    zout[k + ni * nj] = yin2yan_zvals[yincount_yan];
                    yincount_yan++;
                }
            }
        }
        free(yin2yin_zvals);
        free(yan2yin_zvals);
        free(yin2yan_zvals);
        free(yan2yan_zvals);
    }

    return icode;
}
