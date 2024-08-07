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
#include "f_ezscint.h"



int32_t ez_corrvec_aunord(float *uuout, float *vvout, float *uuin, float *vvin, int32_t gdin, int32_t gdout)
{
    float *polar_uu_in, *polar_vv_in, *corr_uus, *corr_vvs, *temp_y,ay[4];
    int32_t ni, nj, j1, j2, npts, i;
    int32_t quatre = 4;
    int32_t un = 1;
    int32_t gdrow_in, gdrow_out, gdcol_in, gdcol_out, idx_gdin;
    _gridset *gset;
    _Grille laGrille;

    c_gdkey2rowcol(gdin,  &gdrow_in,  &gdcol_in);
    c_gdkey2rowcol(gdout, &gdrow_out, &gdcol_out);
    idx_gdin = c_find_gdin(gdin, gdout);

    gset = &(Grille[gdrow_out][gdcol_out].gset[idx_gdin]);
    laGrille = Grille[gdrow_in][gdcol_in];

    ni = laGrille.ni;
    nj = laGrille.j2 - laGrille.j1 + 1;

    j1 = laGrille.j2-2;
    j2 = j1 + 3;

    gset = &(Grille[gdrow_out][gdcol_out].gset[idx_gdin]);
    npts = gset->zones[AU_NORD].npts;

    polar_uu_in = (float *) malloc(4 * ni * sizeof(float));
    polar_vv_in = (float *) malloc(4 * ni * sizeof(float));
    corr_uus = (float *) malloc(npts * sizeof(float));
    corr_vvs = (float *) malloc(npts * sizeof(float));

    ez_calcnpolarwind(polar_uu_in, polar_vv_in, uuin, vvin, ni, nj, gdin);

    switch (groptions.degre_interp)
    {
    case CUBIQUE:
        switch (laGrille.grtyp[0])
            {
            case 'Z':
            case 'E':
            case 'G':
            if (laGrille.ay[j1+1] == 90.0)
                {
                ay[0] = laGrille.ay[j1-2];
                ay[1] = laGrille.ay[j1-1];
                ay[2] = laGrille.ay[j1];
                ay[3] = laGrille.ay[j1+1];
                }
            else
                {
                ay[0] = laGrille.ay[j1-1];
                ay[1] = laGrille.ay[j1];
                ay[2] = laGrille.ay[j1+1];
                ay[3] = 90.0;
                }

            f77name(ez_irgdint_3_wnnc)(corr_uus,gset->zones[AU_NORD].x, gset->zones[AU_NORD].y,&npts,
                        laGrille.ax, ay, polar_uu_in,
                        &ni, &j1, &j2, &laGrille.extension);
            f77name(ez_irgdint_3_wnnc)(corr_vvs,gset->zones[AU_NORD].x, gset->zones[AU_NORD].y,&npts,
                        laGrille.ax, ay, polar_vv_in,
                        &ni, &j1, &j2, &laGrille.extension);
            break;

            default:
            f77name(ez_rgdint_3_wnnc)(corr_uus,gset->zones[AU_NORD].x, gset->zones[AU_NORD].y,&npts,
                        polar_uu_in,&ni, &j1, &j2, &laGrille.extension);
            f77name(ez_rgdint_3_wnnc)(corr_vvs,gset->zones[AU_NORD].x,
                                            gset->zones[AU_NORD].y,&npts,
                        polar_vv_in,&ni, &j1, &j2, &laGrille.extension);
            break;
            }
        break;

    case LINEAIRE:
        temp_y = (float *) malloc(npts*sizeof(float));
        for (i=0; i < npts; i++)
            {
            temp_y[i] = gset->zones[AU_NORD].y[i] - (1.0 * (laGrille.j2-3));
            }
        f77name(ez_rgdint_1_w)(corr_uus,gset->zones[AU_NORD].x,temp_y,&npts,polar_uu_in,&ni, &un, &quatre, &laGrille.extension);
        f77name(ez_rgdint_1_w)(corr_vvs,gset->zones[AU_NORD].x,temp_y,&npts,polar_vv_in,&ni, &un, &quatre, &laGrille.extension);
        free(temp_y);
        break;

    case VOISIN:
        temp_y = (float *) malloc(npts*sizeof(float));
        for (i=0; i < npts; i++)
            {
            temp_y[i] = gset->zones[AU_NORD].y[i] - (1.0 * (laGrille.j2-3));
            }
        f77name(ez_rgdint_0)(corr_uus,gset->zones[AU_NORD].x,temp_y,&npts,polar_uu_in,&ni, &un, &quatre);
        f77name(ez_rgdint_0)(corr_vvs,gset->zones[AU_NORD].x,temp_y,&npts,polar_vv_in,&ni, &un, &quatre);
        free(temp_y);
        break;
    }


    for (i=0; i < gset->zones[AU_NORD].npts; i++)
    {
    uuout[gset->zones[AU_NORD].idx[i]] = corr_uus[i];
    vvout[gset->zones[AU_NORD].idx[i]] = corr_vvs[i];
    }

    free(polar_uu_in);
    free(polar_vv_in);
    free(corr_uus);
    free(corr_vvs);

    return 0;
}
