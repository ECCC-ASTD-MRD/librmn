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


#include <stdint.h>
#include <stdlib.h>

#include <rmn/ezscint.h>
#include "ez_funcdef.h"


int32_t ez_corrval_ausud(float *zout, float *zin, int32_t gdin, int32_t gdout)
{
  int32_t i;
  int32_t npts;
  float vpolesud;
  float *temp, *temp_y, *vals;
  float ay[4];
  int32_t ni, j1, j2;
  _Grille *lgdin;

  int32_t gdrow_in, gdrow_out, gdcol_in, gdcol_out, idx_gdin;
  _gridset *gset;

  c_gdkey2rowcol(gdin,  &gdrow_in,  &gdcol_in);
  c_gdkey2rowcol(gdout, &gdrow_out, &gdcol_out);
  idx_gdin = c_find_gdin(gdin, gdout);

  lgdin = &(Grille[gdrow_in][gdcol_in]);
  &(Grille[gdrow_out][gdcol_out]);

  gset = &(Grille[gdrow_out][gdcol_out].gset[idx_gdin]);
  npts = gset->zones[AU_SUD].npts;
  if (npts > 0)
    {
    ni = lgdin->ni;

    j1 = lgdin->j1 - 1;
    j2 = j1 + 3;

    temp = (float *) malloc(4 * ni * sizeof(float));
    vals = (float *) malloc(npts * sizeof(float));
    f77name(ez_calcpoleval)(&vpolesud, zin, &ni, lgdin->ax,
			    &lgdin->grtyp, &lgdin->grref,1,1);
    f77name(ez_fillspole)(temp, zin, &ni, &lgdin->j1, &lgdin->j2, &vpolesud);

    switch (groptions.degre_interp)
      {
      case CUBIQUE:
	   switch (lgdin->grtyp[0])
	     {
	     case 'Z':
	     case 'E':
	     case 'G':
          if  (lgdin->ay[lgdin->j1-1] == -90.0)
             {
                ay[0] = lgdin->ay[0];
                ay[1] = lgdin->ay[1];
                ay[2] = lgdin->ay[2];
                ay[3] = lgdin->ay[3];
                f77name(ez_irgdint_3_wnnc)(vals,gset->zones[AU_SUD].x,
                            gset->zones[AU_SUD].y,&npts,
                            lgdin->ax, ay, temp,
                            &ni, &j1, &j2, &lgdin->extension);
             }
    else
       {
             ay[0] = -90.0;
             ay[1] = lgdin->ay[0];
             ay[2] = lgdin->ay[1];
             ay[3] = lgdin->ay[2];
             f77name(ez_irgdint_3_wnnc)(vals,gset->zones[AU_SUD].x,
                         gset->zones[AU_SUD].y,&npts,
                         lgdin->ax, ay, temp,
                         &ni, &j1, &j2, &lgdin->extension);

       }
	       break;

	     default:
	       f77name(ez_rgdint_3_wnnc)(vals,gset->zones[AU_SUD].x,
				         gset->zones[AU_SUD].y,&npts,
				         temp,&ni, &j1, &j2, &lgdin->extension);
	       break;
	     }
	break;

      case LINEAIRE:
	   temp_y = (float *) malloc(npts*sizeof(float));
/*	   for (i=0; i < npts; i++)
	     {
	     temp_y[i] = gset->zones[AU_SUD].y[i] - (1.0*j1);
	     }
	   f77name(ez_rgdint_1_nw)(vals,gset->zones[AU_SUD].x,temp_y,&npts,temp,&ni, &un, &quatre);*/
   	   f77name(ez_rgdint_1_w)(vals,gset->zones[AU_SUD].x,gset->zones[AU_SUD].y,&npts,temp,&ni, &j1, &j2,
&lgdin->extension);
	   free(temp_y);
	   break;

      case VOISIN:
  	   temp_y = (float *) malloc(npts*sizeof(float));
/*	   for (i=0; i < npts; i++)
	     {
	     temp_y[i] = gset->zones[AU_SUD].y[i] - (1.0*j1);
	     }*/
	   f77name(ez_rgdint_0)(vals,gset->zones[AU_SUD].x,gset->zones[AU_SUD].y,&npts,temp,&ni, &j1, &j2);
	   free(temp_y);
	   break;
      }

    for (i=0; i < gset->zones[AU_SUD].npts; i++)
      {
      zout[gset->zones[AU_SUD].idx[i]] = vals[i];
      }

    free(vals);
    free(temp);
    }
  return 0;
}
