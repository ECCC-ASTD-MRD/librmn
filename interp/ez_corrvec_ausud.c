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

#include "ezscint.h"
#include "ez_funcdef.h"
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint ez_corrvec_ausud(ftnfloat *uuout, ftnfloat *vvout, 
			 ftnfloat *uuin, ftnfloat *vvin, 
			 _gridset *gdset)
{
  ftnfloat *polar_uu_in, *polar_vv_in, *corr_uus, *corr_vvs, *temp_y, ay[4];
  wordint gdin, ni, nj, i1, i2, j1, j2, degree,npts,i;
  wordint quatre = 4;
  wordint un = 1;
  
  npts = gdset->zones[AU_SUD].npts;
  gdin = gdset->gdin;
  ni = Grille[gdin].ni;
  nj = Grille[gdin].j2;
  
  i1 = 1;
  i2 = ni;
  j1 = 0;
  j2 = 3;
  degree = 3;
  
  polar_uu_in = (ftnfloat *) malloc(4 * ni * sizeof(ftnfloat));
  polar_vv_in = (ftnfloat *) malloc(4 * ni * sizeof(ftnfloat));
  corr_uus = (ftnfloat *) malloc(npts * sizeof(ftnfloat));
  corr_vvs = (ftnfloat *) malloc(npts * sizeof(ftnfloat));
  
  ez_calcspolarwind(polar_uu_in, polar_vv_in, uuin, vvin, ni, nj, gdin);
  
  switch (groptions.degre_interp)
    {
    case CUBIQUE:
      switch (Grille[gdin].grtyp)
	{
	case 'Z':
	case 'E':
	case 'G':
	  ay[0] = -90.;
	  ay[1] = Grille[gdin].ay[0];
	  ay[2] = Grille[gdin].ay[1];
	  ay[3] = Grille[gdin].ay[2];
	  f77name(ez_irgdint_3_wnnc)(corr_uus,gdset->zones[AU_SUD].x,
				     gdset->zones[AU_SUD].y,&npts,
				     Grille[gdin].ax, ay, polar_uu_in,
				     &ni, &j1, &j2, &Grille[gdin].extension);
	  f77name(ez_irgdint_3_wnnc)(corr_vvs,gdset->zones[AU_SUD].x,
				     gdset->zones[AU_SUD].y,&npts,
				     Grille[gdin].ax, ay, polar_vv_in,
				     &ni, &j1, &j2, &Grille[gdin].extension);
	  break;
	  
	default:
	  f77name(ez_rgdint_3_wnnc)(corr_uus,gdset->zones[AU_SUD].x,
				    gdset->zones[AU_SUD].y,&npts,
				    polar_uu_in,&ni, &j1, &j2, &Grille[gdin].extension);
	  f77name(ez_rgdint_3_wnnc)(corr_vvs,gdset->zones[AU_SUD].x,
				    gdset->zones[AU_SUD].y,&npts,
				    polar_vv_in,&ni, &j1, &j2, &Grille[gdin].extension);
	  break;
	}
      break;
      
    case LINEAIRE:
      temp_y = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      for (i=0; i < npts; i++)
	{
	temp_y[i] = gdset->zones[AU_SUD].y[i] + 1.0;
	}
      f77name(ez_rgdint_1_nw)(corr_uus,gdset->zones[AU_SUD].x,temp_y,&npts,polar_uu_in,&ni, &un, &quatre);
      f77name(ez_rgdint_1_nw)(corr_vvs,gdset->zones[AU_SUD].x,temp_y,&npts,polar_vv_in,&ni, &un, &quatre);
      free(temp_y);
      break;
      
    case VOISIN:
      temp_y = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      for (i=0; i < npts; i++)
	{
	temp_y[i] = gdset->zones[AU_SUD].y[i] +  1.0;
	}
      switch(Grille[gdin].extension)
        {
        case 2:
        f77name(ez_rgdint_0_w)(corr_uus,gdset->zones[AU_SUD].x,temp_y,&npts,polar_uu_in,&ni, &un, &quatre, &Grille[gdin].extension);
        f77name(ez_rgdint_0_w)(corr_vvs,gdset->zones[AU_SUD].x,temp_y,&npts,polar_vv_in,&ni, &un, &quatre, &Grille[gdin].extension);
        break;

        default:
        f77name(ez_rgdint_0)(corr_uus,gdset->zones[AU_SUD].x,temp_y,&npts,polar_uu_in,&ni, &un, &quatre);
        f77name(ez_rgdint_0)(corr_vvs,gdset->zones[AU_SUD].x,temp_y,&npts,polar_vv_in,&ni, &un, &quatre);
        break;
        }
      free(temp_y);
      break;
    }
  
  
  for (i=0; i < gdset->zones[AU_SUD].npts; i++)
    {
    uuout[gdset->zones[AU_SUD].idx[i]] = corr_uus[i];
    vvout[gdset->zones[AU_SUD].idx[i]] = corr_vvs[i];
    }
  
  free(polar_uu_in);
  free(polar_vv_in);
  free(corr_uus);
  free(corr_vvs);
  
  return 0;  
}
