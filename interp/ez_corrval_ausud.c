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
wordint ez_corrval_ausud(ftnfloat *zout, ftnfloat *zin, _gridset *gdset)
{
  wordint i;
  wordint gdin;
  wordint npts;
  ftnfloat vpolesud;
  ftnfloat *temp, *temp_y, *vals;
  ftnfloat ay[4];
  wordint ni, nj, i1, i2, j1, j2;
  wordint un = 1;
  wordint quatre = 4;
  
  npts = gdset->zones[AU_SUD].npts;
  if (npts > 0)
    {
    gdin = gdset->gdin;
    ni = Grille[gdin].ni;
    
    i1 = 1;
    i2 = ni;
    j1 = 0;
    j2 = 3;
    
    temp = (ftnfloat *) malloc(4 * ni * sizeof(ftnfloat));
    vals = (ftnfloat *) malloc(npts * sizeof(ftnfloat));
    f77name(ez_calcpoleval)(&vpolesud, zin, &ni, Grille[gdin].ax, &Grille[gdin].extension, 
			    &Grille[gdin].grtyp, &Grille[gdin].grref);
    f77name(ez_fillspole)(temp, zin, &ni, &Grille[gdin].j1, &Grille[gdin].j2, &vpolesud);

    switch (groptions.degre_interp)
      {
      case CUBIQUE:
      switch (Grille[gdin].grtyp)
        {
        case 'Z':
        case 'E':
        case 'G':
          ay[0] = -90.0;
          ay[1] = Grille[gdin].ay[0];
          ay[2] = Grille[gdin].ay[1];
          ay[3] = Grille[gdin].ay[2];
          f77name(ez_irgdint_3_wnnc)(vals,gdset->zones[AU_SUD].x,
                      gdset->zones[AU_SUD].y,&npts,
                      Grille[gdin].ax, ay, temp,
                      &ni, &j1, &j2, &Grille[gdin].extension);
          break;
          
        default:
          f77name(ez_rgdint_3_wnnc)(vals,gdset->zones[AU_SUD].x,
                      gdset->zones[AU_SUD].y,&npts,
                      temp,&ni, &j1, &j2, &Grille[gdin].extension);
          break;
        }
      break;
	
      case LINEAIRE:	
      temp_y = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      for (i=0; i < npts; i++)
        {
        temp_y[i] = gdset->zones[AU_SUD].y[i] + 1.0;
        }
      switch (Grille[gdin].grtyp)
        {
        case 'G':
        case 'B':
        case 'A':
        f77name(ez_rgdint_1_w)(vals,gdset->zones[AU_SUD].x,temp_y,&npts,temp,&ni, &un, &quatre, &Grille[gdin].extension);
        break;

        default:
        f77name(ez_rgdint_1_nw)(vals,gdset->zones[AU_SUD].x,temp_y,&npts,temp,&ni, &un, &quatre);
        break;
        }
      free(temp_y);
      break;
	
      case VOISIN:
      temp_y = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      for (i=0; i < npts; i++)
        {
        temp_y[i] = gdset->zones[AU_SUD].y[i] + 1.0;
        }
      
      switch(Grille[gdin].extension)
        {
        case 2:
        f77name(ez_rgdint_0_w)(vals,gdset->zones[AU_SUD].x,temp_y,&npts,temp,&ni, &un, &quatre, &Grille[gdin].extension);
        break;

        default:
        f77name(ez_rgdint_0)(vals,gdset->zones[AU_SUD].x,temp_y,&npts,temp,&ni, &un, &quatre);
        break;
        }

      free(temp_y);
      break;
      }

    for (i=0; i < gdset->zones[AU_SUD].npts; i++)
      {
      zout[gdset->zones[AU_SUD].idx[i]] = vals[i];
      }
    
    free(vals);
    free(temp);
    }
  return 0;
}

