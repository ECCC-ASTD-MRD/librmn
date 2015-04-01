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

wordint c_ezdefaxes(wordint gdid, ftnfloat *ax, ftnfloat *ay)
{
  wordint i,j;
  ftnfloat *temp, dlon;
  wordint zero, deuxnj;
  
  _Grille *gr;
  
  gr = &Grille[gdid];
  switch (gr->grtyp)
    {
    case '#':
    case 'Z':
      f77name(cigaxg)(&gr->grref,&gr->xgref[XLAT1], &gr->xgref[XLON1], &gr->xgref[XLAT2], &gr->xgref[XLON2],
		      &gr->igref[IG1], &gr->igref[IG2], &gr->igref[IG3], &gr->igref[IG4]);
      
      Grille[gdid].ax = (ftnfloat *) malloc(gr->ni*sizeof(ftnfloat));
      Grille[gdid].ay = (ftnfloat *) malloc(gr->nj*sizeof(ftnfloat));
      
      memcpy(Grille[gdid].ax,ax,gr->ni*sizeof(ftnfloat));
      memcpy(Grille[gdid].ay,ay,gr->nj*sizeof(ftnfloat));
      ez_calcxpncof(gdid);
      ez_calcntncof(gdid);
      break;
      
    case 'Y':
      Grille[gdid].ax = (ftnfloat *) malloc(gr->ni*gr->nj*sizeof(ftnfloat));
      Grille[gdid].ay = (ftnfloat *) malloc(gr->ni*gr->nj*sizeof(ftnfloat));
      memcpy(Grille[gdid].ax,ax,gr->ni*gr->nj*sizeof(ftnfloat));
      memcpy(Grille[gdid].ay,ay,gr->ni*gr->nj*sizeof(ftnfloat));
      
      ez_calcxpncof(gdid);
      break;
      
    case 'G':
      gr->grref = 'L';
      gr->xgref[SWLAT] = 0.0;
      gr->xgref[SWLON] = 0.0;
      gr->xgref[DLAT] = 1.0;
      gr->xgref[DLON] = 1.0;
      f77name(cxgaig)(&gr->grref,&gr->igref[IG1], &gr->igref[IG2], &gr->igref[IG3], &gr->igref[IG4],
		      &gr->xgref[SWLAT], &gr->xgref[SWLON], &gr->xgref[DLAT], &gr->xgref[DLON]);
      
      Grille[gdid].ax = (ftnfloat *) malloc(gr->ni*sizeof(ftnfloat));
      dlon = 360. / (ftnfloat) gr->ni;
      for (i=0; i < gr->ni; i++)
	{
	Grille[gdid].ax[i] = (ftnfloat)i * dlon;
	}
      
      zero = 0;
      ez_calcxpncof(gdid);

      switch (Grille[gdid].ig[IG1])
	{
	case GLOBAL:
	  Grille[gdid].ay = (ftnfloat *) malloc(gr->nj*sizeof(ftnfloat));
	  temp    = (ftnfloat *) malloc(gr->nj*sizeof(ftnfloat));
	  f77name(ez_glat)(Grille[gdid].ay,temp,&gr->nj,&zero);
	  free(temp);
	  break;
	  
	case NORD:
	case SUD:
	  deuxnj = 2 * gr->nj;
	  Grille[gdid].ay = (ftnfloat *) malloc(deuxnj*sizeof(ftnfloat));
	  temp    = (ftnfloat *) malloc(deuxnj*sizeof(ftnfloat));
	  f77name(ez_glat)(Grille[gdid].ay,temp,&deuxnj,&zero);
	  free(temp);
	  break;
	}
      
      
      ez_calcntncof(gdid);
      Grille[gdid].flags |= AX;
      break;
      
    default:
      ez_calcxpncof(gdid);
      break;
    }
  
  
  Grille[gdid].flags |= AX;
  return 0;
}
