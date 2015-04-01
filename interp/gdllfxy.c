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
wordint f77name(gdllfxy)(wordint *gdid, ftnfloat *lat, ftnfloat *lon, ftnfloat *x, ftnfloat *y, wordint *n)
{
  return c_gdllfxy(*gdid, lat, lon, x, y, *n);
 
}

wordint c_gdllfxy(wordint gdid, ftnfloat *lat, ftnfloat *lon, ftnfloat *x, ftnfloat *y, wordint n)
{
  ftnfloat xlat1, xlon1, xlat2, xlon2;
  wordint i,j, npts, un;
  ftnfloat *tmpx, *tmpy;
  ftnfloat delxx, delyy;
  ftnfloat dlat, dlon, swlat, swlon;
  wordint indx, indy;
  
  _Grille gr;
  
  gr = Grille[gdid];
  npts = n;
  un = 1;
  
  switch(gr.grtyp)
    {
    case 'A': 
      for (i=0; i < n; i++)
	{
	lat[i] = (y[i]-1.0)*gr.xg[DLAT]+gr.xg[SWLAT];
	lon[i] = (x[i]-1.0)*gr.xg[DLON]+gr.xg[SWLON];
	lon[i] = (ftnfloat) (fmod((double) (lon[i] + 360.0), (double) 360.0));
	}
      break;
      
    case 'B':
      for (i=0; i < n; i++)
	{
	lat[i] = (y[i]-1.0)*gr.xg[DLAT]+gr.xg[SWLAT];
	lon[i] = (x[i]-1.0)*gr.xg[DLON]+gr.xg[SWLON];
	lon[i] = (ftnfloat) (fmod((double) (lon[i] + 360.0), (double) 360.0));
	}
      break;
      
    case 'E':
      tmpx = (ftnfloat *) malloc(n*sizeof(ftnfloat));
      tmpy = (ftnfloat *) malloc(n*sizeof(ftnfloat));
      for (i=0; i < n; i++)
	{
	dlat  = 180.0 / gr.nj;
	dlon  = 360.0 / (gr.ni - 1);
	swlat = -90.0 + 0.5 * dlat;
	swlon = 0.0;
	tmpx[i] = (x[i]-1.0)*dlon+swlon;
	tmpy[i] = (y[i]-1.0)*dlat+swlat;
	}
      
      f77name(ez_gfllfxy)(lon,lat,tmpx,tmpy,&n,&gr.xg[XLAT1],&gr.xg[XLON1],&gr.xg[XLAT2],&gr.xg[XLON2]);
      free(tmpx);
      free(tmpy);
      break;
      
      
    case 'L':
      for (i=0; i < n; i++)
	{
	lon[i] = (x[i]-1.0)*gr.xg[DLON]+gr.xg[SWLON];
	lon[i] = (ftnfloat) (fmod((double) (lon[i] + 360.0), (double) 360.0));
	lat[i] = (y[i]-1.0)*gr.xg[DLAT]+gr.xg[SWLAT];
	}
      break;
      
    case 'N':
    case 'S':
      f77name(ez_vllfxy)(lat,lon,x,y,&npts,&un,&gr.xg[D60],&gr.xg[DGRW],&gr.xg[PI],&gr.xg[PJ],&gr.hemisphere);
      for (i=0; i < n; i++)
	{
	lon[i] = (ftnfloat) (fmod((double) (lon[i] + 360.0), (double) 360.0));
	}
      break;
      
    case 'T':
      f77name(ez_vtllfxy)(lat,lon,x,y, &gr.xg[CLAT], &gr.xg[CLON], &gr.xg[TD60], &gr.xg[TDGRW], &gr.ni, &gr.nj, &npts);
      break;
      
    case '!':
      f77name(ez_llflamb)(lat,lon,x,y,&npts,&gr.grtyp,&gr.ig[IG1], &gr.ig[IG2], &gr.ig[IG3], &gr.ig[IG4]);
      break;


    case 'Y':
      fprintf(stderr, "********************************************************\n");
      fprintf(stderr, "<gdllfxy>: This operation is not supported for 'Y' grids\n");
      fprintf(stderr, "********************************************************\n");
      break;
      
    case '#':
    case 'Z':
    case 'G':
      tmpx = (ftnfloat *) malloc(n*sizeof(ftnfloat));
      tmpy = (ftnfloat *) malloc(n*sizeof(ftnfloat));
      for (i=0; i < n; i++)
	{
	indx = (int)x[i]-1;
	indy = (int)y[i]-1;
	
	indx = indx < 0 ? 0 : indx;
	indy = indy < 0 ? 0 : indy;
	indx = indx > gr.ni-2 ? gr.ni-2 : indx;
	indy = indy > gr.j2-2 ? gr.j2-2 : indy;
	delxx = gr.ax[indx+1]-gr.ax[indx];
	tmpx[i] = gr.ax[indx] + ((x[i]-1.0-indx)*delxx);
	
	delyy = gr.ay[indy+1]-gr.ay[indy];
	tmpy[i] = gr.ay[indy] + ((y[i]-1.0-indy)*delyy);
	}
      
      switch (gr.grref)
	{
	case 'E':
	  f77name(cigaxg)(&gr.grref,&xlat1,&xlon1,&xlat2,&xlon2,
			  &gr.igref[IG1],&gr.igref[IG2],&gr.igref[IG3],&gr.igref[IG4]);
	  f77name(ez_gfllfxy)(lon, lat, tmpx, tmpy, &npts, &gr.xgref[XLAT1], &gr.xgref[XLON1],
			      &gr.xgref[XLAT2], &gr.xgref[XLON2]);
	  break;
	  
	case 'S':
	case 'N':
	  f77name(ez_vllfxy)(lat,lon,tmpx,tmpy,&npts,&un,&gr.xgref[D60],
			     &gr.xgref[DGRW],&gr.xgref[PI],&gr.xgref[PJ],&gr.hemisphere);
	  for (i=0; i < n; i++)
	    {
	    lon[i] = (ftnfloat) (fmod((double) (lon[i] + 360.0), (double) 360.0));
	    }
	  break;
	  
	case 'L':
	  for (i=0; i < n; i++)
	    {
	    lat[i] = (tmpy[i])*gr.xgref[DLAT]+gr.xgref[SWLAT];
	    lon[i] = (tmpx[i])*gr.xgref[DLON]+gr.xgref[SWLON];
	    lon[i] = (ftnfloat) (fmod((double) (lon[i] + 360.0), (double) 360.0));
	    }
	  break;
	  
	default:
	  fprintf(stderr,"<gdllfxy> Errrrrrrrrrrreur!\n");
	  break;
	}
      free(tmpx);
      free(tmpy);
      break;
    }
  
  return 0;
  
}
