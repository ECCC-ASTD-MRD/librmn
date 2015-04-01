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


wordint c_ezgenerate_gem_cache()
{
return 0;
}
/*
   Cette routine accelere la rotation des vents lorsque une des grilles d'une
     paire "gdin-gdout" est de type GEM
*/

/*
   wordint gdidin, gdidout,npts;
   ftnfloat xlat1, xlon1, xlat2, xlon2;

   if (gridset[iset].use_sincos_cache == NON)
     {
     return -1;
     }

   gdidin = gridset[iset].gdin;
   gdidout= gridset[iset].gdout;
   npts = Grille[gdidout].ni*Grille[gdidout].nj;

   ez_calclatlon(gdidin);
   ez_calclatlon(gdidout);

    latp, lon_true : latlon sur la terre (coordonnees naturelles)
      lat, lon   : latlon TOURNEES
*/
/*
   if ((((Grille[gdidout].grtyp == 'Z' || Grille[gdidout].grtyp == '#') && Grille[gdidout].grref == 'E') || (Grille[gdidout].grtyp == 'E'))
       && !(gridset[iset].gemout.flags & SINLATLON_OK))
      {
            fprintf(stderr,"%d %d %d\n", gridset[iset].gemout.flags, SINLATLON_OK, !(gridset[iset].gemout.flags & SINLATLON_OK));

      gridset[iset].gemout.lat_true = Grille[gdidout].lat;
      gridset[iset].gemout.lon_true = Grille[gdidout].lon;
      gridset[iset].gemout.lat_rot = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      gridset[iset].gemout.lon_rot = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      gridset[iset].gemout.sinlat_rot  = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      gridset[iset].gemout.coslat_rot  = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      gridset[iset].gemout.sinlat_true = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      gridset[iset].gemout.coslat_true = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      gridset[iset].gemout.sinlon_rot  = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      gridset[iset].gemout.coslon_rot  = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      gridset[iset].gemout.sinlon_true = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      gridset[iset].gemout.coslon_true = (ftnfloat *) malloc(npts*sizeof(ftnfloat));

      if (Grille[gdidout].grtyp == 'Z' || Grille[gdidout].grtyp == '#')
         {
	 c_ezgfxyfll(gridset[iset].gemout.lon_true,gridset[iset].gemout.lat_true,
		     gridset[iset].gemout.lon_rot,gridset[iset].gemout.lat_rot,
		     gridset[iset].gemout.r, gridset[iset].gemout.ri,&npts,
		     &Grille[gdidout].xgref[XLAT1], &Grille[gdidout].xgref[XLON1],
		     &Grille[gdidout].xgref[XLAT2], &Grille[gdidout].xgref[XLON2]);
         }
      else
         {
	 c_ezgfxyfll(gridset[iset].gemout.lon_true,gridset[iset].gemout.lat_true,
		     gridset[iset].gemout.lon_rot,gridset[iset].gemout.lat_rot,
		     gridset[iset].gemout.r, gridset[iset].gemout.ri,&npts,
		     &Grille[gdidout].xg[XLAT1], &Grille[gdidout].xg[XLON1],
		     &Grille[gdidout].xg[XLAT2], &Grille[gdidout].xg[XLON2]);
         }

      f77name(ezsincoslatlon)(gridset[iset].gemout.lat_rot, gridset[iset].gemout.lon_rot,
                              gridset[iset].gemout.sinlat_rot, gridset[iset].gemout.sinlon_rot,
                              gridset[iset].gemout.coslat_rot,gridset[iset].gemout.coslon_rot, &npts);
      f77name(ezsincoslatlon)(gridset[iset].gemout.lat_true, gridset[iset].gemout.lon_true,
                              gridset[iset].gemout.sinlat_true, gridset[iset].gemout.sinlon_true,
                              gridset[iset].gemout.coslat_true,gridset[iset].gemout.coslon_true, &npts);
      gridset[iset].gemout.flags |= LATLON_PRIME_OK;
      gridset[iset].gemout.flags |= SINLATLON_OK;
            fprintf(stderr,"%d %d %d\n", gridset[iset].gemout.flags, SINLATLON_OK, !(gridset[iset].gemout.flags & SINLATLON_OK));
      }
*/
/*   if ((((Grille[gdidin].grtyp == 'Z' || Grille[gdidin].grtyp == '#') && Grille[gdidin].grref == 'E') || Grille[gdidin].grtyp == 'E')
       && !(gridset[iset].gemin.flags & SINLATLON_OK))
      {
      gridset[iset].gemin.lat_true = Grille[gdidout].lat;
      gridset[iset].gemin.lon_true = Grille[gdidout].lon;
      gridset[iset].gemin.lat_rot = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      gridset[iset].gemin.lon_rot = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      gridset[iset].gemin.sinlat_rot  = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      gridset[iset].gemin.coslat_rot  = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      gridset[iset].gemin.sinlat_true = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      gridset[iset].gemin.coslat_true = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      gridset[iset].gemin.sinlon_rot  = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      gridset[iset].gemin.coslon_rot  = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      gridset[iset].gemin.sinlon_true = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      gridset[iset].gemin.coslon_true = (ftnfloat *) malloc(npts*sizeof(ftnfloat));


      if (Grille[gdidin].grtyp == 'Z' || Grille[gdidin].grtyp == '#')
         {
	 c_ezgfxyfll(gridset[iset].gemin.lon_true,gridset[iset].gemin.lat_true,
		     gridset[iset].gemin.lon_rot,gridset[iset].gemin.lat_rot,
		     gridset[iset].gemin.r, gridset[iset].gemin.ri,&npts,
		     &Grille[gdidin].xgref[XLAT1], &Grille[gdidin].xgref[XLON1],
		     &Grille[gdidin].xgref[XLAT2], &Grille[gdidin].xgref[XLON2]);
         }
      else
         {
	 c_ezgfxyfll(gridset[iset].gemin.lon_true,gridset[iset].gemin.lat_true,
		     gridset[iset].gemin.lon_rot,gridset[iset].gemin.lat_rot,
		     gridset[iset].gemin.r, gridset[iset].gemin.ri,&npts,
		     &Grille[gdidin].xg[XLAT1], &Grille[gdidin].xg[XLON1],
		     &Grille[gdidin].xg[XLAT2], &Grille[gdidin].xg[XLON2]);
         }

      f77name(ezsincoslatlon)(gridset[iset].gemin.lat_rot, gridset[iset].gemin.lon_rot,
                       gridset[iset].gemin.sinlat_rot, gridset[iset].gemin.sinlon_rot,
                       gridset[iset].gemin.coslat_rot,gridset[iset].gemin.coslon_rot, &npts);
      f77name(ezsincoslatlon)(gridset[iset].gemin.lat_true, gridset[iset].gemin.lon_rot,
                       gridset[iset].gemin.sinlat_true, gridset[iset].gemin.sinlon_true,
                       gridset[iset].gemin.coslat_true,gridset[iset].gemin.coslon_true, &npts);
      gridset[iset].gemin.flags |= LATLON_PRIME_OK;
      gridset[iset].gemin.flags |= SINLATLON_OK;
      }
*/
/*  return 0;*/
/*
}
*/
