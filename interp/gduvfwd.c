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
wordint f77name(gduvfwd)(wordint *gdid, ftnfloat *uugdout, ftnfloat *vvgdout, 
                         ftnfloat *uullin, ftnfloat *vvllin, ftnfloat *latin, ftnfloat *lonin, wordint *npts)
{
   wordint icode;
   
   icode = c_gduvfwd(*gdid, uugdout, vvgdout, uullin, vvllin, latin, lonin, *npts);
   return icode;
}

wordint c_gduvfwd(wordint gdid,  ftnfloat *uugdout, ftnfloat *vvgdout, ftnfloat *uullin, ftnfloat *vvllin,
              ftnfloat *latin, ftnfloat *lonin, wordint npts)
{
   ftnfloat *xlatingf, *xloningf, *xlatingf2, *xloningf2, *uvcart, *xyz;
   wordint ni, nj, use_sincos_cache;
   ftnfloat *lat_true,*lon_true;

   ni = npts;
   nj = 1;

   memcpy(uugdout, uullin, npts*sizeof(ftnfloat));
   memcpy(vvgdout, vvllin, npts*sizeof(ftnfloat));
   
   if (iset == -1)
     {
     use_sincos_cache = NON;
     }
   else
     {
     if (gridset[iset].gemin.lat_true == NULL || gridset[iset].use_sincos_cache == NON)
       {
       use_sincos_cache = NON;
       }
     else
       {
       use_sincos_cache = OUI;
       }
     }

   use_sincos_cache = NON;
   switch (Grille[gdid].grtyp)
      {
      case 'E':
        if (use_sincos_cache == NON)
	  {
	  lat_true=(ftnfloat *)(malloc(npts*sizeof(ftnfloat)));
	  lon_true=(ftnfloat *)(malloc(npts*sizeof(ftnfloat)));
	  f77name(ez_gfxyfll)(lon_true,lat_true,lonin,latin,&ni,
			      &Grille[gdid].xg[XLAT1],&Grille[gdid].xg[XLON1],
			      &Grille[gdid].xg[XLAT2],&Grille[gdid].xg[XLON2]);
	  
	  c_ezgfwfllw(uugdout,vvgdout,latin,lonin,lat_true,lon_true,
		      &ni,&nj,&Grille[gdid].grtyp,
		      &Grille[gdid].ig[IG1],&Grille[gdid].ig[IG2],
		      &Grille[gdid].ig[IG3],&Grille[gdid].ig[IG4]);
	  free(lat_true);
	  free(lon_true);
	  return 0;
	  }
	else
	  {
	  /*	  c_ezgfwfllw(uugdout,vvgdout, latin, lonin,
		  gridset[iset].gemout.lat_rot, gridset[iset].gemout.lon_rot,
		  &ni,&nj,&Grille[gdid].grtyp,
		  &Grille[gdid].ig[IG1],&Grille[gdid].ig[IG2],
		  &Grille[gdid].ig[IG3],&Grille[gdid].ig[IG4]);
	  */
	  uvcart = (ftnfloat *) malloc(3*npts*sizeof(ftnfloat));
	  xyz    = (ftnfloat *) malloc(3*npts*sizeof(ftnfloat));
	  c_gfwfllw_turbo(uugdout,vvgdout, latin, lonin,
			  gridset[iset].gemout.lat_rot, gridset[iset].gemout.lon_rot, uvcart, xyz,
			  gridset[iset].gemout.sinlat_rot, gridset[iset].gemout.sinlon_rot, gridset[iset].gemout.coslat_rot, gridset[iset].gemout.coslon_rot,
			  gridset[iset].gemout.sinlat_true, gridset[iset].gemout.sinlon_true, gridset[iset].gemout.coslat_true, gridset[iset].gemout.coslon_true,
			  gridset[iset].gemout.r, gridset[iset].gemout.ri,
			  &ni,&nj, &Grille[gdid].grtyp, &Grille[gdid].ig[IG1],&Grille[gdid].ig[IG2], &Grille[gdid].ig[IG3],&Grille[gdid].ig[IG4]);
	  
	  free(uvcart);
	  free(xyz);
	  }
        break;
        
        
      case '#':
      case 'Y':
      case 'Z':
        switch(Grille[gdid].grref)
	  {
	  case 'E':
	    if (use_sincos_cache == NON)
	      {
	      lat_true=(ftnfloat *)(malloc(npts*sizeof(ftnfloat)));
	      lon_true=(ftnfloat *)(malloc(npts*sizeof(ftnfloat)));
	      f77name(ez_gfxyfll)(lonin,latin,lon_true,lat_true,&ni,
				  &Grille[gdid].xgref[XLAT1],&Grille[gdid].xgref[XLON1],
				  &Grille[gdid].xgref[XLAT2],&Grille[gdid].xgref[XLON2]);
	      
	      c_ezgfwfllw(uugdout,vvgdout,latin,lonin,lat_true,lon_true,
			  &ni,&nj,&Grille[gdid].grref,
			  &Grille[gdid].igref[IG1],&Grille[gdid].igref[IG2],
			  &Grille[gdid].igref[IG3],&Grille[gdid].igref[IG4]);
	      free(lat_true);
	      free(lon_true);
	      return 0;
	      }
	    else
	      {
	      /*	      c_ezgfwfllw(uugdout,vvgdout, latin, lonin,
			      gridset[iset].gemout.lat_rot, gridset[iset].gemout.lon_rot, 
			      &ni,&nj, &Grille[gdid].grref,
			      &Grille[gdid].igref[IG1],&Grille[gdid].igref[IG2],
			      &Grille[gdid].igref[IG3],&Grille[gdid].igref[IG4]);
	      */

	      uvcart = (ftnfloat *) malloc(3*npts*sizeof(ftnfloat));
	      xyz    = (ftnfloat *) malloc(3*npts*sizeof(ftnfloat));
	      c_gfwfllw_turbo(uugdout,vvgdout, latin, lonin,
			      gridset[iset].gemout.lat_rot, gridset[iset].gemout.lon_rot, uvcart, xyz,
			      gridset[iset].gemout.sinlat_rot, gridset[iset].gemout.sinlon_rot, gridset[iset].gemout.coslat_rot, gridset[iset].gemout.coslon_rot,
			      gridset[iset].gemout.sinlat_true, gridset[iset].gemout.sinlon_true, gridset[iset].gemout.coslat_true, gridset[iset].gemout.coslon_true,
			      gridset[iset].gemout.r, gridset[iset].gemout.ri, &ni,&nj,
			      &Grille[gdid].grref,&Grille[gdid].igref[IG1],&Grille[gdid].igref[IG2], &Grille[gdid].igref[IG3],&Grille[gdid].igref[IG4]);
	      free(uvcart);
	      free(xyz);
	      }
	    
	    break;
	    
	  default:
	    f77name(ez_gdwfllw)(uugdout,vvgdout,lonin,&ni,&nj,&Grille[gdid].grref,
				&Grille[gdid].igref[IG1],&Grille[gdid].igref[IG2],
				&Grille[gdid].igref[IG3],&Grille[gdid].igref[IG4], 1);
	    break;
	  }
        break;
        
      default:
        f77name(ez_gdwfllw)(uugdout,vvgdout,lonin,&ni,&nj,&Grille[gdid].grtyp,
			    &Grille[gdid].ig[IG1],&Grille[gdid].ig[IG2],
			    &Grille[gdid].ig[IG3],&Grille[gdid].ig[IG4], 1);
	break;
      }
   
   return 0;
}

