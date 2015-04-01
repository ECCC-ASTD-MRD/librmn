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
wordint f77name(gdwdfuv)(wordint *gdid, ftnfloat *spd_out, ftnfloat *wd_out, ftnfloat *uuin, ftnfloat *vvin,
                     ftnfloat *latin, ftnfloat *lonin, wordint *npts)
{
   wordint icode;
   
   icode = c_gdwdfuv(*gdid, spd_out, wd_out, uuin, vvin,latin, lonin, *npts);
   return icode;
}

wordint c_gdwdfuv(wordint gdid, ftnfloat *spd_out, ftnfloat *wd_out, ftnfloat *uuin, ftnfloat *vvin, 
              ftnfloat *latin, ftnfloat *lonin, wordint npts)
{
   ftnfloat *xlatingf, *xloningf, *uvcart, *xyz;
   wordint ni, nj, use_sincos_cache;
   ftnfloat *lat_rot, *lon_rot;

   ni = npts;
   nj = 1;

   memcpy(spd_out, uuin, npts*sizeof(ftnfloat));
   memcpy(wd_out, vvin, npts*sizeof(ftnfloat));

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
        lat_rot=(ftnfloat *)(malloc(npts*sizeof(ftnfloat)));
        lon_rot=(ftnfloat *)(malloc(npts*sizeof(ftnfloat)));
        f77name(ez_gfxyfll)(lonin, latin, lon_rot,lat_rot, &ni,
                &Grille[gdid].xg[XLAT1],&Grille[gdid].xg[XLON1],
                &Grille[gdid].xg[XLAT2],&Grille[gdid].xg[XLON2]);
        
        c_ezllwfgfw(spd_out,wd_out, latin,lonin, lat_rot,lon_rot,
              &ni,&nj,&Grille[gdid].grtyp,
              &Grille[gdid].ig[IG1],&Grille[gdid].ig[IG2],
              &Grille[gdid].ig[IG3],&Grille[gdid].ig[IG4]);
        free(lat_rot);
        free(lon_rot);
        return 0;
        }
       else
        {
        /* uvcart = (ftnfloat *) malloc(3*npts*sizeof(ftnfloat));
        xyz    = (ftnfloat *) malloc(3*npts*sizeof(ftnfloat));
          c_ezllwfgfw(spd_out,wd_out, latin, lonin,
          gridset[iset].gemin.lat_rot, gridset[iset].gemin.lon_rot, 
          &ni,&nj,&Grille[gdid].grtyp,
          &Grille[gdid].ig[IG1],&Grille[gdid].ig[IG2],
          &Grille[gdid].ig[IG3],&Grille[gdid].ig[IG4]);
        */
        uvcart = (ftnfloat *) malloc(3*npts*sizeof(ftnfloat));
        xyz    = (ftnfloat *) malloc(3*npts*sizeof(ftnfloat));
        c_llwfgfw_turbo(spd_out,wd_out, latin, lonin,
            gridset[iset].gemin.lat_rot, gridset[iset].gemin.lon_rot, uvcart, xyz,
            gridset[iset].gemin.sinlat_true, gridset[iset].gemin.sinlon_true, gridset[iset].gemin.coslat_true, gridset[iset].gemin.coslon_true,
            gridset[iset].gemin.sinlat_rot, gridset[iset].gemin.sinlon_rot, gridset[iset].gemin.coslat_rot, gridset[iset].gemin.coslon_rot,
            gridset[iset].gemin.r, gridset[iset].gemin.ri, &ni,&nj,
            &Grille[gdid].grref, &Grille[gdid].igref[IG1],&Grille[gdid].igref[IG2], &Grille[gdid].igref[IG3],&Grille[gdid].igref[IG4]);
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
            lat_rot=(ftnfloat *)(malloc(npts*sizeof(ftnfloat)));
            lon_rot=(ftnfloat *)(malloc(npts*sizeof(ftnfloat)));
            f77name(ez_gfxyfll)(lonin,latin,lon_rot, lat_rot, &ni,
              &Grille[gdid].xgref[XLAT1],&Grille[gdid].xgref[XLON1],
              &Grille[gdid].xgref[XLAT2],&Grille[gdid].xgref[XLON2]);
            
            c_ezllwfgfw(spd_out,wd_out,latin,lonin,lat_rot, lon_rot, 
                &ni,&nj,&Grille[gdid].grref,
                &Grille[gdid].igref[IG1],&Grille[gdid].igref[IG2],
                &Grille[gdid].igref[IG3],&Grille[gdid].igref[IG4]);
            free(lat_rot);
            free(lon_rot);
            return 0;
            }
          else
            {
            uvcart = (ftnfloat *) malloc(3*npts*sizeof(ftnfloat));
            xyz    = (ftnfloat *) malloc(3*npts*sizeof(ftnfloat));
            c_llwfgfw_turbo(spd_out,wd_out, latin, lonin,
                gridset[iset].gemin.lat_rot, gridset[iset].gemin.lon_rot, uvcart, xyz,
                gridset[iset].gemin.sinlat_rot, gridset[iset].gemin.sinlon_rot, gridset[iset].gemin.coslat_rot, gridset[iset].gemin.coslon_rot,
                gridset[iset].gemin.sinlat_true, gridset[iset].gemin.sinlon_true, gridset[iset].gemin.coslat_true, gridset[iset].gemin.coslon_true,
                gridset[iset].gemin.r, gridset[iset].gemin.ri, &ni,&nj,
                &Grille[gdid].grref, &Grille[gdid].igref[IG1],&Grille[gdid].igref[IG2], &Grille[gdid].igref[IG3],&Grille[gdid].igref[IG4]);
            free(uvcart);
            free(xyz);
            /*       c_ezllwfgfw(spd_out,wd_out, latin, lonin,
              gridset[iset].gemin.lat_rot, gridset[iset].gemin.lon_rot, 
              &ni,&nj,&Grille[gdid].grref,
              &Grille[gdid].igref[IG1],&Grille[gdid].igref[IG2],
              &Grille[gdid].igref[IG3],&Grille[gdid].igref[IG4]);
            */
            }
          break;
     

          default:
            f77name(ez_llwfgdw)(spd_out,wd_out,lonin,
                    &ni,&nj,
                    &Grille[gdid].grref,
                    &Grille[gdid].igref[IG1],&Grille[gdid].igref[IG2],
                    &Grille[gdid].igref[IG3],&Grille[gdid].igref[IG4]);
            break;
          }
       break;
       
     default:
       f77name(ez_llwfgdw)(spd_out,wd_out,lonin,
        &ni,&nj,
        &Grille[gdid].grtyp,
        &Grille[gdid].ig[IG1],&Grille[gdid].ig[IG2],
        &Grille[gdid].ig[IG3],&Grille[gdid].ig[IG4]);
       break;
     }
   
   return 0;
}
