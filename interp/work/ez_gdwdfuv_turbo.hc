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
wordint f77name(gdwdfuv_turbo)(wordint *gdid, ftnfloat *uullout, ftnfloat *vvllout, ftnfloat *uuin, ftnfloat *vvin,
                     ftnfloat *latin, ftnfloat *lonin, wordint *npts)
{
   wordint icode;
   
   icode = c_gdwdfuv(*gdid, uullout, vvllout, uuin, vvin,latin, lonin, *npts);
   return icode;
}

wordint c_gdwdfuv_turbo(wordint gdid, ftnfloat *uullout, ftnfloat *vvllout, ftnfloat *uuin, ftnfloat *vvin, 
              ftnfloat *latin, ftnfloat *lonin, wordint npts)
{
   ftnfloat *xlatingf, *xloningf, *uvcart, *xyz;
   wordint ni, nj;

   ni = npts;
   nj = 1;

   memcpy(uullout, uuin, npts*sizeof(ftnfloat));
   memcpy(vvllout, vvin, npts*sizeof(ftnfloat));

   switch (Grille[gdid].grtyp)
      {
      case 'E':
        if (gridset[iset].gemin.lat_true == NULL)
           {
           fprintf(stderr, "Erreur fatale routine c_gdwdfuv\n");
           fprintf(stderr, "Contactez la SI de RPN le plus tôt possible\n");
           return -1;
           }
	/*
        uvcart = (ftnfloat *) malloc(3*npts*sizeof(ftnfloat));
        xyz    = (ftnfloat *) malloc(3*npts*sizeof(ftnfloat));
        c_llwfgfw(uullout,vvllout, latin, lonin,
                  gridset[iset].gemin.latp, gridset[iset].gemin.lonp, uvcart, xyz,
                  gridset[iset].gemin.sinlat, gridset[iset].gemin.sinlon, gridset[iset].gemin.coslat, gridset[iset].gemin.coslon,
                  gridset[iset].gemin.sinlatp, gridset[iset].gemin.sinlonp, gridset[iset].gemin.coslatp, gridset[iset].gemin.coslonp,
                  gridset[iset].gemin.r, gridset[iset].gemin.ri,
                  &ni,&nj,
                  &Grille[gdid].grtyp,
                  &Grille[gdid].ig[IG1],&Grille[gdid].ig[IG2],
                  &Grille[gdid].ig[IG3],&Grille[gdid].ig[IG4]);
        free(uvcart);
        free(xyz);
	*/
        break;
        
      case '#':
      case 'Y':
      case 'Z':
        switch(Grille[gdid].grref)
           {
           case 'E':
             if (gridset[iset].gemin.lat_true == NULL)
                {
                fprintf(stderr, "Erreur fatale routine c_gdwdfuv\n");
                fprintf(stderr, "Contactez Y. Chartier au plus sacrant\n");
                return -1;
                }
	     /*
             uvcart = (ftnfloat *) malloc(3*npts*sizeof(ftnfloat));
             xyz    = (ftnfloat *) malloc(3*npts*sizeof(ftnfloat));
             c_llwfgfw(uullout,vvllout, latin, lonin,
                       gridset[iset].gemin.latp, gridset[iset].gemin.lonp, uvcart, xyz,
                       gridset[iset].gemin.sinlat, gridset[iset].gemin.sinlon, gridset[iset].gemin.coslat, gridset[iset].gemin.coslon,
                       gridset[iset].gemin.sinlatp, gridset[iset].gemin.sinlonp, gridset[iset].gemin.coslatp, gridset[iset].gemin.coslonp,
                       gridset[iset].gemin.r, gridset[iset].gemin.ri,
                       &ni,&nj,
                       &Grille[gdid].grref,
                       &Grille[gdid].igref[IG1],&Grille[gdid].igref[IG2],
                       &Grille[gdid].igref[IG3],&Grille[gdid].igref[IG4]);
             free(uvcart);
             free(xyz);
	     */
             break;

           default:
             f77name(ez_llwfgdw)(uullout,vvllout,lonin,
                              &ni,&nj,
                              &Grille[gdid].grref,
                              &Grille[gdid].igref[IG1],&Grille[gdid].igref[IG2],
                              &Grille[gdid].igref[IG3],&Grille[gdid].igref[IG4]);
             break;
           }
        break;
        
      default:
        f77name(ez_llwfgdw)(uullout,vvllout,lonin,
                         &ni,&nj,
                         &Grille[gdid].grtyp,
                         &Grille[gdid].ig[IG1],&Grille[gdid].ig[IG2],
                         &Grille[gdid].ig[IG3],&Grille[gdid].ig[IG4]);
          break;
      }
   
   return 0;
}
