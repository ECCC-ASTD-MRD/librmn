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
wordint f77name(gduvfwd_turbo)(wordint *gdid, ftnfloat *uugdout, ftnfloat *vvgdout, 
                         ftnfloat *uullin, ftnfloat *vvllin, ftnfloat *latin, ftnfloat *lonin, wordint *npts)
{
   wordint icode;
   
   icode = c_gduvfwd(*gdid, uugdout, vvgdout, uullin, vvllin, latin, lonin, *npts);
   return icode;
}

wordint c_gduvfwd_turbo(wordint gdid,  ftnfloat *uugdout, ftnfloat *vvgdout, ftnfloat *uullin, ftnfloat *vvllin,
              ftnfloat *latin, ftnfloat *lonin, wordint npts)
{
   ftnfloat *xlatingf, *xloningf, *xlatingf2, *xloningf2, *uvcart, *xyz;
   wordint ni, nj;

   ni = npts;
   nj = 1;

   memcpy(uugdout, uullin, npts*sizeof(ftnfloat));
   memcpy(vvgdout, vvllin, npts*sizeof(ftnfloat));
   
   switch (Grille[gdid].grtyp)
      {
      case 'E':
        if (gridset[iset].gemout.lat_true == NULL)
           {
           fprintf(stderr, "Erreur fatale routine c_gduvfwd\n");
           fprintf(stderr, "SVP Aviser la Section informatique de RPN\n");
           return -1;
           }
	/*        uvcart = (ftnfloat *) malloc(3*npts*sizeof(ftnfloat));
        xyz    = (ftnfloat *) malloc(3*npts*sizeof(ftnfloat));
        c_gfwfllw(uugdout,vvgdout, latin, lonin,
                  gridset[iset].gemout.lat, gridset[iset].gemout.lon, uvcart, xyz,
                  gridset[iset].gemout.sinlat, gridset[iset].gemout.sinlon, gridset[iset].gemout.coslat, gridset[iset].gemout.coslon,
                  gridset[iset].gemout.sinlatp, gridset[iset].gemout.sinlonp, gridset[iset].gemout.coslatp, gridset[iset].gemout.coslonp,
                  gridset[iset].gemout.r, gridset[iset].gemout.ri,
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
             if (gridset[iset].gemout.lat_true == NULL)
                {
                fprintf(stderr, "Erreur fatale routine c_gduvfwd\n");
                fprintf(stderr, "Contactez Y. Chartier au plus sacrant\n");
                return -1;
                }
	     /*             uvcart = (ftnfloat *) malloc(3*npts*sizeof(ftnfloat));
             xyz    = (ftnfloat *) malloc(3*npts*sizeof(ftnfloat));
             c_gfwfllw(uugdout,vvgdout, latin, lonin,
                       gridset[iset].gemout.lat, gridset[iset].gemout.lon, uvcart, xyz,
                       gridset[iset].gemout.sinlat, gridset[iset].gemout.sinlon, gridset[iset].gemout.coslat, gridset[iset].gemout.coslon,
                       gridset[iset].gemout.sinlatp, gridset[iset].gemout.sinlonp, gridset[iset].gemout.coslatp, gridset[iset].gemout.coslonp,
                       gridset[iset].gemout.r, gridset[iset].gemout.ri,
                       &ni,&nj,
                       &Grille[gdid].grtyp,
                       &Grille[gdid].ig[IG1],&Grille[gdid].ig[IG2],
                       &Grille[gdid].ig[IG3],&Grille[gdid].ig[IG4]);
             free(uvcart);
             free(xyz);
	     */
             
             break;

           default:
             f77name(ez_gdwfllw)(uugdout,vvgdout,lonin,
                              &ni,&nj,
                              &Grille[gdid].grref,
                              &Grille[gdid].igref[IG1],&Grille[gdid].igref[IG2],
                              &Grille[gdid].igref[IG3],&Grille[gdid].igref[IG4], 1);
             break;
           }
        break;
        
      default:
        f77name(ez_gdwfllw)(uugdout,vvgdout,lonin,&ni,&nj,
                         &Grille[gdid].grtyp,
                         &Grille[gdid].ig[IG1],&Grille[gdid].ig[IG2],
                         &Grille[gdid].ig[IG3],&Grille[gdid].ig[IG4], 1);
          break;
      }

   return 0;
}

