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

#include <stdlib.h>

#include <rmn/ezscint.h>
#include "ez_funcdef.h"


void f77name(ez_llwfgfw)(float *uullout, float *vvllout, float *latin, float *lonin,
                  float *xlatingf, float *xloningf, int32_t *ni,int32_t *nj,
                  char *grtyp,int32_t *ig1,int32_t *ig2,int32_t *ig3,int32_t *ig4, F2Cl lengrtyp)
{
  c_ezllwfgfw(uullout, vvllout, latin, lonin, xlatingf, xloningf, ni,nj,
    grtyp,ig1,ig2,ig3,ig4);
}


void c_ezllwfgfw(float *uullout, float *vvllout, float *latin, float *lonin,
                  float *xlatingf, float *xloningf, int32_t *ni,int32_t *nj,
                  char *grtyp,int32_t *ig1,int32_t *ig2,int32_t *ig3,int32_t *ig4)
{
  /*
    llwfgfw -> LatLon Winds From GeF Winds
    Ce sous-programme effectue la rotation des vents d'un systeme de coordonne
    tourne a un systeme de coordonnee non tourne.
    latin, lonin sont les latlons vraies
    xlatingf, xloningf sont les latlons sur la grille tournee
  */

  int32_t zero = 0;
  int32_t npts = *ni * *nj;
  int32_t trois = 3;
  float r[9], ri[9], xlon1, xlat1, xlon2, xlat2;
  float *uvcart, *xyz;
  char grtypl[2];


  uvcart = (float *) malloc(3*npts*sizeof(float));
  xyz    = (float *) malloc(3*npts*sizeof(float));


  f77name(cigaxg)(grtyp, &xlat1, &xlon1, &xlat2, &xlon2, ig1, ig2, ig3, ig4, 1);
  f77name(ez_crot)(r, ri, &xlon1, &xlat1, &xlon2, &xlat2);

  f77name(ez_uvacart)(xyz, uullout, vvllout, xloningf, xlatingf, ni, nj);
  f77name(mxm)(ri, &trois, xyz, &trois, uvcart, &npts);
  f77name(ez_cartauv)(uullout, vvllout, uvcart, lonin, latin, ni, nj);
  grtypl[0] = 'L';
  f77name(ez_llwfgdw)(uullout,vvllout,xloningf,ni,nj,grtypl, &zero, &zero, &zero, &zero, 1);

  free(uvcart);
  free(xyz);
}
