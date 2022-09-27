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


void ez_freezones(_gridset *gdset)
{
  int32_t i;

  for (i=0; i < NZONES; i++)
    {
    if (gdset->zones[i].npts > 0)
      {
      free(gdset->zones[i].idx);
      free(gdset->zones[i].x);
      free(gdset->zones[i].y);
      gdset->zones[i].npts = 0;
      gdset->zones[i].idx  = NULL;
      gdset->zones[i].x  = NULL;
      gdset->zones[i].y = NULL;
      }
    }

}


int32_t c_gdxysval(int32_t gdin, float *zout, float *zin, float *x, float *y, int32_t n)
{
  int32_t j, icode, yin_gdid, yan_gdid,ni,nj;
  float *zoutyin, *zoutyan;
  float *tmpy;

int32_t gdrow_id, gdcol_id,yin_gdrow_id,yin_gdcol_id;

  c_gdkey2rowcol(gdin,  &gdrow_id,  &gdcol_id);

  if (Grille[gdrow_id][gdcol_id].nsubgrids > 0)
      {
      yin_gdid=Grille[gdrow_id][gdcol_id].subgrid[0];
      yan_gdid=Grille[gdrow_id][gdcol_id].subgrid[1];
      c_gdkey2rowcol(yin_gdid,  &yin_gdrow_id,  &yin_gdcol_id);
      ni = Grille[yin_gdrow_id][yin_gdcol_id].ni;
      nj = Grille[yin_gdrow_id][yin_gdcol_id].nj;
      tmpy = (float *) malloc(n*sizeof(float));
      zoutyin = (float *) malloc(n*sizeof(float));
      zoutyan = (float *) malloc(n*sizeof(float));
      for (j=0; j< n; j++)
        {
          if (y[j] > Grille[yin_gdrow_id][yin_gdcol_id].nj)
             {
             tmpy[j]=y[j]-Grille[yin_gdrow_id][yin_gdcol_id].nj;
             }
          else
             {
             tmpy[j]=y[j];
             }
        }
      icode = c_gdxysval_orig(yin_gdid,zoutyin,zin,x,tmpy,n);
      icode = c_gdxysval_orig(yan_gdid,zoutyan,&zin[ni*nj],x,tmpy,n);
      for (j=0; j < n; j++)
        {
        if (y[j] > Grille[yin_gdrow_id][yin_gdcol_id].nj)
           {
           zout[j]=zoutyan[j];
           }
        else
           {
           zout[j]=zoutyin[j];
           }
        }
      free(tmpy);
      free(zoutyan); free(zoutyin);
      return icode;
      }
   else
      {
      icode = c_gdxysval_orig(gdin,zout,zin,x,y,n);
      return icode;
      }

}


int32_t f77name(gdxysval)(int32_t *gdin, float *zout, float *zin, float *x, float *y, int32_t *n)
{
   int32_t icode;

   icode = c_gdxysval(*gdin, zout, zin, x, y, *n);
   return icode;
}


int32_t c_gdxysval_orig(int32_t gdin, float *zout, float *zin, float *x, float *y, int32_t n)
{
   int32_t ier;

   ier = c_gdxysint(zout, zin, gdin, x, y, n);

   return ier;
}
