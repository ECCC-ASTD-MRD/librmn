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


#include <stdio.h>
#include <stdlib.h>
#include "ez_funcdef.h"


int32_t ez_defzone_dehors(int32_t gdin, float *x, float *y, int32_t npts, _zone *zone) {
  float *tmpx, *tmpy;
  int32_t *tmpidx;
  int32_t nhits;
  int32_t i;

  int32_t offsetleft, offsetright, ix, iy;

   int32_t gdrow_in, gdcol_in;

  c_gdkey2rowcol(gdin,  &gdrow_in,  &gdcol_in);

  tmpx =   (float *) malloc(npts*sizeof(float));
  tmpy =   (float *) malloc(npts*sizeof(float));
  tmpidx = (int32_t   *) malloc(npts*sizeof(int32_t));

/*
  if (groptions.degre_interp == CUBIQUE)
    {
    offsetright = 2;
    offsetleft = 1;
    }
  else
    {
    offsetright = 0;
    offsetleft = 0;
    }
*/

  offsetright = 0;
  offsetleft = 0;
  if (groptions.verbose > 0)
    {
    fprintf(stderr, "degre_extrap: %d offset left: %d offset right: %d\n", groptions.degre_extrap, offsetleft, offsetright);
    }
  nhits = 0;
  for (i=0; i < npts; i++)
    {
    ix = (int32_t)(x[i]+0.5);
    iy = (int32_t)(y[i]+0.5);
    if (ix < (1+offsetleft) || iy < (1+offsetleft) || ix > (Grille[gdrow_in][gdcol_in].ni-offsetright) || iy > (Grille[gdrow_in][gdcol_in].nj-offsetright))
      {
      tmpx[nhits]  = x[i];
      tmpy[nhits]  = y[i];
      tmpidx[nhits]=i;
      nhits++;
      }
    }

  if (nhits > 0)
    {
    zone->npts = nhits;
    zone->x =   (float *) malloc(zone->npts*sizeof(float));
    zone->y =   (float *) malloc(zone->npts*sizeof(float));
    zone->idx = (int32_t *) malloc(zone->npts*sizeof(int32_t));
    if (groptions.verbose > 0)
      {
      fprintf(stderr, "Nombre de points dehors: %d\n", zone->npts);
      }
    for (i=0; i < zone->npts; i++)
      {
      zone->x[i] = tmpx[i];
      zone->y[i] = tmpy[i];
      zone->idx[i] = tmpidx[i];
      }
    }

  free(tmpx);
  free(tmpy);
  free(tmpidx);

  return 0;
}
