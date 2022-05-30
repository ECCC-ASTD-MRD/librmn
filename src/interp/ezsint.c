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
#include <string.h>

#include "ez_funcdef.h"


int32_t f77name(ezsint)(float *zout, float *zin)
{
   int32_t icode;

   icode = c_ezsint(zout, zin);
   return icode;
}
int32_t c_ezsint(float *zout, float *zin)
{
  int32_t icode,gdin,gdout;
  int32_t gdrow_in,gdcol_in, gdrow_out,gdcol_out;

  if (iset_gdin == UNDEFINED || iset_gdout == UNDEFINED)
    {
    fprintf(stderr,"<c_ezsint> Source or target grid undefined! Aborting...\n");
    return -1;
    }


  gdin = iset_gdin;
  gdout= iset_gdout;

  c_gdkey2rowcol(gdin,  &gdrow_in,  &gdcol_in);
  c_gdkey2rowcol(gdout, &gdrow_out, &gdcol_out);

  if (iset_gdin == iset_gdout)
    {
    memcpy(zout, zin, Grille[gdrow_in][gdcol_in].ni*Grille[gdrow_in][gdcol_in].nj*sizeof(float));
    return 1;
    }


  if (Grille[gdrow_in][gdcol_in].nsubgrids > 0 || Grille[gdrow_out][gdcol_out].nsubgrids > 0)
      {
/* get the subgrids and interpolate accordingly */
      icode = c_ezyysint(zout,zin,gdout,gdin);
      iset_gdin=gdin;
      iset_gdout=gdout;
      return icode;
      }
  icode = c_ezsint_orig(zout, zin);
  return icode;
}

int32_t c_ezsint_orig(float *zout, float *zin)
{

    float * lzin  = NULL;
    float * lxzin = NULL;
    int32_t ierc  = 0;

    if (iset_gdin == UNDEFINED || iset_gdout == UNDEFINED) {
        fprintf(stderr,"<c_ezsint_orig> Source or target grid undefined! Aborting...\n");
        return -1;
    }

    int32_t gdin = iset_gdin;
    int32_t gdout= iset_gdout;

    int32_t gdrow_in, gdcol_in;
    int32_t gdrow_out, gdcol_out;
    c_gdkey2rowcol(gdin,  &gdrow_in,  &gdcol_in);
    c_gdkey2rowcol(gdout, &gdrow_out, &gdcol_out);

    if (iset_gdin == iset_gdout) {
        memcpy(zout, zin, Grille[gdrow_in][gdcol_in].ni*Grille[gdrow_in][gdcol_in].nj*sizeof(float));
        return 1;
    }

    if (Grille[gdrow_in][gdcol_in].fst.axe_y_inverse == 1) {
        lzin = (float *) malloc(Grille[gdrow_in][gdcol_in].ni*Grille[gdrow_in][gdcol_in].nj*sizeof(float));
        memcpy(lzin, zin, Grille[gdrow_in][gdcol_in].ni*Grille[gdrow_in][gdcol_in].nj*sizeof(float));
        f77name(permut)(lzin, &Grille[gdrow_in][gdcol_in].ni, &Grille[gdrow_in][gdcol_in].nj);
    } else {
        lzin = zin;
    }

    if (Grille[gdrow_in][gdcol_in].needs_expansion == 1) {
        lxzin = (float *) malloc(2*Grille[gdrow_in][gdcol_in].ni*Grille[gdrow_in][gdcol_in].nj*sizeof(float));
        ez_xpnsrcgd(gdin, lxzin, lzin);
    } else {
        lxzin = lzin;
    }

    ez_calclatlon(gdout);
    ez_calcxy(gdin, gdout);
    Grille[gdrow_out][gdcol_out].ni*Grille[gdrow_out][gdcol_out].nj;

    ez_interp(zout, lxzin, gdin, gdout);

    if (groptions.polar_correction == 1) {
        ez_defzones(gdin, gdout);
        ierc= ez_corrval(zout, lxzin, gdin, gdout);
    }

    if (lzin != zin && lzin != NULL) {
        free(lzin);
    }

    if (lxzin != lzin && lxzin != zin && lxzin != NULL) {
        free(lxzin);
    }

    return ierc;
}
