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
wordint f77name(ezget_subgridmem)(wordint *gdid, wordint *subgdid, ftnfloat *zin, ftnfloat *zout)
{
   return c_ezget_subgridmem(*gdid, *subgdid, zin, zout);
}

wordint c_ezget_subgridmem(wordint gdid, wordint subgdid, ftnfloat *zin, ftnfloat *zout)
{
  wordint i,gdrow_id, gdcol_id, sub_gdrow_id,sub_gdcol_id;
  wordint nij,idx,npts;

  c_gdkey2rowcol(gdid, &gdrow_id, &gdcol_id);
  c_gdkey2rowcol(subgdid, &sub_gdrow_id, &sub_gdcol_id);
  npts=Grille[sub_gdrow_id][sub_gdcol_id].ni * Grille[sub_gdrow_id][sub_gdcol_id].nj;
  if (Grille[gdrow_id][gdcol_id].nsubgrids == 0) 
    {
    return -1;  
    }
  
  nij=0;
  for (i=0; i < Grille[gdrow_id][gdcol_id].nsubgrids; i++)
   {
   if (Grille[gdrow_id][gdcol_id].subgrid[i] == subgdid)
      {
      idx = nij;
      }
   c_gdkey2rowcol(Grille[gdrow_id][gdcol_id].subgrid[i], &sub_gdrow_id, &sub_gdcol_id);
   nij = nij+(Grille[sub_gdrow_id][sub_gdcol_id].ni * Grille[sub_gdrow_id][sub_gdcol_id].nj);
   }
  memcpy(zout,&zin[idx],npts*sizeof(ftnfloat));

  return 0;
}
