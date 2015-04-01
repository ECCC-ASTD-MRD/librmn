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
wordint f77name(ezll)(wordint *gdid, ftnfloat *lat, ftnfloat *lon)
{
   wordint icode;
   
   icode = c_gdll(*gdid, lat, lon);
   return icode;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(gdll)(wordint *gdid, ftnfloat *lat, ftnfloat *lon)
{
   wordint icode;
   
   icode = c_gdll(*gdid, lat, lon);
   return icode;
}

wordint c_gdll(wordint gdid, ftnfloat *lat, ftnfloat *lon)
{
   ez_calclatlon(gdid);
   if (Grille[gdid].flags & LAT)
      {
      memcpy(lon, Grille[gdid].lon, Grille[gdid].ni*Grille[gdid].nj*sizeof(ftnfloat));
      if (Grille[gdid].axe_y_inverse == 0)
         {
         memcpy(lat, Grille[gdid].lat, Grille[gdid].ni*Grille[gdid].nj*sizeof(ftnfloat));
         }
      else
         {
         f77name(permut)(Grille[gdid].lat, &Grille[gdid].ni, &Grille[gdid].nj);
         memcpy(lat, Grille[gdid].lat, Grille[gdid].ni*Grille[gdid].nj*sizeof(ftnfloat));
         f77name(permut)(Grille[gdid].lat, &Grille[gdid].ni, &Grille[gdid].nj);
         }
      }
   else
      {
      fprintf(stderr, "Erreur! A l'aide! Descripteurs manquants!\n");
      return -1;
      }
   return 0;
}
