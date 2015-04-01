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

void EliminerGrille(wordint gridid)
{
   wordint i;
   if (Grille[gridid].count > 0)
    {
    Grille[gridid].count--;
    }
   
   if (Grille[gridid].count == 0)
    {
    if (Grille[gridid].flags & LAT)
        {
        free(Grille[gridid].lat);
        free(Grille[gridid].lon);
        Grille[gridid].lat = NULL;
        Grille[gridid].lon = NULL;
        }

    if (Grille[gridid].flags & AX)
        {
        free(Grille[gridid].ax);
        free(Grille[gridid].ay);
        Grille[gridid].ax = NULL;
        Grille[gridid].ay = NULL;
        }

    if (Grille[gridid].ncx != NULL)
        {
        free(Grille[gridid].ncx);
        free(Grille[gridid].ncy);
        Grille[gridid].ncx = NULL;
        Grille[gridid].ncy = NULL;
        }
    Grille[gridid].flags = NULL;
    }
   

   for (i=0; i < nsets; i++)
      {
      if (gridset[i].gdin == gridid || gridset[i].gdout == gridid)
         {
         if (gridset[i].flags & XXX)
            {
            c_ezfreegridset(i);
            gridset[i].flags = NULL;
            }
         }
      }
   
}
