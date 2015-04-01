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
wordint f77name(gdgaxes)(wordint *gdid, ftnfloat *ax, ftnfloat *ay)
{
   c_gdgaxes(*gdid, ax, ay);
   return 0;
}

wordint c_gdgaxes(wordint gdid, ftnfloat *ax, ftnfloat *ay)
{

   wordint nix, njy;

   switch(Grille[gdid].grtyp)
      {
      case 'Y':
        nix = Grille[gdid].ni * Grille[gdid].nj;
        njy = nix;
        break;

      default:
        nix = Grille[gdid].ni;
        njy = Grille[gdid].nj;
        break;
      }
   
   if (Grille[gdid].flags & AX)
      {
      memcpy(ax, Grille[gdid].ax, nix*sizeof(ftnfloat));
      memcpy(ay, Grille[gdid].ay, njy*sizeof(ftnfloat));
      }
   else
      {
      fprintf(stderr, "(gdgaxes) Erreur! A l'aide! Descripteurs manquants!\n");
      exit(999);
      }
   return 0;
}
