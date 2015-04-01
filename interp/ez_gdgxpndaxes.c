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
wordint f77name(gdgxpndaxes)(wordint *gdid, ftnfloat *ax, ftnfloat *ay)
{
   c_gdgxpndaxes(*gdid, ax, ay);
   return 0;
}

wordint c_gdgxpndaxes(wordint gdid, ftnfloat *ax, ftnfloat *ay)
{
  
  wordint nix, njy;
  wordint istart, jstart;
  
  if (!Grille[gdid].flags & AX)
    {
    fprintf(stderr, "(gdgxpndaxes) Erreur! A l'aide! Descripteurs manquants!\n");
    exit(999);
    }

  switch(Grille[gdid].grtyp)
    {
    case 'Y':
      nix = Grille[gdid].ni * Grille[gdid].nj;
      memcpy(ax, Grille[gdid].ax, nix*sizeof(ftnfloat));
      memcpy(ay, Grille[gdid].ay, nix*sizeof(ftnfloat));
      break;
      
    default:
      nix = Grille[gdid].ni;
      njy = Grille[gdid].nj;
      if (Grille[gdid].i2 == (nix+1)) istart = 1;
      if (Grille[gdid].i2 == (nix+2)) istart = 2;
      if (Grille[gdid].i2 == (nix)) istart = 0;

      if (Grille[gdid].j2 == (njy+1)) jstart = 1;
      if (Grille[gdid].j2 == (njy+2)) jstart = 2;
      if (Grille[gdid].j2 == (njy))   jstart = 0;
      memcpy(&ax[istart],Grille[gdid].ax, nix*sizeof(ftnfloat));
      memcpy(&ay[jstart],Grille[gdid].ay, njy*sizeof(ftnfloat));
      
      if (Grille[gdid].i2 == (Grille[gdid].ni+1))
	{
	ax[0] = Grille[gdid].ax[nix-2] - 360.0; 
	ax[nix] = ax[2];
	}
      
      if (Grille[gdid].i2 == (Grille[gdid].ni+2))
	{
	ax[0] = Grille[gdid].ax[nix-1] - 360.0; 
	ax[nix] = Grille[gdid].ax[1]+360.0;
	ax[nix+1] = Grille[gdid].ax[2]+360.0;
	}

      if (Grille[gdid].j2 == (Grille[gdid].nj+1))
	{
	}

      if (Grille[gdid].j2 == (Grille[gdid].nj+2))
	{
	}

    }
  return 0;
}
