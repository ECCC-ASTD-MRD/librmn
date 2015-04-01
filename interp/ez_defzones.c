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
wordint ez_defzones(_gridset *gdset)

{
  wordint i;
  wordint extrap;
  wordint gdin,gdout;
  
  wordint npts;
  
  if (iset >= 0)
    {
    if (gdset->flags & ZONES)
      {
      return 0;
      }
    }
  
  gdin = gdset->gdin;
  gdout = gdset->gdout;
  npts = Grille[gdout].ni * Grille[gdout].nj;
  extrap = EZ_NO_EXTRAP;
  switch (Grille[gdin].grtyp)
    {
    case 'N':
    case 'S':
    case 'L':
    case '!':
      extrap = EZ_EXTRAP;
      break;
      
    case '#':
    case 'Z':
    case 'Y':
      switch(Grille[gdin].grref)
	{
	case 'N':
	case 'S':
	case 'L':
	  extrap = EZ_EXTRAP;
	  break;
	  
	case 'E':
	  if (359.0 > (Grille[gdin].ax[Grille[gdin].ni-1] - Grille[gdin].ax[0]))
	    {
	    extrap = EZ_EXTRAP;
	    }
	  break;
	}
      break;
    }
  
  for (i=0; i < NZONES; i++)
    {
    gdset->zones[i].npts = 0;
    }
  
  switch (extrap)
    {
    case EZ_EXTRAP:
      ez_defzone_dehors(gdin, gdset->x, gdset->y, npts, &gdset->zones[DEHORS]);
      break;
      
    case EZ_NO_EXTRAP:
      ez_defzone_polenord(gdin, gdset->x, gdset->y, npts, &gdset->zones[POLE_NORD]);
      ez_defzone_polesud(gdin, gdset->x, gdset->y, npts, &gdset->zones[POLE_SUD]);
      ez_defzone_sud(gdin, gdset->x, gdset->y, npts, &gdset->zones[AU_SUD]);
      ez_defzone_nord(gdin, gdset->x, gdset->y, npts, &gdset->zones[AU_NORD]);
    }
  
  gdset->flags |= ZONES;
  return 0;
}
