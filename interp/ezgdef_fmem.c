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
wordint f77name(ezgdef_fmem)(wordint *ni, wordint *nj, char *grtyp, char *grref, 
			      wordint *ig1, wordint *ig2, wordint *ig3, wordint *ig4, 
			      ftnfloat *ax, ftnfloat *ay, wordint lengrtyp, wordint lengrref)
{
  wordint icode;
  char lgrtyp[2],lgrref[2];
  
  lgrtyp[0] = grtyp[0];
  lgrtyp[1] = '\0';
  
  lgrref[0] = grref[0];
  lgrref[1] = '\0';
  
  icode = c_ezgdef_fmem(*ni, *nj, lgrtyp, lgrref, *ig1, *ig2, *ig3, *ig4, ax, ay);
  return icode;
}

wordint c_ezgdef_fmem(wordint ni, wordint nj, char *grtyp, char *grref,
		      wordint ig1, wordint ig2, wordint ig3, wordint ig4, ftnfloat *ax, ftnfloat *ay)
{
  wordint gdid;
  

  gdid = c_ezidentifygrid(ni, nj, grtyp, grref, ig1, ig2, ig3, ig4, ax, ay);
  c_ezdefxg(gdid);
  c_ezdefaxes(gdid, ax, ay);

 
  if (groptions.verbose > 0)
    {
    printf("Gdid = %02d\n", gdid);
    printf("Grille[%02d].grtyp = '%c'\n", gdid, Grille[gdid].grtyp);
    printf("Grille[%02d].ni    = %d\n",   gdid, Grille[gdid].ni);
    printf("Grille[%02d].nj    = %d\n",   gdid, Grille[gdid].nj);
    printf("Grille[%02d].ig[IG1]   = %d\n",   gdid, Grille[gdid].ig[IG1]);
    printf("Grille[%02d].ig[IG2]   = %d\n",   gdid, Grille[gdid].ig[IG2]);
    printf("Grille[%02d].ig[IG3]   = %d\n",   gdid, Grille[gdid].ig[IG3]);
    printf("Grille[%02d].ig[IG4]   = %d\n",   gdid, Grille[gdid].ig[IG4]);
    printf("Grille[%02d].grref = '%c'\n", gdid, Grille[gdid].grref);
    printf("Grille[%02d].igref[IG1]= %d\n",   gdid, Grille[gdid].igref[IG1]);
    printf("Grille[%02d].igref[IG2]= %d\n",   gdid, Grille[gdid].igref[IG2]);
    printf("Grille[%02d].igref[IG3]= %d\n",   gdid, Grille[gdid].igref[IG3]);
    printf("Grille[%02d].igref[IG4]= %d\n",   gdid, Grille[gdid].igref[IG4]);
    }
  
  return gdid;
}

