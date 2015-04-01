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
wordint f77name(gdxyvval)(wordint *gdin, ftnfloat *uuout, ftnfloat *vvout, ftnfloat *uuin, ftnfloat *vvin, ftnfloat *x, ftnfloat *y, wordint *n)
{
   wordint icode;

   icode = c_gdxyvval(*gdin, uuout, vvout, uuin, vvin, x, y, *n);
   return icode;
}

wordint c_gdxyvval(wordint gdin, ftnfloat *uuout, ftnfloat *vvout, ftnfloat *uuin, ftnfloat *vvin, ftnfloat *x, ftnfloat *y, wordint n)
{
  wordint ninj_out;
  wordint local_gdout,local_iset, ier;
  wordint gdout;
  _gridset tmpset;

  groptions.vecteur = VECTEUR;

/*
  memset(&tmpset, (int) NULL, sizeof(_gridset));
  gdout = NMAXGRIDS-1;
  tmpset.gdin = gdin;
  tmpset.gdout = gdout;
  tmpset.x = x;
  tmpset.y = y;


  Grille[gdout].ni = n;
  Grille[gdout].nj = 1;
  Grille[gdout].grtyp = 'L';
*/

  groptions.symmetrie = SYM;
  c_gdxysint(uuout,uuin, gdin, x, y, n);
  groptions.symmetrie = ANTISYM;
  c_gdxysint(vvout,vvin, gdin, x, y, n);
  groptions.symmetrie = SYM;

/*
 if (groptions.polar_correction == OUI)
      {
      tmpset.gdin = gdin;
      ier = ez_defzones(&tmpset);
      ez_corrvec(uuout, vvout, uuin, vvin, &tmpset);
      ez_freezones(&tmpset);
      }
*/
 groptions.vecteur = SCALAIRE;

  return 0;

}
