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
wordint f77name(ezgxprm)(wordint *gdid, wordint *ni, wordint *nj, char *grtyp,
                     wordint *ig1, wordint *ig2, wordint *ig3, wordint *ig4, 
                     char *grref, wordint *ig1ref, wordint *ig2ref, 
                     wordint *ig3ref, wordint *ig4ref,
                     wordint lengrtyp, wordint lengrref)
{
   wordint icode;
   
   ftnstrclean(grtyp,lengrtyp);
   ftnstrclean(grref,lengrtyp);
   icode = c_ezgxprm(*gdid, ni, nj, grtyp, ig1, ig2, ig3, ig4, 
                     grref, ig1ref, ig2ref, ig3ref, ig4ref);
   return icode;
}

wordint c_ezgxprm(wordint gdid, wordint *ni, wordint *nj, 
              char *grtyp, wordint *ig1, wordint *ig2, wordint *ig3, wordint *ig4,
              char *grref, wordint *ig1ref, wordint *ig2ref, wordint *ig3ref, wordint *ig4ref)
{
   *ni     = Grille[gdid].ni;
   *nj     = Grille[gdid].nj;
   *grtyp  = Grille[gdid].grtyp;
   *grref  = Grille[gdid].grref;
   *ig1    = Grille[gdid].ig[IG1];
   *ig2    = Grille[gdid].ig[IG2];
   *ig3    = Grille[gdid].ig[IG3];
   *ig4    = Grille[gdid].ig[IG4];
   *ig1ref = Grille[gdid].igref[IG1];
   *ig2ref = Grille[gdid].igref[IG2];
   *ig3ref = Grille[gdid].igref[IG3];
   *ig4ref = Grille[gdid].igref[IG4];

   return 0;
}


