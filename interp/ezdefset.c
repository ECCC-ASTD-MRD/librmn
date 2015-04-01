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
wordint f77name(ezdefset)(wordint *gdout, wordint *gdin)
{
   wordint icode;
   
   icode = c_ezdefset(*gdout, *gdin);
   return icode;
}

wordint c_ezdefset(wordint gdout, wordint gdin)
{
   /* d'abord trouver si l'ensemble est deja defini */
   
   wordint i;
   static wordint found = -1;
   static wordint ncalls = 0;
   
   if (gdin >= nGrilles)
      {
      fprintf(stderr,"<ezgset>: source grid number non existent\n");
      exit(333);
      }
   
   if (gdout >= nGrilles)
      {
      fprintf(stderr,"<ezgset>: target grid number non existent\n");
      exit(333);
      }
   
   if (gridset == NULL)
    {
    gridset = malloc(sizeof(_gridset)*32);
    }
/*   fprintf(stderr,"%d %d %d\n", gdin, gdout, nsets); */
   for (i=0; i < nsets; i++)
      {
/*      fprintf(stderr,"%d %d %d %d %d\n", gdin, gdout, nsets, gridset[i].gdin, gridset[i].gdout); */
      if (gdin == gridset[i].gdin && gdout == gridset[i].gdout)
        {
        found = i;
        iset = found;
        gridset[found].gdin = gdin;
        gridset[found].gdout = gdout;
        return 1;
        }
      }
   
   /* si on se rend jusqu'ici alors c'est que le set n'a pas ete trouve */
   
   
   if (i >= nsets)
      {
      found = i;
      nsets++;
      }

   
  if (0 == (nsets % 32))
    {
    gridset = (_gridset *) realloc(gridset, sizeof(_gridset)*(nsets+32));
    }
   
   
   memset(&gridset[found], NULL, sizeof(_gridset));   
   gridset[found].gdin = gdin;
   gridset[found].gdout = gdout;
   gridset[found].use_sincos_cache = NON;
   iset = found;
   
   if (groptions.verbose > 0)
     {
     printf("iset                = %02d\n", iset);
     printf("gridset[%02d].gdin  = %d\n",   iset, gridset[iset].gdin);
     printf("gridset[%02d].gdout = %d\n",   iset, gridset[iset].gdout);
     }
   return 1;
}
