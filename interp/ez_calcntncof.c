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
void ez_calcntncof(wordint gdid)
{
  wordint nni, nnj;
  
  if (Grille[gdid].flags & NEWTON)
    return;

  nni = Grille[gdid].ni;
  nnj = Grille[gdid].j2 - Grille[gdid].j1 + 1;

  if (Grille[gdid].grtyp == (char)'Y') return;
  Grille[gdid].ncx = (ftnfloat *) malloc(nni*6*sizeof(ftnfloat));
  Grille[gdid].ncy = (ftnfloat *) malloc(nnj*6*sizeof(ftnfloat));
  f77name(ez_nwtncof)(Grille[gdid].ncx,Grille[gdid].ncy,
		      Grille[gdid].ax,Grille[gdid].ay,
		      &Grille[gdid].ni, &Grille[gdid].nj,
		      &Grille[gdid].i1, &Grille[gdid].i2, 
		      &Grille[gdid].j1, &Grille[gdid].j2,
		      &Grille[gdid].extension);
  
  Grille[gdid].flags |= NEWTON;
  
  
}

