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

wordint c_gdinterp(ftnfloat *zout, ftnfloat *zin, wordint gdin, ftnfloat *x, ftnfloat *y, wordint npts)
{
  wordint lnpts,i;

  lnpts = npts;
  
/*  for (i=0; i < npts; i++)
    {
    printf("gdinterp: %d %f %f\n", i, x[i], y[i]);
    } */

  switch(Grille[gdin].grtyp)
    {
    case '#':
    case 'Z':
    case 'G':
      switch (groptions.degre_interp)
        {
        case VOISIN:
          f77name(ez_rgdint_0)(zout,x,y,
                  &lnpts, zin, &Grille[gdin].ni, 
                  &Grille[gdin].j1, &Grille[gdin].j2);
          break;
          
        case LINEAIRE:
          switch(Grille[gdin].extension)
            {
            case 0:
              f77name(ez_irgdint_1_nw)(zout,x, y,
                    &lnpts, Grille[gdin].ax, Grille[gdin].ay,
                    zin,&Grille[gdin].ni, &Grille[gdin].nj);
              break;
              
            case 1:
            case 2:
              f77name(ez_irgdint_1_w)(zout,x, y,
                    &lnpts, Grille[gdin].ax, Grille[gdin].ay,
                    zin,&Grille[gdin].ni, &Grille[gdin].j1, &Grille[gdin].j2, &Grille[gdin].extension);
              break;
            }
          break;
    
          case CUBIQUE:
            switch(Grille[gdin].extension)
              {
              case 0:
                f77name(ez_irgdint_3_nw)(zout, x, y,
                      &lnpts, Grille[gdin].ax, Grille[gdin].ay,
                      Grille[gdin].ncx, Grille[gdin].ncy, zin,
                      &Grille[gdin].i1, &Grille[gdin].i2,
                      &Grille[gdin].j1, &Grille[gdin].j2);
                break;
                
              case 1:
              case 2:
                f77name(ez_irgdint_3_w)(zout, x, y,
                      &lnpts, Grille[gdin].ax, Grille[gdin].ay,
                      Grille[gdin].ncx, Grille[gdin].ncy, zin,
                      &Grille[gdin].ni, &Grille[gdin].j1, &Grille[gdin].j2, 
                      &Grille[gdin].extension);
                break;
              }
            break;
          }
      
      break;
      
    default:
      switch (groptions.degre_interp)
        {
        case VOISIN:
          f77name(ez_rgdint_0)(zout,x,y,
                  &lnpts, zin, &Grille[gdin].ni, 
                  &Grille[gdin].j1, &Grille[gdin].j2);
          
          break;
          
        case LINEAIRE:
          switch(Grille[gdin].extension)
            {
            case 0:
            case 1:
              f77name(ez_rgdint_1_nw)(zout,x,y,
                    &lnpts, zin, &Grille[gdin].ni, 
                    &Grille[gdin].j1, &Grille[gdin].j2);
              break;
              
            case 2:
              f77name(ez_rgdint_1_w)(zout,x,y,
                  &lnpts, zin, &Grille[gdin].ni, 
                  &Grille[gdin].j1, &Grille[gdin].j2,
                  &Grille[gdin].extension);
            }
          break;
   
          case CUBIQUE:
            switch(Grille[gdin].extension)
              {
              case 0:
                f77name(ez_rgdint_3_nw)(zout, x, y,
                      &lnpts, zin, &Grille[gdin].ni, 
                      &Grille[gdin].j1, &Grille[gdin].j2); 
                break;
                
              case 1:
              case 2:
                f77name(ez_rgdint_3_w)(zout, x, y,
                    &lnpts, zin,  &Grille[gdin].ni, 
                    &Grille[gdin].j1, &Grille[gdin].j2,
                    &Grille[gdin].extension);
                break;
              }
            break;
          }
      break;
    }
  
}
