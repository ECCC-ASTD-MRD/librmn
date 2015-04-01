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
wordint ez_corrval(ftnfloat *zout, ftnfloat *zin, _gridset *gdset)
{
  
  wordint i;
  ftnfloat valmax, valmin,fudgeval;
  wordint gdin;
  _Grille grEntree, grSortie;
  wordint degIntCourant;
  wordint npts,nj;
  ftnfloat vpolnor, vpolsud;
  ftnfloat *temp;
  
  extern ftnfloat f77name(amax)();
  extern ftnfloat f77name(amin)();
  
  grEntree = Grille[gdset->gdin];
  grSortie = Grille[gdset->gdout];
  nj = grEntree.j2 - grEntree.j1 +1;

  if (gdset->zones[DEHORS].npts > 0)
    {
    gdin = gdset->gdin;
    
    if (groptions.degre_extrap == ABORT)
      {
      fprintf(stderr, "<ez_corrval> There are points on the source grid that lie outside the source grid\n");
      fprintf(stderr, "<ez_corrval> aborting at your request!\n\n\n");
      return -1;
      }
    
    f77name(ez_aminmax)(&valmin,&valmax,zin,&grEntree.ni, &nj);
    if (groptions.degre_extrap >= MAXIMUM)
      {
      if (groptions.vecteur == VECTEUR)
	{
	fudgeval = 0.0;
	}
      else
	{
	switch (groptions.degre_extrap)
	  {
	  case MAXIMUM:
	    fudgeval = valmax + 0.05 * (valmax - valmin);
	    if (groptions.verbose > 0)
	      {
	      fprintf(stderr, "<ez_corrval>: maximum: %f \n", fudgeval);
	      }
	    break;
	    
	  case MINIMUM:
	    fudgeval = valmin - 0.05 * (valmax - valmin);
	    if (groptions.verbose > 0)
	      {
	      fprintf(stderr, "<ez_corrval>: minimum: %f \n", fudgeval);
	      }
	    break;
	    
	  case VALEUR:
	    fudgeval = groptions.valeur_extrap;
	    if (groptions.verbose > 0)
	      {
	      fprintf(stderr, "<ez_corrval>: valeur: %f \n", fudgeval);
	      }
	    break;
	  }
	}
      
      for (i=0; i < gdset->zones[DEHORS].npts; i++)
	{
	zout[gdset->zones[DEHORS].idx[i]] = fudgeval;
	}
      }
    else
      {
      degIntCourant = groptions.degre_interp;
      groptions.degre_interp = groptions.degre_extrap;
      temp = (ftnfloat *) malloc(gdset->zones[DEHORS].npts*sizeof(ftnfloat));
      
      c_gdinterp(temp, zin, gdset->gdin, gdset->zones[DEHORS].x, 
		 gdset->zones[DEHORS].y, gdset->zones[DEHORS].npts);

     for (i=0; i < gdset->zones[DEHORS].npts; i++)
	{
	zout[gdset->zones[DEHORS].idx[i]] = temp[i];
	}
      free(temp);
      groptions.degre_interp = degIntCourant;
      }
    }
  
  if (groptions.vecteur == VECTEUR)
    {
    return 0;
    }
  
  
  if (gdset->zones[AU_NORD].npts > 0)
    {
    ez_corrval_aunord(zout, zin, gdset);
    }
  
  if (gdset->zones[AU_SUD].npts > 0)
    {
    ez_corrval_ausud(zout, zin, gdset);
    }
  
  
  if (gdset->zones[POLE_NORD].npts > 0 || gdset->zones[POLE_SUD].npts > 0)
    {
    if (grEntree.grtyp != 'E')
      {
      npts = grEntree.ni * grEntree.j2;
      f77name(ez_calcpoleval)(&vpolnor, &zin[(nj-1)*grEntree.ni], &grEntree.ni, grEntree.ax, &grEntree.grtyp, &grEntree.grref);
      for (i=0; i < gdset->zones[POLE_NORD].npts; i++)
	{
	zout[gdset->zones[POLE_NORD].idx[i]] = vpolnor;
	}
      
      f77name(ez_calcpoleval)(&vpolsud, zin, &grEntree.ni, grEntree.ax, &grEntree.grtyp, &grEntree.grref);
      for (i=0; i < gdset->zones[POLE_SUD].npts; i++)
	{
	zout[gdset->zones[POLE_SUD].idx[i]] = vpolsud;
	}
      }
    }
  if ((grEntree.grtyp == 'Z' || grEntree.grtyp == '#') && grEntree.grref == 'E' && grSortie.grtyp == 'B')
    {
    f77name(ez_corrbgd)(zout, &grSortie.ni, &grSortie.nj, &grSortie.ig[IG1]);
    }

  return 0;
}
