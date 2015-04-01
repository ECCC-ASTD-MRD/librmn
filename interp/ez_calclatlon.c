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
wordint ez_calclatlon(wordint gdid)
{
   ftnfloat xlat00, xlon00, dlat, dlon;
   wordint i,j,k,ni, nj, npts, hemisphere;
   ftnfloat *lonp, *latp, *xp, *yp;
   ftnfloat *x, *y;

   if (!(Grille[gdid].flags & LAT))
      {
      ni = Grille[gdid].ni;
      nj = Grille[gdid].nj;
      npts = ni*nj;
      
      Grille[gdid].lat = (ftnfloat *) malloc(ni*nj*sizeof(ftnfloat));
      Grille[gdid].lon = (ftnfloat *) malloc(ni*nj*sizeof(ftnfloat));
      
      switch(Grille[gdid].grtyp)
         {
         case 'A':
         case 'B':
           f77name(grll)(Grille[gdid].lat,Grille[gdid].lon,&ni,&nj,
			 &Grille[gdid].xg[SWLAT],&Grille[gdid].xg[SWLON], &Grille[gdid].xg[DLAT], &Grille[gdid].xg[DLON]);
           break;
           
         case 'E':
           dlon = 360. /(ni-1);
           dlat = 180./(nj);
           xlon00 = 0.0;
           xlat00 = -90. + 0.5*dlat;

           f77name(grll)(Grille[gdid].lat,Grille[gdid].lon,&ni,&nj,&xlat00,&xlon00,&dlat,&dlon);     
	   
           f77name(cigaxg)(&Grille[gdid].grtyp, &Grille[gdid].xg[XLAT1], &Grille[gdid].xg[XLON1], 
			   &Grille[gdid].xg[XLAT2], &Grille[gdid].xg[XLON2], 
                           &Grille[gdid].ig[IG1],  &Grille[gdid].ig[IG2], &Grille[gdid].ig[IG3], &Grille[gdid].ig[IG4]);
           latp = (ftnfloat *) malloc(ni*nj*sizeof(ftnfloat));
           lonp = (ftnfloat *) malloc(ni*nj*sizeof(ftnfloat));
           f77name(ez_gfllfxy)(lonp,latp,Grille[gdid].lon,Grille[gdid].lat,&npts, 
			       &Grille[gdid].xg[XLAT1], &Grille[gdid].xg[XLON1], &Grille[gdid].xg[XLAT2], 
			       &Grille[gdid].xg[XLON2]);
           memcpy(Grille[gdid].lat,latp,npts*sizeof(ftnfloat));
           memcpy(Grille[gdid].lon,lonp,npts*sizeof(ftnfloat));
           free(latp);
           free(lonp);
           break;
           
         case 'L':
           f77name(grll)(Grille[gdid].lat,Grille[gdid].lon,&ni,&nj,
			 &Grille[gdid].xg[SWLAT],&Grille[gdid].xg[SWLON],
			 &Grille[gdid].xg[DLAT], &Grille[gdid].xg[DLON]);
           break;
           
         case 'N':
         case 'S':
           if (Grille[gdid].grtyp == 'N')
	     {
	     hemisphere = 1;
	     }
           else
              {
              hemisphere = 2;
              }
           f77name(grps)(Grille[gdid].lat,Grille[gdid].lon,&ni,&nj,
			 &Grille[gdid].xg[PI],&Grille[gdid].xg[PJ],
			 &Grille[gdid].xg[D60], &Grille[gdid].xg[DGRW], &hemisphere);
           break;
           
	 case 'T':
	   npts = ni * nj;
	   latp = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
	   lonp = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
	   xp = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
	   yp = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
	   for (j=0; j < nj; j++)
	     {
	     for (i=0; i < ni; i++)
	       {
	       k = j*ni + i;
	       yp[k] = 1.0 * (j+1);
	       xp[k] = 1.0 * (i+1);
	       }
	     }

	   f77name(ez_vtllfxy)(latp,lonp, xp,yp, 
			    &Grille[gdid].xg[CLAT], &Grille[gdid].xg[CLON],
			    &Grille[gdid].xg[TD60],&Grille[gdid].xg[TDGRW],
			    &ni,&nj,&npts);
	   
	   memcpy(Grille[gdid].lon, lonp, ni*nj*sizeof(ftnfloat));
	   memcpy(Grille[gdid].lat, latp, ni*nj*sizeof(ftnfloat));
	   free(lonp);
	   free(latp);
	   free(xp);
	   free(yp);
	   break;
	   
         case 'Y':
           switch (Grille[gdid].grref)
	     {
	     case 'N':
	     case 'S':
	       fprintf(stderr, "<ez_calclatlon> Operation not supported - Y grid on PS Grid\n");
	       return -1;
	       break;
	       
	     case 'L':
	       memcpy(Grille[gdid].lon, Grille[gdid].ax, Grille[gdid].ni*Grille[gdid].nj*sizeof(ftnfloat));
	       memcpy(Grille[gdid].lat, Grille[gdid].ay, Grille[gdid].ni*Grille[gdid].nj*sizeof(ftnfloat));
	       for (i=0; i < Grille[gdid].ni*Grille[gdid].nj; i++)
		 {
		 if (Grille[gdid].lon[i] < 0.0)
		   {
		   Grille[gdid].lon[i] = Grille[gdid].ax[i] + 360.0;
		   }
		 }
	       break;
	       
	     case 'E':
	       fprintf(stderr, "<ez_calclatlon> Operation not supported - Y grid on E Grid\n");
	       return -1;
                break;
		
	     }
           break;
           
         case '#':
         case 'Z':
	 case 'G':
	   for (j=0; j < nj; j++)
	     {
	     for (i=0; i < ni; i++)
	       {
	       Grille[gdid].lat[C_TO_FTN(i,j,ni)] = Grille[gdid].ay[j];
	       Grille[gdid].lon[C_TO_FTN(i,j,ni)] = Grille[gdid].ax[i];
	       }
	     }
	     
	   if (Grille[gdid].grtyp == 'G' && Grille[gdid].ig[IG1] == NORD)
	     {
	     for (j=0; j < nj; j++)
	       {
	       for (i=0; i < ni; i++)
		 {
		 Grille[gdid].lat[C_TO_FTN(i,j,ni)] = Grille[gdid].ay[j+nj];
		 }
	       }
	     }
	   
           switch (Grille[gdid].grref)
	     {
	     case 'N':
	     case 'S':
	       latp = (ftnfloat *) malloc(ni*nj*sizeof(ftnfloat));
	       lonp = (ftnfloat *) malloc(ni*nj*sizeof(ftnfloat));
	       f77name(ez_vllfxy)(latp,lonp,
			       Grille[gdid].lon,Grille[gdid].lat,&ni,&nj,
			       &Grille[gdid].xgref[D60],&Grille[gdid].xgref[DGRW],
			       &Grille[gdid].xgref[PI], &Grille[gdid].xgref[PJ], &Grille[gdid].hemisphere);
	       
	       for (i=0; i < ni*nj; i++)
		 {
		 if (lonp[i] < 0.0) lonp[i] += 360.0;
		 }
	       
	       memcpy(Grille[gdid].lon, lonp, ni*nj*sizeof(ftnfloat));
	       memcpy(Grille[gdid].lat, latp, ni*nj*sizeof(ftnfloat));
	       free(lonp);
	       free(latp);
	       break;
	       
	     case 'L':
	       for (j=0; j < nj; j++)
		 {
		 for (i=0; i < ni; i++)
		   {
		   Grille[gdid].lat[C_TO_FTN(i,j,ni)] += 1.0;
		   Grille[gdid].lon[C_TO_FTN(i,j,ni)] += 1.0;
		   }
		 }
	       c_llfgr(Grille[gdid].lat, Grille[gdid].lon, Grille[gdid].lon, Grille[gdid].lat, ni*nj,
		       Grille[gdid].xgref[SWLAT],Grille[gdid].xgref[SWLON], Grille[gdid].xgref[DLAT], Grille[gdid].xgref[DLON]);
	       break;
	       
	     case 'E':
	       latp = (ftnfloat *) malloc(ni*nj*sizeof(ftnfloat));
	       lonp = (ftnfloat *) malloc(ni*nj*sizeof(ftnfloat));
	       f77name(ez_gfllfxy)(lonp,latp,Grille[gdid].lon,Grille[gdid].lat,&npts,
				   &Grille[gdid].xgref[XLAT1],&Grille[gdid].xgref[XLON1], &Grille[gdid].xgref[XLAT2], &Grille[gdid].xgref[XLON2]);
	       memcpy(Grille[gdid].lon, lonp, ni*nj*sizeof(ftnfloat));
	       memcpy(Grille[gdid].lat, latp, ni*nj*sizeof(ftnfloat));
	       free(lonp);
	       free(latp);
	       break;
	       
	     }
           break;
           
         case '!':
	   x = (ftnfloat *) malloc(ni*nj*sizeof(ftnfloat));
	   y = (ftnfloat *) malloc(ni*nj*sizeof(ftnfloat));
	   for (j=0; j < nj; j++)
	     {
	     for (i=0; i < ni; i++)
	       {
	       x[C_TO_FTN(i,j,ni)] = (ftnfloat) (i+1.0);
	       y[C_TO_FTN(i,j,ni)] = (ftnfloat) (j+1.0);
	       }
	     }
	   f77name(ez_llflamb)(Grille[gdid].lat,Grille[gdid].lon,x,y,&npts,
			       &Grille[gdid].grtyp, &Grille[gdid].ig[IG1],&Grille[gdid].ig[IG2],
			       &Grille[gdid].ig[IG3],&Grille[gdid].ig[IG4]);
	   for (i=0; i < npts; i++)
	     {
	     if (Grille[gdid].lon[i] < 0.0)
	       {
	       Grille[gdid].lon[i] += 360.0;
	       }
	     }
	   break;
         }

      switch(Grille[gdid].grtyp)
	{
	case 'G':
	case 'B':
	case 'A':
	  if (Grille[gdid].ig[IG2] == 1)
	    {
	    f77name(permut)(Grille[gdid].lat, &Grille[gdid].ni, &Grille[gdid].nj);
	    }
	  break;
	  
	default:
	  break;
	}
      
      Grille[gdid].flags |= LAT;
      }
   
   if (groptions.verbose == 2)
     {
     fprintf(stderr, "gdid: %ld\n", gdid);
     for (j=0; j < nj; j++)
       {
       for (i=0; i < ni; i++)
	 {
	 fprintf(stderr, "%ld %ld %f %f\n", i,j,Grille[gdid].lat[i], Grille[gdid].lon[i]);
	 }
       }
     }
   return 0;
   
   
}
