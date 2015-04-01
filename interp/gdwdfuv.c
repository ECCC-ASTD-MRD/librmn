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
wordint f77name(gdwdfuv)(wordint *gdid, ftnfloat *spd_out, ftnfloat *wd_out, ftnfloat *uuin, ftnfloat *vvin,
                     ftnfloat *latin, ftnfloat *lonin, wordint *npts)
{
   wordint icode;
   
   icode = c_gdwdfuv(*gdid, spd_out, wd_out, uuin, vvin,latin, lonin, *npts);
   return icode;
}

wordint c_gdwdfuv(wordint gdid, ftnfloat *spd_out, ftnfloat *wd_out, ftnfloat *uuin, ftnfloat *vvin, 
              ftnfloat *latin, ftnfloat *lonin, wordint npts)
{
  wordint j, icode, yin_gdid, yan_gdid,ni,nj ;
  ftnfloat *xyin, *xyan, *yyin, *yyan, *spdout, *wdout;
  wordint gdrow_id, gdcol_id;
  wordint yin_gdrow_id, yin_gdcol_id;
  wordint yan_gdrow_id, yan_gdcol_id;

  c_gdkey2rowcol(gdid,  &gdrow_id,  &gdcol_id);
  if (Grille[gdrow_id][gdcol_id].nsubgrids > 0 )
    {
      yin_gdid=Grille[gdrow_id][gdcol_id].subgrid[0];
      yan_gdid=Grille[gdrow_id][gdcol_id].subgrid[1];
      c_gdkey2rowcol(yin_gdid,  &yin_gdrow_id,  &yin_gdcol_id);
      c_gdkey2rowcol(yan_gdid,  &yan_gdrow_id,  &yan_gdcol_id);
      ni= Grille[yin_gdrow_id][yin_gdcol_id].ni;
      nj= Grille[yin_gdrow_id][yin_gdcol_id].nj;
      xyin = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      xyan = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      yyin = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      yyan = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      spdout = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      wdout = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      icode = c_gdxyfll_orig(yin_gdid,xyin,yyin,latin,lonin,npts);
      icode = c_gdxyfll_orig(yan_gdid,xyan,yyan,latin,lonin,npts);
      for (j=0; j < npts; j++)
        {
        if (xyin[j] > ni || xyin[j] < 0 || yyin[j] > nj || yyin[j] < 0)
         {
         /* point is no good, take from YAN eventhough it may not be good*/
         icode=c_gdwdfuv_orig(yan_gdid,&spdout[j],&wdout[j],
             uuin, vvin, &latin[j],&lonin[j],1);
         }
        else
         {
          /* check if it is inside core of YAN */
          if (xyan[j] >= Grille[yan_gdrow_id][yan_gdcol_id].mymaskgridi0 &&
              xyan[j] <= Grille[yan_gdrow_id][yan_gdcol_id].mymaskgridi1 &&
              yyan[j] >= Grille[yan_gdrow_id][yan_gdcol_id].mymaskgridj0 &&
              yyan[j] <= Grille[yan_gdrow_id][yan_gdcol_id].mymaskgridj1)
              {
              icode=c_gdwdfuv_orig(yan_gdid,&spdout[j],&wdout[j],
                              uuin[ni*nj], vvin[ni*nj], &latin[j],&lonin[j],1);
              }
          else
              /* take from YIN */
              {
              icode=c_gdwdfuv_orig(yin_gdid,&spdout[j],&wdout[j],
                               uuin, vvin, &latin[j],&lonin[j],1);
              }
         }
      spd_out[j]=spdout[j];
      wd_out[j]=wdout[j];
/*      printf("gdwdfuv %d lat %f lon %f : xyin %f, yyin %f xyan %f yyan %f spd_out %f wd_out %f \n",j,latin[j],lonin[j],xyin[j],yyin[j],xyan[j],yyan[j],spdout[j],wdout[j]); */
        }
      free(xyin);free(xyan);free(yyin);free(yyan); free(wdout);free(spdout);
     return icode;
    }
  else
    {
      icode = c_gdwdfuv_orig(gdid,spd_out,wd_out,uuin,vvin,latin,lonin,npts);
      return icode;
    }
}
wordint c_gdwdfuv_orig(wordint gdid, ftnfloat *spd_out, ftnfloat *wd_out, ftnfloat *uuin, ftnfloat *vvin, 
              ftnfloat *latin, ftnfloat *lonin, wordint npts)
{
   ftnfloat *xlatingf, *xloningf, *uvcart, *xyz;
   wordint ni, nj, use_sincos_cache;
   ftnfloat *lat_rot, *lon_rot;

  wordint gdrow_id, gdcol_id;
    
  c_gdkey2rowcol(gdid,  &gdrow_id,  &gdcol_id);
   
   ni = npts;
   nj = 1;

   memcpy(spd_out, uuin, npts*sizeof(ftnfloat));
   memcpy(wd_out, vvin, npts*sizeof(ftnfloat));

/*   
    if (iset == -1)
      {
      use_sincos_cache = NON;
      }
    else
      {
      if ([gdrow_id][gdcol_id].gset[idx_gdin].gemin.lat_true == NULL || [gdrow_id][gdcol_id].gset[idx_gdin].use_sincos_cache == NON)
        {
        use_sincos_cache = NON;
        }
      else
        {
        use_sincos_cache = OUI;
        }
      }
*/

  use_sincos_cache = NON;
   switch (Grille[gdrow_id][gdcol_id].grtyp[0])
     {
     case 'E':
       if (use_sincos_cache == NON)
        {
        lat_rot=(ftnfloat *)(malloc(npts*sizeof(ftnfloat)));
        lon_rot=(ftnfloat *)(malloc(npts*sizeof(ftnfloat)));
        f77name(ez_gfxyfll)(lonin, latin, lon_rot,lat_rot, &ni,
                            &Grille[gdrow_id][gdcol_id].fst.xg[XLAT1],&Grille[gdrow_id][gdcol_id].fst.xg[XLON1],
                            &Grille[gdrow_id][gdcol_id].fst.xg[XLAT2],&Grille[gdrow_id][gdcol_id].fst.xg[XLON2]);
        
        c_ezllwfgfw(spd_out,wd_out, latin,lonin, lat_rot,lon_rot,
                    &ni,&nj,Grille[gdrow_id][gdcol_id].grtyp,
                    &Grille[gdrow_id][gdcol_id].fst.ig[IG1],&Grille[gdrow_id][gdcol_id].fst.ig[IG2],
                    &Grille[gdrow_id][gdcol_id].fst.ig[IG3],&Grille[gdrow_id][gdcol_id].fst.ig[IG4]);
        free(lat_rot);
        free(lon_rot);
        return 0;
        }
       break;
       
     case '#':
     case 'Y':
     case 'Z':
       switch(Grille[gdrow_id][gdcol_id].grref[0])
	 {
	 case 'E':
	     lat_rot=(ftnfloat *)(malloc(npts*sizeof(ftnfloat)));
	     lon_rot=(ftnfloat *)(malloc(npts*sizeof(ftnfloat)));
	     f77name(ez_gfxyfll)(lonin,latin,lon_rot, lat_rot, &ni,
				 &Grille[gdrow_id][gdcol_id].fst.xgref[XLAT1],&Grille[gdrow_id][gdcol_id].fst.xgref[XLON1],
				 &Grille[gdrow_id][gdcol_id].fst.xgref[XLAT2],&Grille[gdrow_id][gdcol_id].fst.xgref[XLON2]);
	     
	     c_ezllwfgfw(spd_out,wd_out,latin,lonin,lat_rot, lon_rot, 
			 &ni,&nj,Grille[gdrow_id][gdcol_id].grref,
			 &Grille[gdrow_id][gdcol_id].fst.igref[IG1],&Grille[gdrow_id][gdcol_id].fst.igref[IG2],
			 &Grille[gdrow_id][gdcol_id].fst.igref[IG3],&Grille[gdrow_id][gdcol_id].fst.igref[IG4]);
	     free(lat_rot);
	     free(lon_rot);
	     return 0;
	   break;
	   

	 default:
	   f77name(ez_llwfgdw)(spd_out,wd_out,lonin,
			       &ni,&nj,
			       &Grille[gdrow_id][gdcol_id].grref,
			       &Grille[gdrow_id][gdcol_id].fst.igref[IG1],&Grille[gdrow_id][gdcol_id].fst.igref[IG2],
			       &Grille[gdrow_id][gdcol_id].fst.igref[IG3],&Grille[gdrow_id][gdcol_id].fst.igref[IG4]);
	   break;
	 }
       break;
       
     default:
       f77name(ez_llwfgdw)(spd_out,wd_out,lonin,
			   &ni,&nj,
			   &Grille[gdrow_id][gdcol_id].grtyp,
			   &Grille[gdrow_id][gdcol_id].fst.ig[IG1],&Grille[gdrow_id][gdcol_id].fst.ig[IG2],
			   &Grille[gdrow_id][gdcol_id].fst.ig[IG3],&Grille[gdrow_id][gdcol_id].fst.ig[IG4]);
       break;
     }
   
   return 0;
}
