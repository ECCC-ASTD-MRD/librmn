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
wordint f77name(gduvfwd)(wordint *gdid, ftnfloat *uugdout, ftnfloat *vvgdout, 
                         ftnfloat *uullin, ftnfloat *vvllin, ftnfloat *latin, ftnfloat *lonin, wordint *npts)
{
   wordint icode;
   
   icode = c_gduvfwd(*gdid, uugdout, vvgdout, uullin, vvllin, latin, lonin, *npts);
   return icode;
}

wordint c_gduvfwd(wordint gdid,  ftnfloat *uugdout, ftnfloat *vvgdout, ftnfloat *uullin, ftnfloat *vvllin,
              ftnfloat *latin, ftnfloat *lonin, wordint npts)
  {
  wordint j, icode, yin_gdid, yan_gdid,maxni,maxnj ;
  ftnfloat *xyin, *xyan, *yyin, *yyan, *uu_gdout,*vv_gdout;
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
      maxni= Grille[yin_gdrow_id][yin_gdcol_id].ni;
      maxnj= Grille[yin_gdrow_id][yin_gdcol_id].nj;
      xyin = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      xyan = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      yyin = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      yyan = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      uu_gdout = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      vv_gdout = (ftnfloat *) malloc(npts*sizeof(ftnfloat));
      icode = c_gdxyfll_orig(yin_gdid,xyin,yyin,latin,lonin,npts);
      icode = c_gdxyfll_orig(yan_gdid,xyan,yyan,latin,lonin,npts);
      for (j=0; j < npts; j++)
        {
        if (xyin[j] > maxni || xyin[j] < 0 || yyin[j] > maxnj || yyin[j] < 0)
         {
         /* point is no good, take from YAN eventhough it may not be good*/
         icode=c_gduvfwd_orig(yan_gdid,&uu_gdout[j],&vv_gdout[j],
             &uullin[j], &vvllin[j], &latin[j],&lonin[j],1);
         }
        else
         {
          /* check if it is inside core of YAN */
          if (xyan[j] >= Grille[yan_gdrow_id][yan_gdcol_id].mymaskgridi0 &&
              xyan[j] <= Grille[yan_gdrow_id][yan_gdcol_id].mymaskgridi1 &&
              yyan[j] >= Grille[yan_gdrow_id][yan_gdcol_id].mymaskgridj0 &&
              yyan[j] <= Grille[yan_gdrow_id][yan_gdcol_id].mymaskgridj1)
              {
              icode=c_gduvfwd_orig(yan_gdid,&uu_gdout[j],&vv_gdout[j],
             &uullin[j], &vvllin[j], &latin[j],&lonin[j],1);
              }
          else
              /* take from YIN */
              {
              icode=c_gduvfwd_orig(yin_gdid,&uu_gdout[j],&vv_gdout[j],
                                 &uullin[j], &vvllin[j], &latin[j],&lonin[j],1);
              }
         }
         uugdout[j]=uu_gdout[j];
         vvgdout[j]=vv_gdout[j];
/*      printf("gduvfwd %d lat %f lon %f : xyin %f, yyin %f xyan %f yyan %f uugdout %f vvgdout %f uullin %f vvllin %f\n",j,latin[j],lonin[j],xyin[j],yyin[j],xyan[j],yyan[j],uugdout[j],vvgdout[j],uullin[j],vvllin[j]); */
        }
      free(xyin);free(xyan);free(yyin);free(yyan);free(uu_gdout); free(vv_gdout);
     return icode;
    }
  else
    {
      icode = c_gduvfwd_orig(gdid,uugdout,vvgdout,uullin,vvllin,latin,lonin,npts);
      return icode;
    }
}

wordint c_gduvfwd_orig(wordint gdid,  ftnfloat *uugdout, ftnfloat *vvgdout, ftnfloat *uullin, ftnfloat *vvllin,
              ftnfloat *latin, ftnfloat *lonin, wordint npts)
  {
  ftnfloat *xlatingf, *xloningf, *xlatingf2, *xloningf2, *uvcart, *xyz;
  wordint ni, nj, use_sincos_cache;
  ftnfloat *lat_true,*lon_true;
  
  wordint gdrow_id, gdcol_id;
    
  c_gdkey2rowcol(gdid,  &gdrow_id,  &gdcol_id);

  
  ni = npts;
  nj = 1;
  
  memcpy(uugdout, uullin, npts*sizeof(ftnfloat));
  memcpy(vvgdout, vvllin, npts*sizeof(ftnfloat));
  
  use_sincos_cache = NON;
  switch (Grille[gdrow_id][gdcol_id].grtyp[0])
    {
    case 'E':
    lat_true=(ftnfloat *)(malloc(npts*sizeof(ftnfloat)));
    lon_true=(ftnfloat *)(malloc(npts*sizeof(ftnfloat)));
    f77name(ez_gfxyfll)(lon_true,lat_true,lonin,latin,&ni,
                        &Grille[gdrow_id][gdcol_id].fst.xg[XLAT1],&Grille[gdrow_id][gdcol_id].fst.xg[XLON1],
                        &Grille[gdrow_id][gdcol_id].fst.xg[XLAT2],&Grille[gdrow_id][gdcol_id].fst.xg[XLON2]);
    
    c_ezgfwfllw(uugdout,vvgdout,latin,lonin,lat_true,lon_true,
                &ni,&nj,Grille[gdrow_id][gdcol_id].grtyp,
                &Grille[gdrow_id][gdcol_id].fst.ig[IG1],&Grille[gdrow_id][gdcol_id].fst.ig[IG2],
                &Grille[gdrow_id][gdcol_id].fst.ig[IG3],&Grille[gdrow_id][gdcol_id].fst.ig[IG4]);
    free(lat_true);
    free(lon_true);
    return 0;
    break;
        
        
    case '#':
    case 'Y':
    case 'Z':
    switch(Grille[gdrow_id][gdcol_id].grref[0])
      {
      case 'E':
      lat_true=(ftnfloat *)(malloc(npts*sizeof(ftnfloat)));
      lon_true=(ftnfloat *)(malloc(npts*sizeof(ftnfloat)));
      f77name(ez_gfxyfll)(lonin,latin,lon_true,lat_true,&ni,
                          &Grille[gdrow_id][gdcol_id].fst.xgref[XLAT1],&Grille[gdrow_id][gdcol_id].fst.xgref[XLON1],
                          &Grille[gdrow_id][gdcol_id].fst.xgref[XLAT2],&Grille[gdrow_id][gdcol_id].fst.xgref[XLON2]);
      
      c_ezgfwfllw(uugdout,vvgdout,latin,lonin,lat_true,lon_true,
                  &ni,&nj,Grille[gdrow_id][gdcol_id].grref,
                  &Grille[gdrow_id][gdcol_id].fst.igref[IG1],&Grille[gdrow_id][gdcol_id].fst.igref[IG2],
                  &Grille[gdrow_id][gdcol_id].fst.igref[IG3],&Grille[gdrow_id][gdcol_id].fst.igref[IG4]);
      free(lat_true);
      free(lon_true);
      return 0;
      break;
	    
      default:
      f77name(ez_gdwfllw)(uugdout,vvgdout,lonin,&ni,&nj,&Grille[gdrow_id][gdcol_id].grref,
                          &Grille[gdrow_id][gdcol_id].fst.igref[IG1],&Grille[gdrow_id][gdcol_id].fst.igref[IG2],
                          &Grille[gdrow_id][gdcol_id].fst.igref[IG3],&Grille[gdrow_id][gdcol_id].fst.igref[IG4], 1);
      break;
      }
        
    default:
    f77name(ez_gdwfllw)(uugdout,vvgdout,lonin,&ni,&nj,&Grille[gdrow_id][gdcol_id].grtyp,
                        &Grille[gdrow_id][gdcol_id].fst.ig[IG1],&Grille[gdrow_id][gdcol_id].fst.ig[IG2],
                        &Grille[gdrow_id][gdcol_id].fst.ig[IG3],&Grille[gdrow_id][gdcol_id].fst.ig[IG4], 1);
    break;
    }
   
   return 0;
}

