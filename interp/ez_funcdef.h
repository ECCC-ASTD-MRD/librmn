#include "ezscint.h"

#ifndef _ezfuncdef

static _Grille *Grille = NULL;
static _gridset *gridset = NULL;
static _groptions groptions = { OUI, CUBIQUE,  MAXIMUM, SYM, SCALAIRE, NON, NON, OUI, 0.0 }; 
static wordint nGrilles = 0;
static wordint nsets    = 0;
static wordint iset     = -1;
/* static ftnfloat r[9],ri[9]; */

/*****************************************************************************/
wordint EliminerGrille(wordint gridid);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint LireEnrPositionnels(_Grille *gr, wordint iunit, wordint ip1, wordint ip2, wordint ip3, wordint ip4);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint c_llfgr(ftnfloat *lat, ftnfloat *lon, ftnfloat *x, ftnfloat *y, wordint npts,
        ftnfloat latOrigine, ftnfloat lonOrigine, ftnfloat deltaLat, ftnfloat deltaLon);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint ez_calclatlon(wordint gdid);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint ez_calcntncof(wordint gdid);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint ez_calcxpncof(wordint gdid);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint ez_calcxy(wordint iset);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint ez_corrval(ftnfloat *zout, ftnfloat *zin, _gridset *gdset);
wordint ez_corrvec(ftnfloat *uuout, ftnfloat *vvout, ftnfloat *uuin, ftnfloat *vvin, _gridset *gdset);
wordint ez_corrval_ausud(ftnfloat *zout, ftnfloat *zin, _gridset *gdset);
wordint ez_corrval_aunord(ftnfloat *zout, ftnfloat *zin, _gridset *gdset);
wordint ez_corrvec_aunord(ftnfloat *uuout, ftnfloat *vvout, ftnfloat *uuin, ftnfloat *vvin, _gridset *gdset);
wordint ez_corrvec_ausud(ftnfloat *uuout, ftnfloat *vvout, ftnfloat *uuin, ftnfloat *vvin, _gridset *gdset);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint ez_defzones(_gridset *gdset);/*, wordint gdin, ftnfloat *x, ftnfloat *y, wordint npts, _zone *zones);*/
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint ez_defzone_dehors(wordint gdin, ftnfloat *x, ftnfloat *y, wordint npts, _zone *zone);
wordint ez_defzone_polenord(wordint gdin, ftnfloat *x, ftnfloat *y, wordint npts, _zone *zone);
wordint ez_defzone_polesud(wordint gdin, ftnfloat *x, ftnfloat *y, wordint npts, _zone *zone);
wordint ez_defzone_nord(wordint gdin, ftnfloat *x, ftnfloat *y, wordint npts, _zone *zone);
wordint ez_defzone_sud(wordint gdin, ftnfloat *x, ftnfloat *y, wordint npts, _zone *zone);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint ez_interp(ftnfloat *zout, ftnfloat *zin, wordint iset);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint ez_poleovrw(ftnfloat *zout, wordint gdid);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint ez_xpncof(wordint *i1,wordint *i2,wordint *j1,wordint *j2,wordint *couverture,
            wordint ni,wordint nj,char grtyp, char grref,
            wordint ig1,wordint ig2,wordint ig3,wordint ig4,wordint sym,
            ftnfloat *ax, ftnfloat *ay);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint ez_xpnsrcgd(wordint gdid, ftnfloat *zout, ftnfloat *zin);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint c_ezfreegridset(wordint selectedset);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(ezdefset)(wordint *gdout, wordint *gdin);
wordint c_ezdefset(wordint gdout, wordint gdin);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(ezgdef)(wordint *ni, wordint *nj, char *grtyp, char *grref, 
                    wordint *ig1, wordint *ig2, wordint *ig3, wordint *ig4, 
                    ftnfloat *ax, ftnfloat *ay, wordint lengrtyp, wordint lengrref);
wordint c_ezgdef(wordint ni, wordint nj, char *grtyp, char *grref,
             wordint ig1, wordint ig2, wordint ig3, wordint ig4, ftnfloat *ax, ftnfloat *ay);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(ezgdef_ffile)(wordint *ni, wordint *nj, char *grtyp,
			      wordint *ig1, wordint *ig2, wordint *ig3, wordint *ig4, 
			      wordint *iunit, wordint lengrtyp);
wordint c_ezgdef_ffile(wordint ni, wordint nj, char *grtyp,
		       wordint ig1, wordint ig2, wordint ig3, wordint ig4, wordint iunit);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(ezgdef_fmem)(wordint *ni, wordint *nj, char *grtyp, char *grref, 
                    wordint *ig1, wordint *ig2, wordint *ig3, wordint *ig4, 
                    ftnfloat *ax, ftnfloat *ay, wordint lengrtyp, wordint lengrref);
wordint c_ezgdef_fmem(wordint ni, wordint nj, char *grtyp, char *grref,
             wordint ig1, wordint ig2, wordint ig3, wordint ig4, ftnfloat *ax, ftnfloat *ay);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(ezgenpole)(ftnfloat *vpolnor, ftnfloat *vpolsud, ftnfloat *fld,
                           wordint *ni, wordint *nj, wordint *vecteur, 
                           char *grtyp, wordint *hem);
wordint c_ezgenpole(ftnfloat *vpolnor, ftnfloat *vpolsud, ftnfloat *fld,
                           wordint ni, wordint nj, wordint vecteur, 
                           char *grtyp, wordint hem);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(ezgetopt)(char *option, char *value, wordint lenoption, wordint lenvalue);
wordint c_ezgetopt(char *option, char *value);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(ezgetval)(char *option, ftnfloat *value, wordint lenoption);
wordint c_ezgetval(char *option, ftnfloat *value);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(ezgfstp)(wordint *gdid, 
		     char *nomvarx, char *typvarx, char *etiketx,
		     char *nomvary, char *typvary, char *etikety,
		     wordint *ip1, wordint *ip2, wordint *ip3, wordint *dateo, 
                     wordint *deet, wordint *npas, wordint *nbits,
		     wordint lennomvarx, wordint lentypvarx, wordint lenetiketx,
		     wordint lennomvary, wordint lentypvary, wordint lenetikety);
wordint c_ezgfstp(wordint gdid, char *nomvarx, char *typvarx, char *etiketx, 
              char *nomvary, char *typvary, char *etikety,
              wordint *ip1, wordint *ip2, wordint *ip3, wordint *dateo, wordint *deet, wordint *npas, wordint *nbits);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(ezgprm)(wordint *gdid, char *grtyp, wordint *ni, wordint *nj,
             wordint *ig1, wordint *ig2, wordint *ig3, wordint *ig4);
wordint   c_ezgprm(wordint gdid, char *grtyp, wordint *ni, wordint *nj, wordint *ig1, wordint *ig2, wordint *ig3, wordint *ig4);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(ezgxprm)(wordint *gdid, wordint *ni, wordint *nj, char *grtyp,
                     wordint *ig1, wordint *ig2, wordint *ig3, wordint *ig4, 
                     char *grref, wordint *ig1ref, wordint *ig2ref, 
                     wordint *ig3ref, wordint *ig4ref,
                     wordint lengrtyp, wordint lengrref);
wordint c_ezgxprm(wordint gdid, wordint *ni, wordint *nj, 
              char *grtyp, wordint *ig1, wordint *ig2, wordint *ig3, wordint *ig4,
              char *grref, wordint *ig1ref, wordint *ig2ref, wordint *ig3ref, wordint *ig4ref);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(gdll)(wordint *gdid, ftnfloat *lat, ftnfloat *lon);
wordint c_gdll(wordint gdid, ftnfloat *lat, ftnfloat *lon);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(ezqkdef)(wordint *ni, wordint *nj, char *grtyp, 
                    wordint *ig1, wordint *ig2, wordint *ig3, wordint *ig4, wordint *iunit, wordint lengrtyp);
wordint c_ezqkdef(wordint ni, wordint nj, char *grtyp,
             wordint ig1, wordint ig2, wordint ig3, wordint ig4, wordint iunit);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(ezquickdef)(wordint *ni, wordint *nj, char *grtyp, 
			    wordint *ig1, wordint *ig2, wordint *ig3, wordint *ig4, wordint *iunit, wordint lengrtyp);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint c_ezquickdef(wordint ni, wordint nj, char *grtyp,
		     wordint ig1, wordint ig2, wordint ig3, wordint ig4, wordint iunit);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(gdrls)(wordint *gdin);
wordint c_gdrls(wordint gdin);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(ezsetopt)(char *option, char *value, wordint lenoption, wordint lenvalue);
wordint c_ezsetopt(char *option, char *value);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(ezsetval)(char *option, ftnfloat *fvalue, wordint lenoption);
wordint c_ezsetval(char *option, ftnfloat fvalue);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(ezsint)(ftnfloat *zout, ftnfloat *zin);
wordint c_ezsint(ftnfloat *zout, ftnfloat *zin);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(ezuvint)(ftnfloat *uuout, ftnfloat *vvout, ftnfloat *uuin, ftnfloat *vvin);
wordint c_ezuvint(ftnfloat *uuout, ftnfloat *vvout, ftnfloat *uuin, ftnfloat *vvin);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(ezwdint)(ftnfloat *uuout, ftnfloat *vvout, ftnfloat *uuin, ftnfloat *vvin);
wordint c_ezwdint(ftnfloat *uuout, ftnfloat *vvout, ftnfloat *uuin, ftnfloat *vvin);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint ftnstrclean(char *str, wordint lenstr);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(gdgaxes)(wordint *gdid, ftnfloat *ax, ftnfloat *ay);
wordint c_gdgaxes(wordint gdid, ftnfloat *ax, ftnfloat *ay);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(gdllfxy)(wordint *gdid, ftnfloat *lat, ftnfloat *lon, ftnfloat *x, ftnfloat *y, wordint *n);
wordint c_gdllfxy(wordint gdid, ftnfloat *lat, ftnfloat *lon, ftnfloat *x, ftnfloat *y, wordint n);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(gdllfxyz)(wordint *gdid, ftnfloat *lat, ftnfloat *lon, ftnfloat *x, ftnfloat *y, wordint *n);
wordint c_gdllfxyz(wordint gdid, ftnfloat *lat, ftnfloat *lon, ftnfloat *x, ftnfloat *y, wordint n);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(gdllsval)(wordint *gdid, ftnfloat *zout, ftnfloat *zin, ftnfloat *lat, ftnfloat *lon, wordint *n);
wordint c_gdllsval(wordint gdid, ftnfloat *zout, ftnfloat *zin, ftnfloat *lat, ftnfloat *lon, wordint n);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(gdllvval)(wordint *gdid, ftnfloat *uuout, ftnfloat *vvout, ftnfloat *uuin, ftnfloat *vvin, 
                      ftnfloat *lat, ftnfloat *lon, wordint *n);
wordint c_gdllvval(wordint gdid, ftnfloat *uuout, ftnfloat *vvout, ftnfloat *uuin, ftnfloat *vvin, 
               ftnfloat *lat, ftnfloat *lon, wordint n);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(gdllwdval)(wordint *gdid, ftnfloat *uuout, ftnfloat *vvout, ftnfloat *uuin, ftnfloat *vvin, 
                      ftnfloat *lat, ftnfloat *lon, wordint *n);
wordint c_gdllwdval(wordint gdid, ftnfloat *uuout, ftnfloat *vvout, ftnfloat *uuin, ftnfloat *vvin, 
               ftnfloat *lat, ftnfloat *lon, wordint n);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(gdxpncf)(wordint *gdin, wordint *i1, wordint *i2, wordint *j1, wordint *j2);
wordint c_gdxpncf(wordint gdin, wordint *i1, wordint *i2, wordint *j1, wordint *j2);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(gdxysval)(wordint *gdin, ftnfloat *zout, ftnfloat *zin, ftnfloat *x, ftnfloat *y, wordint *n);
wordint c_gdxysval(wordint gdin, ftnfloat *zout, ftnfloat *zin, ftnfloat *x, ftnfloat *y, wordint n);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(gdxywdval)(wordint *gdin, ftnfloat *uuout, ftnfloat *vvout, ftnfloat *uuin, ftnfloat *vvin, ftnfloat *x, ftnfloat *y, wordint *n);
wordint c_gdxywdval(wordint gdin, ftnfloat *uuout, ftnfloat *vvout, ftnfloat *uuin, ftnfloat *vvin, ftnfloat *x, ftnfloat *y, wordint n);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(gdxyvval)(wordint *gdin, ftnfloat *uuout, ftnfloat *vvout, ftnfloat *uuin, ftnfloat *vvin, ftnfloat *x, ftnfloat *y, wordint *n);
wordint c_gdxyvval(wordint gdin, ftnfloat *uuout, ftnfloat *vvout, ftnfloat *uuin, ftnfloat *vvin, ftnfloat *x, ftnfloat *y, wordint n);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(gduvfwd)(wordint *gdid, ftnfloat *uugdout, ftnfloat *vvgdout, 
                     ftnfloat *uullin, ftnfloat *vvllin, ftnfloat *latin, ftnfloat *lonin, wordint *npts);
wordint c_gduvfwd(wordint gdid,  ftnfloat *uugdout, ftnfloat *vvgdout, ftnfloat *uullin, ftnfloat *vvllin,
              ftnfloat *latin, ftnfloat *lonin, wordint npts);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(gdwdfuv)(wordint *gdid, ftnfloat *uullout, ftnfloat *vvllout, ftnfloat *uuin, ftnfloat *vvin,
              ftnfloat *latin, ftnfloat *lonin, wordint *npts);
wordint c_gdwdfuv(wordint gdid, ftnfloat *uullout, ftnfloat *vvllout, ftnfloat *uuin, ftnfloat *vvin, 
              ftnfloat *latin, ftnfloat *lonin, wordint npts);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(gdxpngd)(wordint *gdin, ftnfloat *zxpnded, ftnfloat *zin);
wordint c_gdxpngd(wordint gdin, ftnfloat *zxpnded, ftnfloat *zin);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(gdxyfll)(wordint *gdid, ftnfloat *x, ftnfloat *y, ftnfloat *lat, ftnfloat *lon, wordint *n);
wordint c_gdxyfll(wordint gdid, ftnfloat *x, ftnfloat *y, ftnfloat *lat, ftnfloat *lon, wordint n);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(gdxyzfll)(wordint *gdid, ftnfloat *x, ftnfloat *y, ftnfloat *lat, ftnfloat *lon, wordint *n);
wordint c_gdxyzfll(wordint gdid, ftnfloat *x, ftnfloat *y, ftnfloat *lat, ftnfloat *lon, wordint n);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint c_ezgetgdin();
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint c_ezgetgdout();
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wordint f77name(guval)(wordint *gdin, ftnfloat *uuout, ftnfloat *vvout, ftnfloat *uuin,  ftnfloat *vvin, 
                   ftnfloat *x, ftnfloat *y, wordint *n);
wordint c_guval(wordint gdin, ftnfloat *uuout, ftnfloat *vvout, ftnfloat *uuin,  ftnfloat *vvin, ftnfloat *x, ftnfloat *y, wordint n);
/*****************************************************************************/
wordint c_ezgfllfxy(ftnfloat *lonp, ftnfloat *latp,
                 ftnfloat *lon, ftnfloat *lat, 
                 ftnfloat *r, ftnfloat *ri, wordint *npts, 
                 ftnfloat *xlat1, ftnfloat *xlon1, ftnfloat *xlat2, ftnfloat *xlon2);
/*****************************************************************************/
wordint c_ezgfxyfll(ftnfloat *lonp, ftnfloat *latp,
                 ftnfloat *lon, ftnfloat *lat, 
                 ftnfloat *r, ftnfloat *ri, wordint *npts, 
                 ftnfloat *xlat1, ftnfloat *xlon1, ftnfloat *xlat2, ftnfloat *xlon2);
/*****************************************************************************/
wordint c_ezgfwfllw(ftnfloat *uullout, ftnfloat *vvllout, ftnfloat *latin, ftnfloat *lonin,
                  ftnfloat *xlatingf, ftnfloat *xloningf, 
                  wordint *ni, wordint *nj,
                  char *grtyp, wordint *ig1, wordint *ig2, wordint *ig3, wordint *ig4);
/*****************************************************************************/
wordint  c_ezllwfgfw(ftnfloat *uullout, ftnfloat *vvllout, ftnfloat *latin, ftnfloat *lonin,
                  ftnfloat *xlatingf, ftnfloat *xloningf, 
                 wordint *ni,wordint *nj,
                  char *grtyp,wordint *ig1,wordint *ig2,wordint *ig3,wordint *ig4);
/*****************************************************************************/
wordint c_ezgenerate_gem_cache();
/*****************************************************************************/

wordint c_ezdefxg(wordint gdid);
wordint c_ezdefaxes(wordint gdid, ftnfloat *ax, ftnfloat *ay);
wordint c_gdinterp(ftnfloat *zout, ftnfloat *zin, wordint gdin, ftnfloat *x, ftnfloat *y, wordint npts);
#endif 
#define _ezfuncdef
