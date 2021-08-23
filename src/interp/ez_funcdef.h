#ifndef _ezfuncdef
#define _ezfuncdef

#include <stddef.h>
#include "ezscint.h"
#include "gd_key2rowcol.h"

static _Grille **Grille  = NULL;
static _Grille **gr_list = NULL;
static int32_t nGrilles = 0;
static int32_t nGrillesMax = CHUNK*CHUNK;
static int32_t cur_log_chunk = 7;

static __thread int32_t nsets    = 0;
static __thread int32_t iset     = -1;
static __thread int32_t iset_gdin = -1;
static __thread int32_t iset_gdout = -1;
static __thread _gridset *gridset = NULL;
static  __thread _groptions groptions = { OUI, CUBIQUE,  MAXIMUM, NON, -1, SYM, SCALAIRE, NON, NON, OUI, 16, 0, DISTANCE, NEAREST, 0.5, 3.0, 0.0  };

static int32_t log_chunks[]= {0, 1,  2, 3,    4,    5,    6,      7,     8,      9,      10,     11,        12};
static int32_t primes[]    = {0, 0,  3, 7,   13,   31,   61,    127,   251,    509,    1021,   2039,      4093};
static int32_t chunks[]    = {0, 0,  4, 8,   16,   32,   64,    128,   256,    512,    1024,   2048,      4096};
static int32_t primes_sq[] = {0, 0,  3, 61, 251, 1021, 4093,  16381, 65521, 262139, 1048573, 4194301, 16777213};
static int32_t chunks_sq[] = {0, 0, 16, 64, 256, 1024, 4096,  16384, 65536, 262144, 1048576, 4194304, 16777216};


void EliminerGrille(int32_t gridid);

void f77name(ez_avg)(float *zout, float *x, float *y, int *ni_src, int *nj_src,
            float *zin, int *ni_dst, int *nj_dst, int *extension);

int32_t LireEnrPositionnels(_Grille *gr, int32_t iunit, int32_t ip1, int32_t ip2, int32_t ip3, int32_t ip4, int32_t read);

void c_llfgr(float *lat, float *lon, float *x, float *y, int32_t npts,
        float latOrigine, float lonOrigine, float deltaLat, float deltaLon);

unsigned int ez_calc_crc(int *p, int *flen,  float *ax, float *ay, int ni, int nj);
int32_t ez_calclatlon(int32_t gdid);

void ez_calcntncof(int32_t gdid);

int32_t ez_calcxpncof(int32_t gdid);

int32_t ez_calcxy(int32_t gdin, int32_t gdout);

int32_t c_ez_check_xpndable(int32_t *extension, int32_t ni, int32_t nj, char grtyp, int32_t ig1, int32_t ig2, int32_t ig3, int32_t ig4);

int32_t ez_corrval(float *zout, float *zin,  int32_t gdin, int32_t gdout);
int32_t ez_corrvec(float *uuout, float *vvout, float *uuin, float *vvin, int32_t gdin, int32_t gdout);
int32_t ez_corrval_ausud(float *zout, float *zin,  int32_t gdin, int32_t gdout);
int32_t ez_corrval_aunord(float *zout, float *zin,  int32_t gdin, int32_t gdout);
int32_t ez_corrvec_aunord(float *uuout, float *vvout, float *uuin, float *vvin,  int32_t gdin, int32_t gdout);
int32_t ez_corrvec_ausud(float *uuout, float *vvout, float *uuin, float *vvin,  int32_t gdin, int32_t gdout);


int32_t ez_defzones(int32_t gdin, int32_t gdout);

int32_t ez_defzone_dehors(int32_t gdin, float *x, float *y, int32_t npts, _zone *zone);
int32_t ez_defzone_polenord(int32_t gdin, float *x, float *y, int32_t npts, _zone *zone);
int32_t ez_defzone_polesud(int32_t gdin, float *x, float *y, int32_t npts, _zone *zone);
int32_t ez_defzone_nord(int32_t gdin, float *x, float *y, int32_t npts, _zone *zone);
int32_t ez_defzone_sud(int32_t gdin, float *x, float *y, int32_t npts, _zone *zone);

int32_t ez_interp(float *zout, float *zin, int32_t gdin, int32_t gdout);

int32_t ez_poleovrw(float *zout, int32_t gdid);

void ez_xpncof(int32_t *i1,int32_t *i2,int32_t *j1,int32_t *j2,int32_t *couverture,
            int32_t ni,int32_t nj,char grtyp, char grref,
            int32_t ig1,int32_t ig2,int32_t ig3,int32_t ig4,int32_t sym,
            float *ax, float *ay);

void ez_xpnsrcgd(int32_t gdid, float *zout, float *zin);

int32_t c_ezfreegridset(int32_t gdid, int32_t index);

int32_t f77name(ezdefset)(int32_t *gdout, int32_t *gdin);
int32_t c_ezdefset(int32_t gdout, int32_t gdin);

int32_t f77name(ezgdef)(int32_t *ni, int32_t *nj, char *grtyp, char *grref,
                    int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4,
                    float *ax, float *ay, F2Cl lengrtyp, F2Cl lengrref);
int32_t c_ezgdef(int32_t ni, int32_t nj, char *grtyp, char *grref,
             int32_t ig1, int32_t ig2, int32_t ig3, int32_t ig4, float *ax, float *ay);

int32_t f77name(ezgdef_ffile)(int32_t *ni, int32_t *nj, char *grtyp,
            int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4,
            int32_t *iunit, F2Cl lengrtyp);
int32_t c_ezgdef_ffile(int32_t ni, int32_t nj, char *grtyp,
           int32_t ig1, int32_t ig2, int32_t ig3, int32_t ig4, int32_t iunit);

int32_t f77name(ezgdef_fll)(int32_t *ni, int32_t *nj, float *lat, float *lon);
int32_t c_ezgdef_fll(int32_t ni, int32_t nj,float *lat, float *lon);

int32_t f77name(ezgdef_fmem)(int32_t *ni, int32_t *nj, char *grtyp, char *grref,
                    int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4,
                    float *ax, float *ay, F2Cl lengrtyp, F2Cl lengrref);
int32_t c_ezgdef_fmem(int32_t ni, int32_t nj, char *grtyp, char *grref,
             int32_t ig1, int32_t ig2, int32_t ig3, int32_t ig4, float *ax, float *ay);



int32_t f77name(ezgdef_supergrid)(int32_t *ni, int32_t *nj, char *grtyp, char *grref, int32_t *vercode, int32_t *nsubgrids, int32_t *subgrid, F2Cl lengrtyp, F2Cl lengrref);

int32_t c_ezgdef_supergrid(int32_t ni, int32_t nj, char *grtyp, char *grref, int32_t vercode, int32_t nsubgrids, int32_t *subgrid);
int32_t c_ezgdef_yymask(_Grille *gr);


int32_t f77name(ezgenpole)(float *vpolnor, float *vpolsud, float *fld,
                           int32_t *ni, int32_t *nj, int32_t *vecteur,
                           char *grtyp, int32_t *hem, F2Cl lengrtyp);
int32_t c_ezgenpole(float *vpolnor, float *vpolsud, float *fld,
                           int32_t ni, int32_t nj, int32_t vecteur,
                           char *grtyp, int32_t hem);

int32_t f77name(ezgetopt)(char *option, char *value, F2Cl lenoption, F2Cl lenvalue);
int32_t c_ezgetopt(char *option, char *value);

int32_t f77name(ezgetival)(char *option, int32_t *value, F2Cl lenoption);
int32_t c_ezgetival(char *option, int32_t *value);

int32_t f77name(ezgetval)(char *option, float *value, F2Cl lenoption);
int32_t c_ezgetval(char *option, float *value);

int32_t f77name(ezgfstp)(int32_t *gdid,
         char *nomvarx, char *typvarx, char *etiketx,
         char *nomvary, char *typvary, char *etikety,
         int32_t *ip1, int32_t *ip2, int32_t *ip3, int32_t *dateo,
                     int32_t *deet, int32_t *npas, int32_t *nbits,
         F2Cl lennomvarx, F2Cl lentypvarx, F2Cl lenetiketx,
         F2Cl lennomvary, F2Cl lentypvary, F2Cl lenetikety);
int32_t c_ezgfstp(int32_t gdid, char *nomvarx, char *typvarx, char *etiketx,
              char *nomvary, char *typvary, char *etikety,
              int32_t *ip1, int32_t *ip2, int32_t *ip3, int32_t *dateo, int32_t *deet, int32_t *npas, int32_t *nbits);

int32_t f77name(ezgprm)(int32_t *gdid, char *grtyp, int32_t *ni, int32_t *nj,
             int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4, F2Cl lengrtyp);
int32_t   c_ezgprm(int32_t gdid, char *grtyp, int32_t *ni, int32_t *nj, int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4);

int32_t f77name(ezgxprm)(int32_t *gdid, int32_t *ni, int32_t *nj, char *grtyp,
                     int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4,
                     char *grref, int32_t *ig1ref, int32_t *ig2ref,
                     int32_t *ig3ref, int32_t *ig4ref,
                     F2Cl lengrtyp, F2Cl lengrref);
int32_t c_ezgxprm(int32_t gdid, int32_t *ni, int32_t *nj,
              char *grtyp, int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4,
              char *grref, int32_t *ig1ref, int32_t *ig2ref, int32_t *ig3ref, int32_t *ig4ref);

int32_t f77name(gdll)(int32_t *gdid, float *lat, float *lon);
int32_t c_gdll(int32_t gdid, float *lat, float *lon);

int32_t f77name(ezqkdef)(int32_t *ni, int32_t *nj, char *grtyp,
                    int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4, int32_t *iunit, F2Cl lengrtyp);
int32_t c_ezqkdef(int32_t ni, int32_t nj, char *grtyp,
             int32_t ig1, int32_t ig2, int32_t ig3, int32_t ig4, int32_t iunit);

int32_t f77name(ezquickdef)(int32_t *ni, int32_t *nj, char *grtyp,
          int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4, int32_t *iunit, F2Cl lengrtyp);

int32_t c_ezquickdef(int32_t ni, int32_t nj, char *grtyp,
         int32_t ig1, int32_t ig2, int32_t ig3, int32_t ig4, int32_t iunit);

int32_t f77name(gdrls)(int32_t *gdin);
int32_t c_gdrls(int32_t gdin);

int32_t f77name(ezsetopt)(char *option, char *value, F2Cl lenoption, F2Cl lenvalue);
int32_t c_ezsetopt(char *option, char *value);

int32_t f77name(ezsetival)(char *option, int32_t *ivalue, F2Cl lenoption);
int32_t c_ezsetival(char *option, int32_t ivalue);

int32_t f77name(ezsetval)(char *option, float *fvalue, F2Cl lenoption);
int32_t c_ezsetval(char *option, float fvalue);

int32_t f77name(ezsint)(float *zout, float *zin);
int32_t c_ezsint(float *zout, float *zin);

int32_t c_ez_find_gdin(int gdin, int gdout);
int32_t find_gdin_in_gset(int32_t gdin, int32_t gdout);



int32_t f77name(ezuvint)(float *uuout, float *vvout, float *uuin, float *vvin);
int32_t c_ezuvint(float *uuout, float *vvout, float *uuin, float *vvin);

int32_t f77name(ezwdint)(float *uuout, float *vvout, float *uuin, float *vvin);
int32_t c_ezwdint(float *uuout, float *vvout, float *uuin, float *vvin);

int32_t ftnstrclean(char *str, int32_t lenstr);

int32_t f77name(gdgaxes)(int32_t *gdid, float *ax, float *ay);
int32_t c_gdgaxes(int32_t gdid, float *ax, float *ay);

int32_t f77name(gdgxpndaxes)(int32_t *gdid, float *ax, float *ay);
int32_t c_gdgxpndaxes(int32_t gdid, float *ax, float *ay);

int32_t f77name(gdllfxy)(int32_t *gdid, float *lat, float *lon, float *x, float *y, int32_t *n);
int32_t c_gdllfxy(int32_t gdid, float *lat, float *lon, float *x, float *y, int32_t n);

int32_t f77name(gdllfxyz)(int32_t *gdid, float *lat, float *lon, float *x, float *y, int32_t *n);
int32_t c_gdllfxyz(int32_t gdid, float *lat, float *lon, float *x, float *y, int32_t n);

int32_t f77name(gdllsval)(int32_t *gdid, float *zout, float *zin, float *lat, float *lon, int32_t *n);
int32_t c_gdllsval(int32_t gdid, float *zout, float *zin, float *lat, float *lon, int32_t n);

int32_t f77name(gdllvval)(int32_t *gdid, float *uuout, float *vvout, float *uuin, float *vvin,
                      float *lat, float *lon, int32_t *n);
int32_t c_gdllvval(int32_t gdid, float *uuout, float *vvout, float *uuin, float *vvin,
               float *lat, float *lon, int32_t n);

int32_t f77name(gdllwdval)(int32_t *gdid, float *uuout, float *vvout, float *uuin, float *vvin,
                      float *lat, float *lon, int32_t *n);
int32_t c_gdllwdval(int32_t gdid, float *uuout, float *vvout, float *uuin, float *vvin,
               float *lat, float *lon, int32_t n);

int32_t f77name(gdxpncf)(int32_t *gdin, int32_t *i1, int32_t *i2, int32_t *j1, int32_t *j2);
int32_t c_gdxpncf(int32_t gdin, int32_t *i1, int32_t *i2, int32_t *j1, int32_t *j2);

int32_t f77name(gdxysval)(int32_t *gdin, float *zout, float *zin, float *x, float *y, int32_t *n);
int32_t c_gdxysval(int32_t gdin, float *zout, float *zin, float *x, float *y, int32_t n);

int32_t f77name(gdxywdval)(int32_t *gdin, float *uuout, float *vvout, float *uuin, float *vvin, float *x, float *y, int32_t *n);
int32_t c_gdxywdval(int32_t gdin, float *uuout, float *vvout, float *uuin, float *vvin, float *x, float *y, int32_t n);

int32_t f77name(gdxyvval)(int32_t *gdin, float *uuout, float *vvout, float *uuin, float *vvin, float *x, float *y, int32_t *n);
int32_t c_gdxyvval(int32_t gdin, float *uuout, float *vvout, float *uuin, float *vvin, float *x, float *y, int32_t n);

int32_t f77name(gduvfwd)(int32_t *gdid, float *uugdout, float *vvgdout,
                     float *uullin, float *vvllin, float *latin, float *lonin, int32_t *npts);
int32_t c_gduvfwd(int32_t gdid,  float *uugdout, float *vvgdout, float *uullin, float *vvllin,
              float *latin, float *lonin, int32_t npts);

int32_t f77name(gdwdfuv)(int32_t *gdid, float *uullout, float *vvllout, float *uuin, float *vvin,
              float *latin, float *lonin, int32_t *npts);
int32_t c_gdwdfuv(int32_t gdid, float *uullout, float *vvllout, float *uuin, float *vvin,
              float *latin, float *lonin, int32_t npts);

int32_t f77name(gdxpngd)(int32_t *gdin, float *zxpnded, float *zin);
int32_t c_gdxpngd(int32_t gdin, float *zxpnded, float *zin);

int32_t f77name(gdxyfll)(int32_t *gdid, float *x, float *y, float *lat, float *lon, int32_t *n);
int32_t c_gdxyfll(int32_t gdid, float *x, float *y, float *lat, float *lon, int32_t n);

int32_t f77name(gdxyzfll)(int32_t *gdid, float *x, float *y, float *lat, float *lon, int32_t *n);
int32_t c_gdxyzfll(int32_t gdid, float *x, float *y, float *lat, float *lon, int32_t n);

int32_t c_ezgetgdin();

int32_t c_ezgetgdout();

int32_t f77name(guval)(int32_t *gdin, float *uuout, float *vvout, float *uuin,  float *vvin,
                   float *x, float *y, int32_t *n);
int32_t c_guval(int32_t gdin, float *uuout, float *vvout, float *uuin,  float *vvin, float *x, float *y, int32_t n);

void c_ezgfllfxy(float *lonp, float *latp,
                 float *lon, float *lat,
                 float *r, float *ri, int32_t *npts,
                 float *xlat1, float *xlon1, float *xlat2, float *xlon2);

void c_ezgfxyfll(float *lonp, float *latp,
                 float *lon, float *lat,
                 float *r, float *ri, int32_t *npts,
                 float *xlat1, float *xlon1, float *xlat2, float *xlon2);

void c_ezgfwfllw(float *uullout, float *vvllout, float *latin, float *lonin,
                  float *xlatingf, float *xloningf,
                  int32_t *ni, int32_t *nj,
                  char *grtyp, int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4);

void  c_ezllwfgfw(float *uullout, float *vvllout, float *latin, float *lonin,
                  float *xlatingf, float *xloningf,
                 int32_t *ni,int32_t *nj,
                  char *grtyp,int32_t *ig1,int32_t *ig2,int32_t *ig3,int32_t *ig4);

void c_ez_manageGrillesMemory();
int c_ez_refgrid(int grid_index);



void c_ezdefxg(int32_t gdid);
void c_ezdefaxes(int32_t gdid, float *ax, float *ay);
int32_t c_gdinterp(float *zout, float *zin, int32_t gdin, float *x, float *y, int32_t npts);

int f77name(gdsetmask)(int *gdid, int *mask);
int f77name(gdgetmask)(int *gdid, int *mask);
int f77name(ezsint_m)(float *zout, float *zin);
int f77name(ezuvint_m)(float *uuout, float *vvout, float *uuin, float *vvin);
int f77name(ezsint_mdm)(float *zout, int *mask_out, float *zin, int *mask_in);
int f77name(ezuvint_mdm)(float *uuout, float *vvout, int *mask_out, float *uuin, float *vvin, int *mask_in);
int f77name(ezsint_mask)(int *mask_out, int *mask_in);

int c_gdsetmask(int gdid, int *mask);
int c_gdgetmask(int gdid, int *mask);
int c_ezsint_m(float *zout, float *zin);
int c_ezuvint_m(float *uuout, float *vvout, float *uuin, float *vvin);
int c_ezsint_mdm(float *zout, int *mask_out, float *zin, int *mask_in);
int c_ezuvint_mdm(float *uuout, float *vvout, int *mask_out, float *uuin, float *vvin, int *mask_in);
int c_ezsint_mask(int *mask_out, int *mask_in);

#endif
