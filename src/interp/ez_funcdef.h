#ifndef _ezfuncdef
#define _ezfuncdef

#include <rmn/rpnmacros.h>

#include "ez_def.h"
#include "gd_key2rowcol.h"


// Declaration of all static objects shared by ezscint
// Definitions are in ez_statics.c
extern _Grille **Grille;
extern _Grille **gr_list;
extern int32_t nGrilles;
extern int32_t nGrillesMax;
extern int32_t cur_log_chunk;

extern __thread int32_t nsets;
extern __thread int32_t iset;
extern __thread int32_t iset_gdin;
extern __thread int32_t iset_gdout;
extern __thread _gridset *gridset;
extern  __thread _groptions groptions;

extern int32_t log_chunks[];
extern int32_t primes[];
extern int32_t chunks[];
extern int32_t primes_sq[];
extern int32_t chunks_sq[];


// Prototypes
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

void ez_xpncof(int32_t *i1,int32_t *i2,int32_t *j1,int32_t *j2,int32_t *couverture,
            int32_t ni,int32_t nj,char grtyp, char grref,
            int32_t ig1,int32_t ig2,int32_t ig3,int32_t ig4,int32_t sym,
            float *ax, float *ay);

void ez_xpnsrcgd(int32_t gdid, float *zout, float *zin);

int32_t c_ezfreegridset(int32_t gdid, int32_t index);

int32_t c_ezgdef_yymask(_Grille *gr);

//! Pas appellée dans ezscint -> Public?
int32_t f77name(ezgenpole)(float *vpolnor, float *vpolsud, float *fld,
                           int32_t *ni, int32_t *nj, int32_t *vecteur,
                           char *grtyp, int32_t *hem, F2Cl lengrtyp);
int32_t c_ezgenpole(float *vpolnor, float *vpolsud, float *fld,
                           int32_t ni, int32_t nj, int32_t vecteur,
                           char *grtyp, int32_t hem);

int32_t find_gdin_in_gset(int32_t gdin, int32_t gdout);

int32_t ftnstrclean(char *str, int32_t lenstr);

int32_t c_ezgetgdin();

int32_t c_ezgetgdout();

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

void c_ezdefxg(int32_t gdid);
void c_ezdefaxes(int32_t gdid, float *ax, float *ay);
int32_t c_gdinterp(float *zout, float *zin, int32_t gdin, float *x, float *y, int32_t npts);

int c_find_gdin(int gdin, int gdout);

int32_t c_gdllfxy_orig(int32_t gdid, float *lat, float *lon, float *x, float *y, int32_t n);

int32_t ez_calcnpolarwind(
    float *polar_uu_in,
    float *polar_vv_in,
    float *uuin,
    float *vvin,
    int32_t ni,
    int32_t nj,
    int32_t gdin
);

int32_t ez_calcspolarwind(
    float *polar_uu_in,
    float *polar_vv_in,
    float *uuin,
    float *vvin,
    int32_t ni,
    int32_t nj,
    int32_t gdin
);

int c_ez_addgrid(
    int32_t grid_index,
    _Grille *newgr
);

int c_ez_findgrid(int grid_index, _Grille *gr);

int c_ezget_mask_zones(int *mask_out, int *mask_in);

int32_t c_ezidentify_irreg_grid(
    int32_t ni,
    int32_t nj,
    char* grtyp,
    char* grref,
    int32_t ig1,
    int32_t ig2,
    int32_t ig3,
    int32_t ig4,
    float* ax,
    float* ay
);

int32_t c_ezidentify_reg_grid(
    int32_t ni,
    int32_t nj,
    char* grtyp,
    int32_t ig1,
    int32_t ig2,
    int32_t ig3,
    int32_t ig4
);

void RemplirDeBlancs(char str[], int32_t longueur);

int32_t c_gdxyfll_orig(int32_t gdid, float *x, float *y, float *lat, float *lon, int32_t n);

int32_t c_ezyysint(float *zout, float *zin,int32_t gdout,int32_t gdin);

int32_t c_ezsint_orig(float *zout, float *zin);

int32_t c_ezyyuvint(float *uuout, float *vvout, float *uuin,  float *vvin, int32_t gdout,int32_t gdin);

int32_t c_ezuvint_orig(float *uuout, float *vvout, float *uuin, float *vvin);

int32_t c_ezyywdint(float *uuout, float *vvout, float *uuin,  float *vvin, int32_t gdout,int32_t gdin);

int32_t c_ezwdint_orig(float *uuout, float *vvout, float *uuin, float *vvin);

int32_t c_ezyymint(int32_t gdout, int32_t gdin, int32_t ni, int32_t nj, float *maskout, float *dlat, float *dlon, float *yinlat, float *yinlon, int32_t *yyincount, float *yanlat, float *yanlon, int32_t *yyancount);

int32_t c_ezyy_calcxy(int32_t gdout,int32_t gdin);

int32_t c_gdxyvval_orig(int32_t gdin, float *uuout, float *vvout, float *uuin, float *vvin, float *x, float *y, int32_t n);

int32_t c_gdwdfuv_orig(int32_t gdid, float *spd_out, float *wd_out, float *uuin, float *vvin,
              float *latin, float *lonin, int32_t npts);

int32_t ez_find_gdin_in_gset(int32_t gdin, int32_t gdout);

#endif
