#ifndef _ezscint
#define _ezscint

#include "rpnmacros.h"

int32_t f77name(ezdefset)(int32_t *gdout, int32_t *gdin);
int32_t c_ezdefset(int32_t gdout, int32_t gdin);

int32_t f77name(ezgdef)(int32_t *ni, int32_t *nj, char *grtyp, char *grref,
           int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4, float *ax, float *ay, F2Cl lengrtyp, F2Cl lengrref);
int32_t c_ezgdef(int32_t ni, int32_t nj, char *grtyp, char *grref,
           int32_t ig1, int32_t ig2, int32_t ig3, int32_t ig4, float *ax, float *ay);

int32_t f77name(ezgdef_ffile)(int32_t *ni, int32_t *nj, char *grtyp,
           int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4, int32_t *iunit, F2Cl lengrtyp);
int32_t c_ezgdef_ffile(int32_t ni, int32_t nj, char *grtyp,
           int32_t ig1, int32_t ig2, int32_t ig3, int32_t ig4, int32_t iunit);

int32_t f77name(ezgdef_fll)(int32_t *ni, int32_t *nj, float *lat, float *lon);
int32_t c_ezgdef_fll(int32_t ni, int32_t nj,float *lat, float *lon);

int32_t f77name(ezgdef_fmem)(int32_t *ni, int32_t *nj, char *grtyp, char *grref,
           int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4,float *ax, float *ay, F2Cl lengrtyp, F2Cl lengrref);
int32_t c_ezgdef_fmem(int32_t ni, int32_t nj, char *grtyp, char *grref,
           int32_t ig1, int32_t ig2, int32_t ig3, int32_t ig4, float *ax, float *ay);

int32_t f77name(ezgdef_supergrid)(int32_t *ni, int32_t *nj, char *grtyp, char *grref, int32_t *vercode, int32_t *nsubgrids, int32_t *subgrid, F2Cl lengrtyp, F2Cl lengrref);
int32_t c_ezgdef_supergrid(int32_t ni, int32_t nj, char *grtyp, char *grref, int32_t vercode, int32_t nsubgrids, int32_t *subgrid);

int32_t f77name(ezgetopt)(char *option, char *value, F2Cl lenoption, F2Cl lenvalue);
int32_t c_ezgetopt(char *option, char *value);

int32_t f77name(ezgetival)(char *option, int32_t *value, F2Cl lenoption);
int32_t c_ezgetival(char *option, int32_t *value);

int32_t f77name(ezgetval)(char *option, float *value, F2Cl lenoption);
int32_t c_ezgetval(char *option, float *value);

int32_t f77name(ezgfstp)(int32_t *gdid, char *nomvarx, char *typvarx, char *etiketx, char *nomvary, char *typvary, char *etikety,
           int32_t *ip1, int32_t *ip2, int32_t *ip3, int32_t *dateo, int32_t *deet, int32_t *npas, int32_t *nbits,
           F2Cl lennomvarx, F2Cl lentypvarx, F2Cl lenetiketx,
           F2Cl lennomvary, F2Cl lentypvary, F2Cl lenetikety);
int32_t c_ezgfstp(int32_t gdid, char *nomvarx, char *typvarx, char *etiketx, char *nomvary, char *typvary, char *etikety,
           int32_t *ip1, int32_t *ip2, int32_t *ip3, int32_t *dateo, int32_t *deet, int32_t *npas, int32_t *nbits);

int32_t f77name(ezgprm)(int32_t *gdid, char *grtyp, int32_t *ni, int32_t *nj, int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4, F2Cl lengrtyp);
int32_t c_ezgprm(int32_t gdid, char *grtyp, int32_t *ni, int32_t *nj, int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4);

int32_t f77name(ezgxprm)(int32_t *gdid, int32_t *ni, int32_t *nj, char *grtyp,
           int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4,
           char *grref, int32_t *ig1ref, int32_t *ig2ref, int32_t *ig3ref, int32_t *ig4ref,
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

int32_t f77name(ezuvint)(float *uuout, float *vvout, float *uuin, float *vvin);
int32_t c_ezuvint(float *uuout, float *vvout, float *uuin, float *vvin);

int32_t f77name(ezwdint)(float *uuout, float *vvout, float *uuin, float *vvin);
int32_t c_ezwdint(float *uuout, float *vvout, float *uuin, float *vvin);

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

int32_t f77name(gdllvval)(int32_t *gdid, float *uuout, float *vvout, float *uuin, float *vvin, float *lat, float *lon, int32_t *n);
int32_t c_gdllvval(int32_t gdid, float *uuout, float *vvout, float *uuin, float *vvin, float *lat, float *lon, int32_t n);

int32_t f77name(gdllwdval)(int32_t *gdid, float *uuout, float *vvout, float *uuin, float *vvin, float *lat, float *lon, int32_t *n);
int32_t c_gdllwdval(int32_t gdid, float *uuout, float *vvout, float *uuin, float *vvin, float *lat, float *lon, int32_t n);

int32_t f77name(gdxpncf)(int32_t *gdin, int32_t *i1, int32_t *i2, int32_t *j1, int32_t *j2);
int32_t c_gdxpncf(int32_t gdin, int32_t *i1, int32_t *i2, int32_t *j1, int32_t *j2);

int32_t f77name(gdxysval)(int32_t *gdin, float *zout, float *zin, float *x, float *y, int32_t *n);
int32_t c_gdxysval(int32_t gdin, float *zout, float *zin, float *x, float *y, int32_t n);

int32_t f77name(gdxywdval)(int32_t *gdin, float *uuout, float *vvout, float *uuin, float *vvin, float *x, float *y, int32_t *n);
int32_t c_gdxywdval(int32_t gdin, float *uuout, float *vvout, float *uuin, float *vvin, float *x, float *y, int32_t n);

int32_t f77name(gdxyvval)(int32_t *gdin, float *uuout, float *vvout, float *uuin, float *vvin, float *x, float *y, int32_t *n);
int32_t c_gdxyvval(int32_t gdin, float *uuout, float *vvout, float *uuin, float *vvin, float *x, float *y, int32_t n);

int32_t f77name(gduvfwd)(int32_t *gdid, float *uugdout, float *vvgdout, float *uullin, float *vvllin, float *latin, float *lonin, int32_t *npts);
int32_t c_gduvfwd(int32_t gdid,  float *uugdout, float *vvgdout, float *uullin, float *vvllin, float *latin, float *lonin, int32_t npts);

int32_t f77name(gdwdfuv)(int32_t *gdid, float *uullout, float *vvllout, float *uuin, float *vvin, float *latin, float *lonin, int32_t *npts);
int32_t c_gdwdfuv(int32_t gdid, float *uullout, float *vvllout, float *uuin, float *vvin, float *latin, float *lonin, int32_t npts);

int32_t f77name(gdxpngd)(int32_t *gdin, float *zxpnded, float *zin);
int32_t c_gdxpngd(int32_t gdin, float *zxpnded, float *zin);

int32_t f77name(gdxyfll)(int32_t *gdid, float *x, float *y, float *lat, float *lon, int32_t *n);
int32_t c_gdxyfll(int32_t gdid, float *x, float *y, float *lat, float *lon, int32_t n);

int32_t f77name(gdxyzfll)(int32_t *gdid, float *x, float *y, float *lat, float *lon, int32_t *n);
int32_t c_gdxyzfll(int32_t gdid, float *x, float *y, float *lat, float *lon, int32_t n);

int c_ez_refgrid(int grid_index);

int f77name(gdsetmask)(int *gdid, int *mask);
int c_gdsetmask(int gdid, int *mask);

int f77name(gdgetmask)(int *gdid, int *mask);
int c_gdgetmask(int gdid, int *mask);

int f77name(ezsint_m)(float *zout, float *zin);
int c_ezsint_m(float *zout, float *zin);

int f77name(ezuvint_m)(float *uuout, float *vvout, float *uuin, float *vvin);
int c_ezuvint_m(float *uuout, float *vvout, float *uuin, float *vvin);

int f77name(ezsint_mdm)(float *zout, int *mask_out, float *zin, int *mask_in);
int c_ezsint_mdm(float *zout, int *mask_out, float *zin, int *mask_in);

int f77name(ezuvint_mdm)(float *uuout, float *vvout, int *mask_out, float *uuin, float *vvin, int *mask_in);
int c_ezuvint_mdm(float *uuout, float *vvout, int *mask_out, float *uuin, float *vvin, int *mask_in);

int f77name(ezsint_mask)(int *mask_out, int *mask_in);
int c_ezsint_mask(int *mask_out, int *mask_in);

int f77name(ez_rgll2gd)(float *z1, float *z2, float *xlon, int32_t *ni, int32_t *nj,
                    char *grtyp, int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4,F2Cl lengrtyp);

int32_t c_ezget_nsubgrids(int32_t gdid);
int32_t f77name(ezget_nsubgrids)(int32_t *gdid);

int32_t c_ezget_subgridids(int32_t gdid, int32_t *subgrid);
int32_t f77name(ezget_subgridids)(int32_t *gdid, int32_t *subgrid);

void f77name(ez_calcdist)(float *distance, float *lat1, float *lon1, float *lat2, float *lon2);
void f77name(ez_calcdist2)(double *distance, float *lat1, float *lon1, float *lat2, float *lon2);
void f77name(c_ez_calcarea_rect)(float *area, float lat1, float lon1, float lat2, float lon2);
void f77name(ez_calcarea)(float *area, float lats[], float lons[]);
void f77name(ez_calcarea2)(float *area, float lats[], float lons[]);
void c_ez_calcdist(float *distance, float lat1, float lon1, float lat2, float lon2);
void c_ez_calcdist2(double *distance, float lat1, float lon1, float lat2, float lon2);
void c_ez_calcarea_rect(float *area,     float lat1, float lon1, float lat2, float lon2);
void c_ez_calcarea(float *area, float lats[], float lons[]);
void c_ez_calcarea2(double *area, float lats[], float lons[]);

#endif
