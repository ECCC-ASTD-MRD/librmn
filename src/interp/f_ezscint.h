#ifndef RMN_INTERP_F_EZSCINT_H__
#define RMN_INTERP_F_EZSCINT_H__

#include <rmn/rpnmacros.h>

void f77name(ez_aminmax)(float *fldmin, float *fldmax, float *fld, int32_t *ni, int32_t *nj);
void f77name(ez_applywgts)(float *outfld, float *wts, int32_t *idxs, float *infld, float *x, float *y, int32_t *masque,
                           int32_t *ni_src, int32_t *nj_src, int32_t *ni_dst, int32_t *nj_dst, int32_t *n_wts);
void f77name(ez_avg_sph)(float *zout, float *xx, float *yy, float *lats_dst, int32_t *ni_dst, int32_t *nj_dst,
                         float *zin, int32_t *ni_src, int32_t *nj_src, int32_t *extension);
void f77name(ez_calcpoleval)(float *poleval, float *z, int32_t *ni, float *ax,
                             char grtyp[], char grref[], F2Cl, F2Cl);
void f77name(ez_cal)(float *lon, float *lat, float *xyz, int32_t *n);
void f77name(ez_calcxy_y)(float *wts, int32_t *idxs, float *x, float *y, float *gdout_lat, float *gdout_lon,
                          float *gdin_lat, float *gdin_lon, int32_t *masque,
                          int32_t *ni_src, int32_t *nj_src, int32_t *ni_dst, int32_t *nj_dst, int32_t *num_wts);
void f77name(ez_calcxy_y_m)(float *wts, int32_t *idxs, float *x, float *y, float *gdout_lat, float *gdout_lon,
                            int32_t *gdout_masque, float *gdin_lat, float *gdin_lon, int32_t *gdin_masque,
                            int32_t *ni_src, int32_t *nj_src, int32_t *ni_dst, int32_t *nj_dst, int32_t *num_wts);
void f77name(ez_cartauv)(float *u, float *v, float *uvcart, float *lon, float *lat, int32_t *ni, int32_t *nj);
void f77name(ez_corrbgd)(float *zout, int32_t *ni, int32_t *nj, int32_t *hem);
void f77name(ez_crot)(float *r, float *ri, float *lon1, float *lat1, float *lon2, float *lat2);
void f77name(ez_fillnpole)(float *zout, float *zin, int32_t *ni, int32_t *j1, int32_t *j2, float *valpole);
void f77name(ez_fillspole)(float *zout, float *zin, int32_t *ni, int32_t *j1, int32_t *j2, float *valpole);
void f77name(ez_gdwfllw)(float *z1, float *z2, float *xlon, int32_t *li, int32_t *lj, char grtyp[],
                         int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4, F2Cl);
void f77name(ez_genpole)(float *vpolnor, float *vpolsud, float *fld, int32_t *ni, int32_t *nj,
                         int32_t *vecteur, char grtyp[], int32_t *hem,
                         float *x, float *y, float *z, float *lat, float *lon, float *glat, int32_t *ordint, F2Cl);
void f77name(ez_gfllfxy)(float *lonp, float *latp, float *lon, float *lat, int32_t *npts,
                         float *xlat1, float *xlon1, float *xlat2, float *xlon2);
void f77name(ez_gfxyfll)(float *lonp, float *latp, float *lon, float *lat, int32_t *ni,
                         float *xlat1, float *xlon1, float *xlat2, float *xlon2);
void f77name(ez_glat)(float *latroots, float *groots, int32_t *nj, int32_t *hem);
void f77name(ez_irgdint_1_nw)(float *zo, float *px, float *py, int32_t *npts, float *ax, float *ay, float *z,
                              int32_t *ni, int32_t *nj);
void f77name(ez_irgdint_1_w)(float *zo, float *px, float *py, int32_t *npts, float *ax, float *ay, float *z,
                             int32_t *ni, int32_t *j1, int32_t *j2, int32_t *wrap);
void f77name(ez_irgdint_3_nw)(float *zo, float *px, float *py, int32_t *npts, float *ax, float *ay,
                              float *cx, float *cy, float *z, int32_t *i1, int32_t *i2, int32_t *j1, int32_t *j2);
void f77name(ez_irgdint_3_w)(float *zo, float *px, float *py, int32_t *npts, float *ax, float *ay,
                             float *cx, float *cy, float *z, int32_t *ni, int32_t *j1, int32_t *j2, int32_t *wrap);
void f77name(ez_irgdint_3_wnnc)(float *zo, float *px, float *py, int32_t *npts, float *ax, float *ay, float *z,
                                int32_t *ni, int32_t *j1, int32_t *j2, int32_t *wrap);
void f77name(ez_lac)(float *xyz, float *lon, float *lat, int32_t *nbpts);
void f77name(ez_ll2igd)(float *px, float *py, float *xlat, float *xlon, int32_t *npts, int32_t *ni, int32_t *nj,
                        char grtyp[], char grref[], int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4,
                        float *ax, float *ay, int32_t *coordflag); // Missing F2Cl param?? 
void f77name(ez_ll2rgd)(float *px, float *py, float *xlat, float *xlon, int32_t *npts, int32_t *ni, int32_t *nj,
                        char grtyp[], int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4,
                        int32_t *sym, float *lroots); // Missing F2Cl param??
void f77name(ez_llflamb)(float *lat, float *lon, float *x, float *y, int32_t *npts,
                         char grtyp[][4], int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4, F2Cl);
void f77name(ez_llwfgdw)(float *z1, float *z2, float *xlon, int32_t *li, int32_t *lj, char grtyp[],
                         int32_t *ig1, int32_t *ig2, int32_t *ig3, int32_t *ig4, F2Cl);
void f77name(ez_nwtncof)(float *cx, float *cy, float *ax, float *ay, int32_t *ni, int32_t *nj,
                         int32_t *i1, int32_t *i2, int32_t *j1, int32_t *j2, int32_t *extension);
void f77name(ez_rgdint_0)(float *zo, float *px, float *py, int32_t *npts, float *z,
                          int32_t *ni, int32_t *j1, int32_t *j2);
void f77name(ez_rgdint_1_nw)(float *zo, float *px, float *py, int32_t *npts, float *z,
                             int32_t *ni, int32_t *j1, int32_t *j2);
void f77name(ez_rgdint_1_w)(float *zo, float *px, float *py, int32_t *npts, float *z,
                            int32_t *ni, int32_t *j1, int32_t *j2, int32_t *wrap);
void f77name(ez_rgdint_3_nw)(float *zo, float *px, float *py, int32_t *npts, float *z,
                             int32_t *ni, int32_t *j1, int32_t *j2);
void f77name(ez_rgdint_3_w)(float *zo, float *px, float *py, int32_t *npts, float *z,
                            int32_t *ni, int32_t *j1, int32_t *j2, int32_t *wrap);
void f77name(ez_rgdint_3_wnnc)(float *zo, float *px, float *py, int32_t *npts, float *z,
                               int32_t *ni, int32_t *j1, int32_t *j2, int32_t *wrap);
void f77name(ez_uvacart)(float *xyz, float *u, float *v, float *lon, float *lat, int32_t *ni, int32_t *nj);
void f77name(ez_vllfxy)(float *latp, float *lonp, float *lon, float *lat, int32_t *ni, int32_t *nj,
                        float *d60, float *dgrw, float *pi, float *pj, int32_t *hemisphere);
void f77name(ez_vtllfxy)(float *latp, float *lonp, float *xp, float *yp,
                        float *clat, float *clon, float *d60, float *dgrw,
                        int32_t *ni, int32_t *nj, int32_t *npts);
void f77name(ez_xpngdag2)(float *zout, float *zi, int32_t *ni, int32_t *nj, int32_t *j1, int32_t *j2,
                          int32_t *hem, int32_t *symetrie);
void f77name(ez_xpngdb2)(float *zout, float *zi, int32_t *ni, int32_t *nj, int32_t *j1, int32_t *j2,
                         int32_t *hem, int32_t *symetrie);
void f77name(lorenzo_mask_fill)(float *fld, int32_t *masque, int32_t *ni, int32_t *nj, int32_t *methode);
void f77name(qqq_ezget_mask_zones)(int32_t *mask_zones, float *x, float *y, int32_t *ni_out, int32_t *nj_out,
                                   int32_t *mask_in, int32_t *ni_in, int32_t *nj_in);
void f77name(qqq_ezsint_mask)(int32_t *mask_out, float *x, float *y, int32_t *ni_out, int32_t *nj_out,
                              int32_t *mask_in, int32_t *ni_in, int32_t *nj_in);

#endif // RMN_INTERP_F_EZSCINT_H__
