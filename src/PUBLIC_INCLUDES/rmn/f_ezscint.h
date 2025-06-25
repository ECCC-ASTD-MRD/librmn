#ifndef RMN_INTERP_F_EZSCINT_H__
#define RMN_INTERP_F_EZSCINT_H__

#include <rmn/rpnmacros.h>


void f77name(ez_aminmax)(
    float * const fldmin,
    float * const fldmax,
    const float * const fld,
    const int32_t * const ni,
    const int32_t * const nj
);
void f77name(ez_applywgts)(
    float * const outfld,
    const float * const wts,
    const int32_t * const idxs,
    const float * const infld,
    const float * const x,
    const float * const y,
    const int32_t * const masque,
    const int32_t * const ni_src,
    const int32_t * const nj_src,
    const int32_t * const ni_dst,
    const int32_t * const nj_dst,
    const int32_t * const n_wts
);
void f77name(ez_avg_sph)(
    float * const zout,
    const float * const xx,
    const float * const yy,
    const float * const lats_dst,
    const int32_t * const ni_dst,
    const int32_t * const nj_dst,
    const float * const zin,
    const int32_t * const ni_src,
    const int32_t * const nj_src,
    const int32_t * const extension
);
void f77name(ez_calcpoleval)(
    float * const poleval,
    const float * const z,
    const int32_t * const ni,
    const float * const ax,
    const char grtyp[],
    const char grref[],
    F2Cl,
    F2Cl
);
void f77name(ez_cal)(
    float * const lon,
    float * const lat,
    const float * const xyz,
    const int32_t * const n
);
void f77name(ez_calcxy_y)(
    float * const wts,
    int32_t * const idxs,
    float * const x,
    float * const y,
    const float * const gdout_lat,
    const float * const gdout_lon,
    const float * const gdin_lat,
    const float * const gdin_lon,
    int32_t * const masque,
    const int32_t * const ni_src,
    const int32_t * const nj_src,
    const int32_t * const ni_dst,
    const int32_t * const nj_dst,
    const int32_t * const num_wts

);
void f77name(ez_calcxy_y_m)(
    float * const wts,
    int32_t * const idxs,
    float * const x,
    float * const y,
    const float * const gdout_lat,
    const float * const gdout_lon,
    int32_t * const gdout_masque,
    const float * const gdin_lat,
    const float * const gdin_lon,
    const int32_t * const gdin_masque,
    const int32_t * const ni_src,
    const int32_t * const nj_src,
    const int32_t * const ni_dst,
    const int32_t * const nj_dst,
    const int32_t * const num_wts
);
void f77name(ez_cartauv)(
    float * const u,
    float * const v,
    const float * const uvcart,
    const float * const lon,
    const float * const lat,
    const int32_t * const ni,
    const int32_t * const nj
);
void f77name(ez_corrbgd)(
    float * const zout,
    const int32_t * const ni,
    const int32_t * const nj,
    const int32_t * const hem
);
void f77name(ez_crot)(
    float * const r,
    float * const ri,
    const float * const lon1,
    const float * const lat1,
    const float * const lon2,
    const float * const lat2
);
void f77name(ez_fillnpole)(
    float * const zout,
    const float * const zin,
    const int32_t * const ni,
    const int32_t * const j1,
    const int32_t * const j2,
    const float * const valpole
);
void f77name(ez_fillspole)(
    float * const zout,
    const float * const zin,
    const int32_t * const ni,
    const int32_t * const j1,
    const int32_t * const j2,
    const float * const valpole
);
void f77name(ez_gdwfllw)(
    float * const z1,
    float * const z2,
    const float * const xlon,
    const int32_t * const li,
    const int32_t * const lj,
    const char grtyp[],
    const int32_t * const ig1,
    const int32_t * const ig2,
    const int32_t * const ig3,
    const int32_t * const ig4,
    F2Cl
);
void f77name(ez_genpole)(
    float * const vpolnor,
    float * const vpolsud,
    const float * const fld,
    const int32_t * const ni,
    const int32_t * const nj,
    const int32_t * const vecteur,
    const char grtyp[],
    const int32_t * const hem,
    float * const x,
    float * const y,
    float * const z,
    float * const lat,
    float * const lon,
    float * const glat,
    const int32_t * const ordint,
    F2Cl
);
void f77name(ez_gfllfxy)(
    float * const lonp,
    float * const latp,
    const float * const lon,
    const float * const lat,
    const int32_t * const npts,
    const float * const xlat1,
    const float * const xlon1,
    const float * const xlat2,
    const float * const xlon2
);
void f77name(ez_gfxyfll)(
    float * const lonp,
    float * const latp,
    const float * const lon,
    const float * const lat,
    const int32_t * const ni,
    const float * const xlat1,
    const float * const xlon1,
    const float * const xlat2,
    const float * const xlon2
);
void f77name(ez_glat)(
    float * const latroots,
    float * const groots,
    const int32_t * const nj,
    const int32_t * const hem
);
void f77name(ez_irgdint_1_nw)(
    float * const zo,
    const float * const px,
    const float * const py,
    const int32_t * const npts,
    const float * const ax,
    const float * const ay,
    const float * const z,
    const int32_t * const ni,
    const int32_t * const nj
);
void f77name(ez_irgdint_1_w)(
    float * const zo,
    const float * const px,
    const float * const py,
    const int32_t * const npts,
    const float * const ax,
    const float * const ay,
    const float * const z,
    const int32_t * const ni,
    const int32_t * const j1,
    const int32_t * const j2,
    const int32_t * const wrap
);
void f77name(ez_irgdint_3_nw)(
    float * const zo,
    const float * const px,
    const float * const py,
    const int32_t * const npts,
    const float * const ax,
    const float * const ay,
    const float * const cx,
    const float * const cy,
    const float * const z,
    const int32_t * const i1,
    const int32_t * const i2,
    const int32_t * const j1,
    const int32_t * const j2
);
void f77name(ez_irgdint_3_w)(
    float * const zo,
    const float * const px,
    const float * const py,
    const int32_t * const npts,
    const float * const ax,
    const float * const ay,
    const float * const cx,
    const float * const cy,
    const float * const z,
    const int32_t * const ni,
    const int32_t * const j1,
    const int32_t * const j2,
    const int32_t * const wrap
);
void f77name(ez_irgdint_3_wnnc)(
    float * const zo,
    const float * const px,
    const float * const py,
    const int32_t * const npts,
    const float * const ax,
    const float * const ay,
    const float * const z,
    const int32_t * const ni,
    const int32_t * const j1,
    const int32_t * const j2,
    const int32_t * const wrap
);
void f77name(ez_lac)(
    float * const xyz,
    const float * const lon,
    const float * const lat,
    const int32_t * const nbpts
);
void f77name(ez_ll2igd)(
    float * const px,
    const float * const py,
    const float * const xlat,
    const float * const xlon,
    const int32_t * const npts,
    const int32_t * const ni,
    const int32_t * const nj,
    const char grtyp[],
    const char grref[],
    const int32_t * const ig1,
    const int32_t * const ig2,
    const int32_t * const ig3,
    const int32_t * const ig4,
    const float * const ax,
    const float * const ay,
    const int32_t * const coordflag,
    F2Cl,
    F2Cl
);
void f77name(ez_ll2rgd)(
    float * const px,
    float * const py,
    float * const xlat,
    float * const xlon,
    const int32_t * const npts,
    const int32_t * const ni,
    const int32_t * const nj,
    const char grtyp[],
    const int32_t * const ig1,
    const int32_t * const ig2,
    const int32_t * const ig3,
    const int32_t * const ig4,
    const int32_t * const sym,
    const float * const lroots,
    F2Cl
);
void f77name(ez_llflamb)(
    float * const lat,
    float * const lon,
    float * const x,
    float * const y,
    const int32_t * const npts,
    const char grtyp[],
    int32_t * const ig1,
    int32_t * const ig2,
    int32_t * const ig3,
    int32_t * const ig4,
    F2Cl
);
void f77name(ez_llwfgdw)(
    float * const z1,
    float * const z2,
    const float * const xlon,
    const int32_t * const li,
    const int32_t * const lj,
    const char grtyp[],
    const int32_t * const ig1,
    const int32_t * const ig2,
    const int32_t * const ig3,
    const int32_t * const ig4,
    F2Cl
);
void f77name(ez_nwtncof)(
    float * const cx,
    float * const cy,
    const float * const ax,
    const float * const ay,
    const int32_t * const ni,
    const int32_t * const nj,
    const int32_t * const i1,
    const int32_t * const i2,
    const int32_t * const j1,
    const int32_t * const j2,
    const int32_t * const extension
);
void f77name(ez_rgdint_0)(
    float * const zo,
    const float * const px,
    const float * const py,
    const int32_t * const npts,
    const float * const z,
    const int32_t * const ni,
    const int32_t * const j1,
    const int32_t * const j2
);
void f77name(ez_rgdint_1_nw)(
    float * const zo,
    const float * const px,
    const float * const py,
    const int32_t * const npts,
    const float * const z,
    const int32_t * const ni,
    const int32_t * const j1,
    const int32_t * const j2
);
void f77name(ez_rgdint_1_w)(
    float * const zo,
    const float * const px,
    const float * const py,
    const int32_t * const npts,
    const float * const z,
    const int32_t * const ni,
    const int32_t * const j1,
    const int32_t * const j2,
    const int32_t * const wrap
);
void f77name(ez_rgdint_3_nw)(
    float * const zo,
    const float * const px,
    const float * const py,
    const int32_t * const npts,
    const float * const z,
    const int32_t * const ni,
    const int32_t * const j1,
    const int32_t * const j2
);
void f77name(ez_rgdint_3_w)(
    float * const zo,
    const float * const px,
    const float * const py,
    const int32_t * const npts,
    const float * const z,
    const int32_t * const ni,
    const int32_t * const j1,
    const int32_t * const j2,
    const int32_t * const wrap
);
void f77name(ez_rgdint_3_wnnc)(
    float * const zo,
    const float * const px,
    const float * const py,
    const int32_t * const npts,
    const float * const z,
    const int32_t * const ni,
    const int32_t * const j1,
    const int32_t * const j2,
    const int32_t * const wrap
);
void f77name(ez_uvacart)(
    float * const xyz,
    const float * const u,
    const float * const v,
    const float * const lon,
    const float * const lat,
    const int32_t * const ni,
    const int32_t * const nj
);
void f77name(ez_vllfxy)(
    float * const latp,
    float * const lonp,
    const float * const lon,
    const float * const lat,
    const int32_t * const ni,
    const int32_t * const nj,
    const float * const d60,
    const float * const dgrw,
    const float * const pi,
    const float * const pj,
    const int32_t * const hemisphere
);
void f77name(ez_vtllfxy)(
    float * const latp,
    float * const lonp,
    float * const xp,
    float * const yp,
    const float * const clat,
    const float * const clon,
    const float * const d60,
    const float * const dgrw,
    const int32_t * const ni,
    const int32_t * const nj,
    const int32_t * const npts
);
void f77name(ez_xpngdag2)(
    float * const zout,
    const float * const zi,
    const int32_t * const ni,
    const int32_t * const nj,
    const int32_t * const j1,
    const int32_t * const j2,
    const int32_t * const hem,
    const int32_t * const symetrie
);
void f77name(ez_xpngdb2)(
    float * const zout,
    const float * const zi,
    const int32_t * const ni,
    const int32_t * const nj,
    const int32_t * const j1,
    const int32_t * const j2,
    const int32_t * const hem,
    const int32_t * const symetrie
);
void f77name(lorenzo_mask_fill)(
    float * const fld,
    const int32_t * const masque,
    const int32_t * const ni,
    const int32_t * const nj,
    const int32_t * const methode
);
void f77name(qqq_ezget_mask_zones)(
    int32_t * const mask_zones,
    const float * const x,
    const float * const y,
    const int32_t * const ni_out,
    const int32_t * const nj_out,
    const int32_t * const mask_in,
    const int32_t * const ni_in,
    const int32_t * const nj_in
);
void f77name(qqq_ezsint_mask)(
    int32_t * const mask_out,
    const float * const x,
    const float * const y,
    const int32_t * const ni_out,
    const int32_t * const nj_out,
    const int32_t * const mask_in,
    const int32_t * const ni_in,
    const int32_t * const nj_in
);

#endif // RMN_INTERP_F_EZSCINT_H__
