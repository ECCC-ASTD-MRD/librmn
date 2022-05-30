#ifndef _EZSCINT
#define _EZSCINT

#include <stdint.h>

#define NMAXGRIDS 32
#define NMAXSETS  NMAXGRIDS * (NMAXGRIDS - 1)
#define NMAXSUBGRIDS 20

#define LAT                         1
#define AX                          2
#define XXX                         4
#define SYMMETRIQUE                64
#define TYPE_EXPANSION            128
#define NEWTON                    256
#define LATLON_PRIME_OK           512
#define SINLATLON_OK             1024
#define ZONES                    2048

#define GRID_GRAPE               1024
#define GRID_CHUNK               1024

#define GRID                      0
#define CLOUD                     1

#define SCALAIRE                  0
#define VECTEUR                   1

#define GLOBALE                   0
#define LOCALE                    1

#define VOISIN                    0
#define NEAREST                   0
#define LINEAIRE                  1
#define LINEAR                    1
#define CUBIQUE                   3
#define DISTANCE                  4
#define TRIANGLE                  5
#define LINEAR_AND_NEAREST        6

#define EZ_EXTRAP                 1
#define EZ_NO_EXTRAP              0
#define RIEN                     -1

#define MAXIMUM                   4
#define MINIMUM                   5
#define VALEUR                    6
#define ABORT                     13

#define FICHIER         1
#define MEMOIRE         2

#define NZONES             5
#define DEHORS             0
#define AU_NORD            1
#define AU_SUD             2
#define POLE_NORD          3
#define POLE_SUD           4

#define GLOBAL          0
#define NORD            1
#define SUD             2

#define SYM             1
#define ANTISYM         0

#define CONSERVATIVE    0
#define LIBERAL         1

#define UNDEFINED       -1

#define C_TO_FTN(i,j,ni)  (int32_t)((ni) * (j) + i)

#define OUT_OUT         5

#define ABSOLU          0
#define RELATIF         1

#define SWLAT           0
#define SWLON           1
#define DLAT            2
#define DLON            3

#define TD60            0
#define TDGRW           1
#define CLAT            2
#define CLON            3

#define PI              0
#define PJ              1
#define D60             2
#define DGRW            3

#define IG1             0
#define IG2             1
#define IG3             2
#define IG4             3

#define XLAT1           0
#define XLON1           1
#define XLAT2           2
#define XLON2           3

#define CHUNK           128
#define LOG2_CHUNK      7
#define MAX_LOG_CHUNK   12

#define JOINT           1
#define DISJOINT        0

#define YES             1
#define NO              0

typedef struct {
    //! Nombre de points
    int32_t npts;
    //! Vecteur de coordonnees x
    float *x;
    //! Vecteur de coordonnees y
    float *y;
    //! indice du point dans le champ de destination
    int32_t *idx;
} _zone;

//! Grille Y
typedef struct {
    //! Nombre de poids
    int32_t n_wts;
    float *xx;
    float *yy;
    float *lat;
    float *lon;
    //! Tableau de poids
    float *wts;
    int32_t *mask;
    //! indice du point dans le champ de destination
    int32_t *idx;
} _ygrid;

typedef struct {
    int32_t flags;
    float *lat_rot, *lon_rot, *lat_true, *lon_true;
    float *sinlat_rot, *coslat_rot, *sinlon_rot, *coslon_rot;
    float *sinlat_true, *coslat_true, *sinlon_true, *coslon_true;
    float r[9], ri[9];
} _gemgrid;

typedef struct {
    int32_t flags,yyflags;
    int32_t use_sincos_cache;
    int32_t gdin;
    int32_t next_gdin;
    float valpolesud, valpolenord;
    float *x, *y;
    int32_t *mask_in, *mask_out;
    float *yin_maskout,*yan_maskout;
    float *yinlat,*yinlon,*yanlat,*yanlon;
    float *yin2yin_lat,*yin2yin_lon,*yan2yin_lat,*yan2yin_lon;
    float *yin2yan_lat,*yin2yan_lon,*yan2yan_lat,*yan2yan_lon;
    float *yin2yin_x,*yin2yin_y,*yan2yin_x,*yan2yin_y;
    float *yin2yan_x,*yin2yan_y,*yan2yan_x,*yan2yan_y;
    int32_t yincount_yin,yancount_yin,yincount_yan,yancount_yan;
    _gemgrid gemin, gemout;
    _ygrid ygrid;
    _zone zones[NZONES];
}_gridset;

typedef struct {
   int child;
   int childOf;
   int parent;
   int niOffset, njOffset;
   int sister;
   int assembly;
   int *parentOf;
   int *sisterOf;
} _sousgrille;

typedef struct {
    int32_t  ip1, ip2, ip3;
    int32_t date;
    int32_t npas, deet, nbits;
    int32_t hemisphere,axe_y_inverse;
    float xg[16], xgref[16];
    int32_t  ig[16], igref[16];
    char fst_grtyp[4],fst_grref[4];
    int32_t key_ax, key_ay;
    char nomvarx[8];
    char nomvary[8];
    char typvarx[4];
    char typvary[4];
    char etiketx[16];
    char etikety[16];
} _fstinfo;

typedef struct {
    int32_t index;
    int32_t grid_index;
    int32_t flags;
    int32_t i1, i2, j1, j2;
    int32_t ni,nj;
    int32_t nig, nxg;
    int32_t ni_ax, nj_ay;
    int32_t extension;
    int32_t needs_expansion;
    int32_t access_count;
    int32_t structured;
    int32_t next_gd;
    int32_t n_gdin, next_gdin, idx_last_gdin, n_gdin_for;
    int32_t log_chunk_gdin, log_chunk_gdin_for;
    int32_t *gdin_for, *mask;
    int32_t nsubgrids,mymaskgrid;
    int32_t mymaskgridi0,mymaskgridi1;
    int32_t mymaskgridj0,mymaskgridj1;
    int32_t *subgrid;
    float *lat, *lon;
    float *ax, *ay;
    float *ncx, *ncy;
    char grtyp[4], grref[4];
    _fstinfo fst;
    _gridset *gset;
} _Grille;


typedef struct {
  int32_t  damage_control;
  int32_t  degre_interp;
  int32_t  degre_extrap;
  int32_t  use_1subgrid;
  int32_t  valeur_1subgrid;
  int32_t  symmetrie;
  int32_t  vecteur;
  int32_t  verbose;
  int32_t  memory_use;
  int32_t  polar_correction;
  int32_t  wgt_num;
  int32_t  msg_pt_tol;
  int32_t  cld_interp_alg;
  int32_t  msg_interp_alg;
  float msg_gridpt_dist;
  float msg_dist_thresh;
  float valeur_extrap;
} _groptions;

#endif
