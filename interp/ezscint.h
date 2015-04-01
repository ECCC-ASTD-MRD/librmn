#ifndef _EZSCINT

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <rpnmacros.h>
#include <string.h>

#define NMAXGRIDS 32
#define NMAXSETS  NMAXGRIDS * (NMAXGRIDS - 1)

#define LAT                         1
#define AX                          2
#define XXX                         4
#define SYMMETRIQUE                64
#define TYPE_EXPANSION            128
#define NEWTON                    256
#define LATLON_PRIME_OK           512
#define SINLATLON_OK             1024
#define ZONES                    2048

#define SCALAIRE                  0
#define VECTEUR                   1

#define GLOBALE                   0
#define LOCALE                    1

#define NON                       0
#define OUI                       1

#define VOISIN                    0
#define LINEAIRE                  1
#define CUBIQUE                   3

#define EZ_EXTRAP                    1
#define EZ_NO_EXTRAP                 0
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

#define C_TO_FTN(i,j,ni)  (wordint)((ni) * (j) + i)

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

typedef struct
{
  wordint npts;               /* nombre de points */
  ftnfloat *x;                /* vecteur de coordonnees x */
  ftnfloat *y;                /* vecteur de coordonnees y */
  wordint *idx;               /* indice du point dans le champ de destination */
} _zone;

typedef struct
{
  wordint flags;
  wordint i1, i2, j1, j2;
  wordint ni,nj;
  wordint nig, nxg;
  wordint extension;
  wordint needs_expansion;
  wordint  ip1, ip2, ip3;
  wordint date;
  wordint npas, deet, nbits;
  wordint hemisphere,axe_y_inverse,count;
  ftnfloat xg[16], xgref[16];
  wordint  ig[16], igref[16];
  char nomvarx[8];
  char nomvary[8];
  char typvarx[4];
  char typvary[4];
  char etiketx[16];
  char etikety[16];
  char grtyp,grref;
  ftnfloat *lat, *lon;
  ftnfloat *ax, *ay;
  ftnfloat *ncx, *ncy;
} _Grille;

typedef struct
{
  wordint flags;
  ftnfloat *lat_rot, *lon_rot, *lat_true, *lon_true;
  ftnfloat *sinlat_rot, *coslat_rot, *sinlon_rot, *coslon_rot;
  ftnfloat *sinlat_true, *coslat_true, *sinlon_true, *coslon_true;
  ftnfloat r[9], ri[9];
} _geminfo;

typedef struct
{
  wordint flags;
  wordint gdin, gdout;
  wordint use_sincos_cache;
  ftnfloat *x, *y;
  ftnfloat valpolesud, valpolenord;
  _geminfo gemin, gemout;
  _zone zones[NZONES];
}_gridset;

typedef struct
{
  wordint damage_control;
  wordint degre_interp;
  wordint degre_extrap;
  wordint symmetrie;
  wordint vecteur;
  wordint verbose;
  wordint memory_use;
  wordint polar_correction;
  ftnfloat valeur_extrap;
}_groptions;


#endif


#define _EZSCINT
