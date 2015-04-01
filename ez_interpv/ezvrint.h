/*==============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : ezvrint
 * Creation  : version 1.0 mai 2003
 * Auteur    : Stéphane Gaudreault
 *
 * Description: ezvrint is the C version of EzInterpv, a front end to the
 * Interp1D package created by Jeffrey Blezius. This interface has been
 * designed to resemble that of the ezscint package. Most of the functionality
 * of the Interp1D package is available through the ezvrint interface. The
 * advantage of using ezvrint is that multiple interpolations are automated,
 * using an interface with a familiar feel. Ezvrint, also support some kind of
 * levels that are not supported by EzInterpv.
 *
 * Modification:
 *
 *   Nom         : -
 *   Date        : -
 *   Description : -
 *
 *==============================================================================
 */

#ifndef INCLUDED_EZVRINT
#define INCLUDED_EZVRINT

#ifndef _RPN_MACROS_
#  include <rpnmacros.h>
#endif

#define LVL_MASL    0
#define LVL_SIGMA   1  /* P/Ps */
#define LVL_PRES    2  /* in mb */
#define LVL_UNDEF   3  /* units are user defined */
#define LVL_MAGL    4
#define LVL_HYBRID  5
#define LVL_THETA   6
#define LVL_ETA     7  /* (Pt-P)/(Pt-Ps) -not in convip */
#define LVL_GALCHEN 8  /* Original Gal-Chen -not in convip */

/* Interpolation */
#define NEAREST_NEIGHBOUR 0x001
#define LINEAR            0x002
#define CUBIC_WITH_DERIV  0x004
#define CUBIC_LAGRANGE    0x008
/* Extrapolation */
#define CLAMPED    0x010
#define LAPSERATE  0x020
/* Other options */
#define VERBOSE    0x040
/* check for float exception (will do nothing on SX6) */
#define CHECKFLOAT 0x080

int c_viqkdef (const int, const int, float *, float, float, float, float *);
int c_videfset (const int, const int, int, int, float *, float *);
int c_visetopti (unsigned char);
int c_visetopt (const char*, const char*);
int c_visint (float *, float *, float *, float *, float, float);

/*
 * Interface Fortran
 */
wordint f77name (viqkdef)  (wordint *, wordint *, ftnfloat *, ftnfloat *, ftnfloat *, ftnfloat *, ftnfloat *);
wordint f77name (viqkdefset)  (wordint *, wordint *, wordint *, wordint *, ftnfloat *, ftnfloat *);
wordint f77name (visetopt) (wordint *, wordint *);
wordint f77name (visint)   (ftnfloat *, ftnfloat *, ftnfloat *, ftnfloat *, ftnfloat *, ftnfloat *);

#endif
