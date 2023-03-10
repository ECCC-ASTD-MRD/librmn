#ifndef FSTD98_INTERNAL_H__
#define FSTD98_INTERNAL_H__

#include <rmn/fstd98.h>

#include "armn_compress.h"
#include "base/moduledate.h"
#include "compresseur/armn_compress_32.h"
#include "compresseur/c_zfstlib.h"
#include "qstdir.h"
#include "packers/packers.h"
#include "primitives/primitives.h"
#include <rmn/fst_missing.h>

//! Throw an error when val is not between minval and maxval
#define VALID(val, minval, maxval, what, caller) \
    if ((val < minval) || (val > maxval)) { \
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: %s = %d must be between %d and %d\n",__func__,what,val,minval,maxval);\
        return(ERR_OUT_RANGE);\
    }

extern int remap_table[2][10];
extern int nb_remap;
extern char prnt_options[128];
extern int ip1s_flag;
extern int ip2s_flag;
extern int ip3s_flag;

// Signatures from fstd98.c
int init_ip_vals();
int ip_is_equal(int target, const int ip, int ind);
void memcpy_8_16(int16_t *p16, int8_t *p8, int nb);
void memcpy_16_8(int8_t *p8, int16_t *p16, int nb);
void memcpy_16_32(int32_t *p32, int16_t *p16, int nbits, int nb);
void memcpy_32_16(short *p16, int *p32, int nbits, int nb);
int fnom_index(const int iun);
void print_std_parms(const stdf_dir_keys * const stdf_entry, const char * const pre, const char * const option,
                     const int header);

#endif // FSTD98_INTERNAL_H__
