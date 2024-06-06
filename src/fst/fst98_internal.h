#ifndef fst98_internaL_H__
#define fst98_internaL_H__

#include <rmn/fst98.h>

#include "armn_compress.h"
#include "base/base.h"
#include "bitPacking.h"
#include "compresseur/armn_compress_32.h"
#include "compresseur/c_zfstlib.h"
#include "convip.h"
#include "fst_internal.h"
#include "qstdir.h"
#include "packers/packers.h"
#include "primitives/primitives.h"
#include <rmn/fst_missing.h>
#include <rmn/rsf.h>

#define use_old_signed_pack_unpack_code YES

//! Throw an error when val is not between minval and maxval
#define VALID(val, minval, maxval, what) \
    if ((val < minval) || (val > maxval)) { \
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: %s = %d must be between %d and %d\n",__func__,what,val,minval,maxval);\
        return(ERR_OUT_RANGE);\
    }

static inline int32_t is_type_real(const int32_t type_flag) {
    return ((base_fst_type(type_flag) == FST_TYPE_REAL_IEEE) ||
            (base_fst_type(type_flag) == FST_TYPE_REAL_OLD_QUANT) ||
            (base_fst_type(type_flag) == FST_TYPE_REAL));
}
static inline int32_t is_type_complex(const int32_t type_flag) {
    return (base_fst_type(type_flag) == FST_TYPE_COMPLEX);
}
static inline int32_t is_type_turbopack(const int32_t type_flag) {
    return ((type_flag & FST_TYPE_TURBOPACK) == FST_TYPE_TURBOPACK);
}
static inline int32_t has_type_missing(const int32_t type_flag) {
    return ((type_flag & FSTD_MISSING_FLAG) == FSTD_MISSING_FLAG);
}
static inline int32_t is_type_integer(const int32_t type_flag) {
    return ((base_fst_type(type_flag) == FST_TYPE_SIGNED) ||
            (base_fst_type(type_flag) == FST_TYPE_UNSIGNED));
}

//! Swap (in-place) the two halves of each 64-bit element in the given array
static inline void swap_words(void* array, const int32_t num_elem64) {
    register int32_t temp32, *src, *dest;
    src = (int32_t *) array;
    dest = (int32_t *) array;
    for (int i = 0; i < num_elem64; i++) {
        temp32 = *src++;
        *dest++ = *src++;
        *dest++ = temp32;
    }
}

typedef struct {
    fst_query query;
    int32_t next_file;
} fstd_usage_info;

extern int remap_table[2][10];
extern int nb_remap;
extern char prnt_options[128];
extern int ip1s_flag;
extern int ip2s_flag;
extern int ip3s_flag;
extern int downgrade_32;
extern fstd_usage_info fstd_open_files[MAXFILES];

static inline uint32_t stamp_from_date(const int64_t origin_date) {
    uint32_t date32 = origin_date & 0xffffffff;
    return 8 * (date32/10) + (date32 % 10);
}

uint32_t get_valid_date32(const int64_t origin_date, const int32_t timestep_size, const int32_t timestep_num);

// Signatures from fstd98.c
void copy_record_string(char* const dest, const char* const src, const int32_t max_length);
int32_t is_same_record_string(const char* str_a, const char* str_b, const int32_t max_length);
int init_ip_vals(void);
int ip_is_equal(int target, const int ip, int ind);
void memcpy_8_16(int16_t *p16, int8_t *p8, int nb);
void memcpy_16_8(int8_t *p8, int16_t *p16, int nb);
void memcpy_16_32(int32_t *p32, int16_t *p16, int nbits, int nb);
void memcpy_32_16(short *p16, int *p32, int nbits, int nb);
int fnom_index(const int iun);
void print_std_parms(const stdf_dir_keys * const stdf_entry, const char * const pre, const char * const option,
                     const int header);
void crack_std_parms(const stdf_dir_keys * const stdf_entry, stdf_special_parms * const cracked_parms);
int32_t c_fstunl(void);
int c_fstnbr_xdf(const int iun);
int c_fstecr_xdf(void *field_in, void *work, int npak, int iun, int date, int deet, int npas, int ni,
    int nj, int nk, int ip1, int ip2, int ip3, char *in_typvar, char *in_nomvar, char *in_etiket,
    char *in_grtyp, int ig1, int ig2, int ig3, int ig4, int in_datyp_ori, int rewrit);
int c_fstluk_xdf(void * const vfield, const int handle, int * const ni, int * const nj, int * const nk);
int c_fstprm_xdf(int handle, int *dateo, int *deet, int *npas, int *ni, int *nj, int *nk, int *nbits, int *datyp,
    int *ip1, int *ip2, int *ip3, char *typvar, char *nomvar, char *etiket, char *grtyp, int *ig1, int *ig2, int *ig3,
    int *ig4, int *swa, int *lng, int *dltf, int *ubc, int *extra1, int *extra2, int *extra3);
int c_fstcheck_xdf(const char *filePath);
int c_fstsui_xdf(int iun, int *ni, int *nj, int *nk);
int FstCanTranslateName(const char *varname);
char *kinds(int kind);
int c_fstckp_xdf(const int iun);
int c_fsteff_xdf(int handle);
int32_t c_fstlnk(const int32_t *liste, const int32_t n);

// Signatures from fstd98_rsf.c
int32_t is_rsf(const int32_t iun, int32_t* out_index_fnom);
int64_t find_next_rsf(const RSF_handle file_handle, fst_query* const search_params);
int c_fstecr_rsf(void *field_in, void *work, int npak, int iun, int index_fnom, int date, int deet, int npas, int ni,
                 int nj, int nk, int ip1, int ip2, int ip3, char *in_typvar, char *in_nomvar, char *in_etiket,
                 char *in_grtyp, int ig1, int ig2, int ig3, int ig4, int in_datyp_ori, int rewrit);
int c_fstfrm_rsf(int iun, const int index_fnom);
int c_fstinfx_rsf(const int handle, const int iun, const int index_fnom, int * const ni, int * const nj,
                  int * const nk, const int datev, const char * const in_etiket, const int ip1, const int ip2,
                  const int ip3, const char * const in_typvar, const char * const in_nomvar);
int c_fstlirx_rsf(void *field, int handle, int iun, const int index_fnom, int *ni, int *nj, int *nk, int datev,
                  char *etiket, int ip1, int ip2, int ip3, char *typvar, char *nomvar);
int c_fstnbr_rsf(const int index_fnom);
int c_fstouv_rsf(const int index_fnom, const int mode, const int32_t parallel_segment_size_mb);
int c_fstluk_rsf(void * const vfield, const RSF_handle file_handle,
                 const int key, int * const ni, int * const nj, int * const nk);
int c_fsteff_rsf(RSF_handle file_handle, int handle);
int c_fstinl_rsf(int iun, const int index_fnom, int *ni, int *nj, int *nk, int datev, char *etiket,
                 int ip1, int ip2, int ip3,
                 char *typvar, char *nomvar, int *liste, int *infon, int nmax);
int c_fstlis_rsf(void *field, int iun, const int index_fnom, int *ni, int *nj, int *nk);
int c_fstmsq_rsf(const int iun, const int index_fnom, int *mip1, int *mip2, int *mip3, char *metiket,
                 const int getmode);
int c_fstnbrv_rsf(const int index_fnom);
int c_fstprm_rsf(RSF_handle file_handle, int handle, int *dateo, int *deet, int *npas, int *ni, int *nj, int *nk,
                 int *nbits, int *datyp, int *dasiz, int *ip1, int *ip2, int *ip3, char *typvar, char *nomvar, char *etiket,
                 char *grtyp, int *ig1, int *ig2, int *ig3, int *ig4,  int *swa, int *lng, int *dltf, int *ubc,
                 int *extra1, int *extra2, int *extra3);
int c_fstsui_rsf(int iun, const int index_fnom, int *ni, int *nj, int *nk);
int c_fstvoi_rsf(const int iun, const int index_fnom, const char * const options, const int print_stats,
                 int64_t* const total_num_entries, int64_t* const total_num_valid_records, int64_t* const total_file_size,
                 int64_t* const total_num_writes, int64_t* const total_num_rewrites, int64_t* const total_num_erasures,
                 int64_t* const total_erased_size);
int32_t c_fstckp_rsf(const int iun, const int index_fnom);

#endif // fst98_internaL_H__
