/*
 * RMNLIB - Library of useful routines for C and FORTRAN programming
 * Copyright (C) 1975-2021  Division de Recherche en Prevision Numerique
 *                          Environnement Canada
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 */
#ifndef rmn_fst98H
#define rmn_fst98H

#include <stdint.h>

#include <rmn/rpnmacros.h>

#ifdef __cplusplus
extern "C" {
#endif

//!> Raw binary data. Its elements can have any size, and it is not subject to interpretation by the FST layer.
//!> Identified with X.
static const int32_t FST_TYPE_BINARY    = 0;

//!> Real-valued data using the old quantification scheme.
//!> Identified with R.
//!> This quantification is lossy and not reversible (non-cyclic)
//!> If trying to store with [31-32] bits, automatically converted to FST_TYPE_REAL_IEEE
static const int32_t FST_TYPE_REAL_OLD_QUANT = 1;

//!> Unsigned integer data
static const int32_t FST_TYPE_UNSIGNED  = 2;

//!> Characters (not compressed)
static const int32_t FST_TYPE_CHAR     = 3;

//!> Signed integer data
static const int32_t FST_TYPE_SIGNED    = 4;

//!> Real-valued data using IEEE format (no quantification), in 32 or 64 bits. Identified with E.
//!> When trying to store data with number of bits in the range [33-63], the original data size is
//!> preserved (either 32 or 64 bits).
//!> When trying to store 64-bit (double) data with 32 bits or less, it is first converted to 32-bit IEEE (float)
//!> When trying to store 32-bit (float) data with less than 32 bits, the extra bits are simply truncated from the
//!> mantissa (so don't go too low).
static const int32_t FST_TYPE_REAL_IEEE = 5;

//!> Real-valued data using a new quantification scheme.
//!> *This is the recommended REAL type to use.*
//!> This quantification scheme is lossy, but reversible (cyclic)
//!> Depending on number of bits requested for storage, a conversion may be performed at write-time.
//!>   if > 24 -> use FST_TYPE_REAL_IEEE with 32 bits
//!>   if [17-23] -> use FST_TYPE_REAL_OLD_QUANT with that number of bits
//!>   if < 16 -> quantify to 16, then truncate any extra bit from the new mantissa
static const int32_t FST_TYPE_REAL   = 6;

//!> Characters (compressed)
static const int32_t FST_TYPE_STRING    = 7;

//!> Complex number (32 or 64 bits)
static const int32_t FST_TYPE_COMPLEX   = 8;

static char* FST_TYPE_NAMES[] = {
    "FST_TYPE_BINARY",
    "FST_TYPE_OLD_QUANT",
    "FST_TYPE_UNSIGNED",
    "FST_TYPE_CHAR",
    "FST_TYPE_SIGNED",
    "FST_TYPE_REAL_IEEE",
    "FST_TYPE_REAL",
    "FST_TYPE_STRING",
    "FST_TYPE_COMPLEX"
};

static inline int32_t base_fst_type(const int32_t type_flag) {
    return type_flag & 0x3f;
}

//!> When added or |'d to a base type, indicate that we want to apply additional lossless compression to the data
static const int32_t FST_TYPE_TURBOPACK = 128;
static const int32_t FST_TYPE_MAGIC     = 801; // 512+256+32+1 no interference with turbo pack (128) and missing value (64) flags

int c_fst_data_length(const int length_type);
int c_ip1_all(const float level, const int kind);
int c_ip1_val(const float level, const int kind);
int c_ip2_all(const float level, const int kind);
int c_ip2_val(const float level, const int kind);
int c_ip3_all(const float level, const int kind);
int c_ip3_val(const float level, const int kind);
int c_fstckp(const int iun);
int c_fstmsq(const int iun, int *mip1, int *mip2, int *mip3, char *metiket, const int getmode);
int c_fstopi(const char * const option, const int value, const int getmode);
int c_fstopl(char *option, int value, int getmode);
int c_fstopr(char *option, float value, int getmode);
int c_fstopc(char *option, char *value, int getmode);
int c_fstcheck(const char *filePath);
int c_fstinf(const int iun, int * const ni, int * const nj, int * const nk, const int datev, const char * const in_etiket,
            const int ip1, const int ip2, const int ip3, const char * const in_typvar, const char * const in_nomvar);
int c_fstsui(int iun, int *ni, int *nj, int *nk);
int c_fstinl(int iun, int *ni, int *nj, int *nk, int datev, char *etiket,
            int ip1, int ip2, int ip3, char *typvar, char *nomvar,
            int *liste, int *infon, int nmax);
int c_fstinfx(const int handle, const int iun, int * const ni, int *const nj, int *const nk,
            const int datev, const char * const in_etiket,
            const int ip1, const int ip2, const int ip3, const char * const in_typvar, const char * const in_nomvar);
int c_fstlir(void *field, int iun, int *ni, int *nj, int *nk,
            int datev, char *etiket,
            int ip1, int ip2, int ip3, char *typvar, char *nomvar);
int c_fstlirx(void *field, int handle, int iun,
            int *ni, int *nj, int *nk, int datev, char *etiket,
            int ip1, int ip2, int ip3, char *typvar, char *nomvar);
int c_fstlis(void *field, int iun, int *ni, int *nj, int *nk);
int c_fstlic(void *field, int iun, int niin, int njin, int nkin,
            int datein, char *etiketin, int ip1in, int ip2in, int ip3in,
            char *typvarin, char *nomvarin,
            int ig1in, int ig2in, int ig3in, int ig4in, char *grtypin);
int c_fstluk(void * const field, const int handle, int * const ni, int * const nj, int * const nk);
int c_fstprm(int handle, int *dateo, int *deet, int *npas, int *ni, int *nj, int *nk,
            int *nbits, int *datyp, int *ip1, int *ip2, int *ip3, char *typvar,
            char *nomvar, char *etiket, char *grtyp,
            int *ig1, int *ig2, int *ig3, int *ig4, int *swa, int *lng,
            int *dltf, int *ubc, int *extra1, int *extra2, int *extra3);
int c_fstecr(void *field_in, void * work, int npak, int iun, int date,
            int deet, int npas, int ni, int nj, int nk, int ip1, int ip2, int ip3,
            char *in_typvar, char *in_nomvar, char *in_etiket, char *in_grtyp, 
            int ig1, int ig2, int ig3, int ig4, int in_datyp_ori, int rewrit);
int c_fstouv(int iun, char *options);
int c_fstvoi(const int iun, const char * const options);
int c_fstapp(int iun, char *option);
int c_fsteff(int handle);
int c_fsteof(int iun);
int c_fstrwd(int iun);
int c_fstskp(int iun, int nrec);
int c_fstweo(int iun, int level);
int c_fstnbr(const int iun);
int c_fstnbrv(int iun);
int c_fstfrm(int iun);
int c_fst_version();
void c_fstreset_ip_flags();
void c_fst_env_var(char *cle, int index, char *content);
int c_fst_edit_dir_plus(int handle, unsigned int date, int deet, int npas,
                        int ni, int nj, int nk, int ip1, int ip2, int ip3,
                        char *in_typvar, char *in_nomvar, char *in_etiket, char *in_grtyp, 
                        int ig1, int ig2, int ig3, int ig4, int datyp);
int c_fst_edit_dir(int handle, unsigned int date, int deet, int npas,
                int ni, int nj, int nk, int ip1, int ip2, int ip3,
                char *in_typvar, char *in_nomvar, char *in_etiket, char *in_grtyp, 
                int ig1, int ig2, int ig3, int ig4, int datyp);

void c_ip_string(char * const buffer, const int size, const int ip1, const int ip2, const int ip3);

int32_t f77name(fstluk)(uint32_t *field, int32_t *f_handle, int32_t *f_ni, int32_t *f_nj, int32_t *f_nk);
int32_t f77name(fstprm)(int32_t *f_handle,
                        int32_t *f_dateo, int32_t *f_deet, int32_t *f_npas,
                        int32_t *f_ni, int32_t *f_nj, int32_t *f_nk,
                        int32_t *f_nbits, int32_t *f_datyp, int32_t *f_ip1,
                        int32_t *f_ip2, int32_t *f_ip3, char *f_typvar,
                        char *f_nomvar, char *f_etiket, char *f_grtyp,
                        int32_t *f_ig1, int32_t *f_ig2, int32_t *f_ig3,
                        int32_t *f_ig4, int32_t *f_swa, int32_t *f_lng,
                        int32_t *f_dltf, int32_t *f_ubc, int32_t *f_extra1,
                        int32_t *f_extra2, int32_t *f_extra3,
                        F2Cl ll1, F2Cl ll2, F2Cl ll3, F2Cl ll4);

#ifdef __cplusplus
}
#endif

#endif // rmn_fst98H
