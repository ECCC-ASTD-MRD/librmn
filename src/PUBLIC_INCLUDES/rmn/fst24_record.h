#ifndef RMN_FST_RECORD_H__
#define RMN_FST_RECORD_H__

#ifndef IN_FORTRAN_CODE

#include <stdint.h>
#include <stdlib.h>

#include "rmn/fst_sz.h"

#define ALIGN_TO_4(val) ((val + 3) & 0xfffffffc)
#define FST_REC_SIZE(REC) (REC->ni*REC->nj*REC->nk*(REC->dasiz>>3))  //TODO define right size
//#define FST_REC_SIZE(REC) (REC->ni*REC->nj*REC->nk*16 + 500)  //TODO define right size
#define FST_REC_ASSIGNED 0x1


// Forward declare, to be able to point to it
typedef struct fst24_file_ fst_file;

// This struct should only be modified by ADDING member at the end (once we're stable)
typedef struct {
    int64_t version;

    // 64-bit elements first
    fst_file* file;   //!< FST file where the record is stored
    void*   data;     //!< Record data
    void*   metadata; //!< Record metadata
    int64_t flags;    //!< Record status flags
    int64_t dateo;    //!< Origin Date timestamp
    int64_t datev;    //!< Valid Date timestamp
    int64_t handle;   //!< Handle to specific record (if stored in a file)
    int64_t alloc;    //!< Size of allocated memody for data

    // 32-bit elements
    int32_t datyp;//!< Data type of elements
    int32_t dasiz;//!< Number of bits per elements
    int32_t npak; //!< Compression factor (none if 0 or 1). Number of bit if negative
    int32_t ni;   //!< First dimension of the data field (number of elements)
    int32_t nj;   //!< Second dimension of the data field (number of elements)
    int32_t nk;   //!< Thierd dimension of the data field (number of elements)

    int32_t deet; //!< Length of the time steps in seconds (deet)
    int32_t npas; //!< Time step number

    int32_t ip1;  //!< Vertical level
    int32_t ip2;  //!< Forecast hour
    int32_t ip3;  //!< User defined identifier

    int32_t ig1;  //!< First grid descriptor
    int32_t ig2;  //!< Second grid descriptor
    int32_t ig3;  //!< Third grid descriptor
    int32_t ig4;  //!< Fourth grid descriptor

    int32_t dummy; // To make explicit the fact that the strings start at a 64-bit boundary

    char typvar[ALIGN_TO_4(FST_TYPVAR_LEN + 1)]; //!< Type of field (forecast, analysis, climatology)
    char grtyp [ALIGN_TO_4(FST_GTYP_LEN + 1)];   //!< Type of geographical projection
    char nomvar[ALIGN_TO_4(FST_NOMVAR_LEN + 1)]; //!< Variable name
    char etiket[ALIGN_TO_4(FST_ETIKET_LEN + 1)]; //!< Label

} fst_record;


static const fst_record default_fst_record = (fst_record){
        .version = sizeof(fst_record),
        .file     = NULL,
        .data     = NULL,
        .metadata = NULL,
        .flags = 0x0,
        .dateo     = -1,
        .datev     = -1,
        .handle   = -1,
        .alloc    = 0,

        .datyp = -1,
        .dasiz = -1,
        .npak = -1,
        .ni = -1,
        .nj = -1,
        .nk = -1,

        .deet = -1,
        .npas = -1,

        .ip1 = -1,
        .ip2 = -1,
        .ip3 = -1,

        .ig1 = -1,
        .ig2 = -1,
        .ig3 = -1,
        .ig4 = -1,

        .dummy = -1,

        .typvar = {' ' , ' ' , '\0', '\0'},
        .grtyp  = {' ' , '\0', '\0', '\0'},
        .nomvar = {' ' , ' ' , ' ' , ' ',
                   '\0', '\0', '\0', '\0'},
        .etiket = {' ' , ' ' , ' ' , ' ',
                   ' ' , ' ' , ' ' , ' ',
                   ' ' , ' ' , ' ' , ' ',
                   '\0', '\0', '\0', '\0'},
    };


typedef struct {
    int32_t dateo, datev, datestamps;
    int32_t level;
    int32_t datyp, ni, nj, nk;
    int32_t deet, npas;
    int32_t ip1, ip2, ip3, decoded_ip;
    int32_t grid_info, ig1234;
    int32_t typvar, nomvar, etiket;
} fst_record_fields;

static const fst_record_fields default_fields = (fst_record_fields) {
    .dateo = 1,
    .datev = 0,
    .datestamps = 1,

    .level = 0,
    .datyp = 1,

    .deet = 0,
    .npas = 0,

    .nomvar = 1,
    .typvar = 1,
    .etiket = 1,

    .ni = 1,
    .nj = 1,
    .nk = 1,

    .decoded_ip = 0,
    .ip1 = 1,
    .ip2 = 1,
    .ip3 = 1,

    .grid_info = 0,
    .ig1234 = 0
};

inline int64_t fst24_record_num_elem(const fst_record* record) {
    return record->ni * record->nj * record->nk;
}

//!> Number of data bytes in record
inline int64_t fst24_record_data_size(const fst_record* record) {
    return fst24_record_num_elem(record) * (record->dasiz>>3);
}

int32_t    fst24_record_is_valid(const fst_record* record);
int32_t    fst24_record_validate_params(const fst_record* record);
void       fst24_record_print(const fst_record* record);
void       fst24_record_print_short(const fst_record* record, const fst_record_fields* fields, const int print_header,
                                    const char* prefix);
fst_record fst24_record_new(void *data,int32_t type,int32_t nbits,int32_t ni,int32_t nj,int32_t nk);
int32_t    fst24_record_free(fst_record* record);
int32_t    fst24_record_has_same_info(const fst_record* a, const fst_record* b);
void       fst24_record_diff(const fst_record* a, const fst_record* b);

int32_t fst24_record_validate_default(const fst_record* fortran_record, const size_t fortran_size);

#else
    type, bind(C) :: fst_record_c
        integer(C_INT64_T) :: VERSION  = 168            ! Must be the number of bytes in the struct
        type(C_PTR)        :: file     = C_NULL_PTR
        type(C_PTR)        :: data     = C_NULL_PTR
        type(C_PTR)        :: metadata = C_NULL_PTR
        integer(C_INT64_T) :: flags    = 0
        integer(C_INT64_T) :: dateo    = -1
        integer(C_INT64_T) :: datev    = -1
        integer(C_INT64_T) :: handle   = -1
        integer(C_INT64_T) :: alloc    = 0

        integer(C_INT32_T) :: datyp = -1
        integer(C_INT32_T) :: dasiz = -1
        integer(C_INT32_T) :: npak  = -1
        integer(C_INT32_T) :: ni    = -1
        integer(C_INT32_T) :: nj    = -1
        integer(C_INT32_T) :: nk    = -1

        integer(C_INT32_T) :: deet  = -1
        integer(C_INT32_T) :: npas  = -1

        integer(C_INT32_T) :: ip1   = -1
        integer(C_INT32_T) :: ip2   = -1
        integer(C_INT32_T) :: ip3   = -1

        integer(C_INT32_T) :: ig1   = -1
        integer(C_INT32_T) :: ig2   = -1
        integer(C_INT32_T) :: ig3   = -1
        integer(C_INT32_T) :: ig4   = -1

        integer(C_INT32_T) :: dummy = -1

        character(len=1), dimension(4)  :: typvar = [' ', ' ', c_null_char, c_null_char]
        character(len=1), dimension(4)  :: grtyp  = [' ', c_null_char, c_null_char, c_null_char]
        character(len=1), dimension(8)  :: nomvar = [' ', ' ', ' ', ' ',        &
                                                     c_null_char, c_null_char, c_null_char, c_null_char]
        character(len=1), dimension(16) :: etiket = [' ', ' ', ' ', ' ',        &
                                                     ' ', ' ', ' ', ' ',        &
                                                     ' ', ' ', ' ', ' ',        &
                                                     c_null_char, c_null_char, c_null_char, c_null_char]
    end type fst_record_c

    type, bind(C) :: fst_record_fields
        integer(C_INT32_T) :: dateo = 1, datev = 0, datestamps = 1
        integer(C_INT32_T) :: level = 0
        integer(C_INT32_T) :: datyp = 1, ni = 1, nj = 1, nk = 1
        integer(C_INT32_T) :: deet = 0, npas = 0
        integer(C_INT32_T) :: ip1 = 1, ip2 = 0, ip3 = 0, decoded_ip = 0
        integer(C_INT32_T) :: grid_info = 0, ig1234 = 1
        integer(C_INT32_T) :: typvar = 1, nomvar = 1, etiket = 1
    end type fst_record_fields

#endif // IN_FORTRAN_CODE

#endif // RMN_FST_RECORD_H__
