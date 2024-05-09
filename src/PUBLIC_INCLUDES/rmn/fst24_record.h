#ifndef RMN_FST_RECORD_H__
#define RMN_FST_RECORD_H__

#ifndef IN_FORTRAN_CODE
//! \file fst24_record.h Public definitions for fst_record

//! Version identifier that needs to be incremented when we make
//! changes in the way records are stored and interpreted, so that it
//! can be recognized when read by a different version of the library
//! It has to take at most 8 bits, so its maximum value is 255
#endif

#define FST24_VERSION_COUNT  0

#define FST24_VERSION_OFFSET_C 1010101000u
#define FST24_VERSION_OFFSET_F 1010101000_int32

#define FST24_META_ALL  31 
#define FST24_META_TIME 1
#define FST24_META_GRID 2
#define FST24_META_INFO 4
#define FST24_META_SIZE 8
#define FST24_META_EXT  16

#ifndef IN_FORTRAN_CODE

#include <stdint.h>
#include <stdlib.h>

#include "rmn/fst_sz.h"

//! Smallest amount of bytes in multiples of 4 that can contain the given number of bytes
#define ALIGN_TO_4(val) ((val + 3) & 0xfffffffc)

static const int64_t FST_REC_ASSIGNED = 0x1; //!< Indicate a record whose data has been assigned by the user

// Forward declare, to be able to point to it
typedef struct fst24_file_ fst_file;

//! Description of an FST record. See \ref default_fst_record for the default values.
typedef struct {
    //!> Internal implementation details
    struct {
        int32_t version;  //!< Version marker
        int32_t deleted;  //!< Whether the record is deleted
        int64_t handle;   //!< Handle to specific record (if stored in a file)
        int64_t alloc;    //!< Size of allocated memody for data
        int32_t flags;    //!< Record status flags
        uint16_t num_search_keys;    //!< Number of directory search keys (32 bits)
        uint16_t extended_meta_size; //!< Size of extended metadata (32-bit units)
    } do_not_touch;

    // 64-bit elements first
    const fst_file* file;   //!< FST file where the record is stored
    void*   data;     //!< Record data
    void*   metadata; //!< Record metadata

    int64_t dateo;    //!< Origin Date timestamp
    int64_t datev;    //!< Valid Date timestamp

    // 32-bit elements
    int32_t data_type; //!< Data type of elements. See FST_TYPE_* constants.
    int32_t data_bits; //!< Number of bits per input elements
    int32_t pack_bits; //!< Number of stored bits
    int32_t ni;     //!< First dimension of the data field (number of elements)
    int32_t nj;     //!< Second dimension of the data field (number of elements)
    int32_t nk;     //!< Third dimension of the data field (number of elements)
    int32_t num_meta_bytes; //!< Size of the metadata in bytes

    int32_t deet; //!< Length of the time steps in seconds (deet)
    int32_t npas; //!< Time step number

    int32_t ip1;  //!< Vertical level
    int32_t ip2;  //!< Forecast hour
    int32_t ip3;  //!< User defined identifier

    int32_t ig1;  //!< First grid descriptor
    int32_t ig2;  //!< Second grid descriptor
    int32_t ig3;  //!< Third grid descriptor
    int32_t ig4;  //!< Fourth grid descriptor

    char typvar[ALIGN_TO_4(FST_TYPVAR_LEN + 1)]; //!< Type of field (forecast, analysis, climatology)
    char grtyp [ALIGN_TO_4(FST_GTYP_LEN + 1)];   //!< Type of geographical projection
    char nomvar[ALIGN_TO_4(FST_NOMVAR_LEN + 1)]; //!< Variable name
    char etiket[ALIGN_TO_4(FST_ETIKET_LEN + 1)]; //!< Label

} fst_record;


//! Default values for all members of an fst_record.
//! Values for searchable parameters correspond to their wildcard.
static const fst_record default_fst_record = (fst_record){
        .do_not_touch = {.version  = (FST24_VERSION_OFFSET_C + FST24_VERSION_COUNT),
                         .deleted  = 0,
                         .handle   = -1,
                         .alloc    = 0,
                         .flags    = 0x0,
                         .num_search_keys = 0,
                         .extended_meta_size = 0,},

        .file     = NULL,
        .data     = NULL,
        .metadata = NULL,
        .dateo     = -1,
        .datev     = -1,

        .data_type = -1,
        .data_bits = -1,
        .pack_bits = -1,
        .ni = -1,
        .nj = -1,
        .nk = -1,
        .num_meta_bytes = 0,

        .deet = -1,
        .npas = -1,

        .ip1 = -1,
        .ip2 = -1,
        .ip3 = -1,

        .ig1 = -1,
        .ig2 = -1,
        .ig3 = -1,
        .ig4 = -1,

        .typvar = {' ' , ' ' , '\0', '\0'},
        .grtyp  = {' ' , '\0', '\0', '\0'},
        .nomvar = {' ' , ' ' , ' ' , ' ',
                   '\0', '\0', '\0', '\0'},
        .etiket = {' ' , ' ' , ' ' , ' ',
                   ' ' , ' ' , ' ' , ' ',
                   ' ' , ' ' , ' ' , ' ',
                   '\0', '\0', '\0', '\0'},
    };


//! A set of (boolean) parameters to indicate which information to print or not.
//! See \ref default_fields for their default values
typedef struct {
    int32_t dateo, datev, datestamps;
    int32_t level;
    int32_t data_type, nijk;
    int32_t deet, npas;
    int32_t ip1, ip2, ip3, decoded_ip;
    int32_t grid_info, ig1234;
    int32_t typvar, nomvar, etiket;
    int32_t metadata;
} fst_record_fields;

//! A set of default values for fst_record_fields
static const fst_record_fields default_fields = (fst_record_fields) {
    .dateo = 1,
    .datev = 0,
    .datestamps = 1,

    .level = 0,
    .data_type = 1,
    .nijk = 1,

    .deet = 0,
    .npas = 0,

    .ip1 = 1,
    .ip2 = 1,
    .ip3 = 1,
    .decoded_ip = 0,

    .grid_info = 0,
    .ig1234 = 0,

    .typvar = 1,
    .nomvar = 1,
    .etiket = 1,

    .metadata = 0
};

//! \addtogroup public_fst
//! \{
//! Number of elements contained in the given record
static inline int64_t fst24_record_num_elem(const fst_record* const record) {
    return record->ni * record->nj * record->nk;
}

//! Number of data bytes in record
static inline int64_t fst24_record_data_size(const fst_record* record) {
    return (fst24_record_num_elem(record) * record->data_bits) / 8;
}

int32_t     fst24_record_is_valid(const fst_record* record);
int32_t     fst24_record_validate_params(const fst_record* record);
void        fst24_record_print(const fst_record* record);
void        fst24_record_print_short(const fst_record* record, const fst_record_fields* fields, const int print_header, const char* prefix);
fst_record* fst24_record_new(void *data, int32_t type, int32_t nbits, int32_t ni, int32_t nj, int32_t nk);
int32_t     fst24_record_free(fst_record* record);
int32_t     fst24_record_has_same_info(const fst_record* a, const fst_record* b);
int32_t     fst24_record_is_same(const fst_record* const a, const fst_record* const b);
void        fst24_record_diff(const fst_record* a, const fst_record* b);
int32_t     fst24_record_copy_metadata(fst_record* a, const fst_record* b,int What);
//! \}

int32_t fst24_record_validate_default(const fst_record* fortran_record, const size_t fortran_size);

#else

    type, bind(C) :: fst_record_c
        integer(C_INT32_T) :: version  = FST24_VERSION_OFFSET_F + FST24_VERSION_COUNT
        integer(C_INT32_T) :: deleted  = 0
        integer(C_INT64_T) :: handle   = -1
        integer(C_INT64_T) :: alloc    = 0
        integer(C_INT32_T) :: flags    = 0
        integer(C_INT16_T) :: num_search_keys = 0
        integer(C_INT16_T) :: extended_meta_size = 0

        type(C_PTR)        :: file     = C_NULL_PTR
        type(C_PTR)        :: data     = C_NULL_PTR
        type(C_PTR)        :: metadata = C_NULL_PTR
        integer(C_INT64_T) :: dateo    = -1
        integer(C_INT64_T) :: datev    = -1

        integer(C_INT32_T) :: data_type = -1
        integer(C_INT32_T) :: data_bits = -1
        integer(C_INT32_T) :: pack_bits = -1
        integer(C_INT32_T) :: ni    = -1
        integer(C_INT32_T) :: nj    = -1
        integer(C_INT32_T) :: nk    = -1
        integer(C_INT32_T) :: num_meta_bytes = 0

        integer(C_INT32_T) :: deet  = -1
        integer(C_INT32_T) :: npas  = -1

        integer(C_INT32_T) :: ip1   = -1
        integer(C_INT32_T) :: ip2   = -1
        integer(C_INT32_T) :: ip3   = -1

        integer(C_INT32_T) :: ig1   = -1
        integer(C_INT32_T) :: ig2   = -1
        integer(C_INT32_T) :: ig3   = -1
        integer(C_INT32_T) :: ig4   = -1

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
        integer(C_INT32_T) :: data_type = 1, nijk = 1
        integer(C_INT32_T) :: deet = 0, npas = 0
        integer(C_INT32_T) :: ip1 = 1, ip2 = 1, ip3 = 1, decoded_ip = 0
        integer(C_INT32_T) :: grid_info = 0, ig1234 = 1
        integer(C_INT32_T) :: typvar = 1, nomvar = 1, etiket = 1
        integer(C_INT32_T) :: metadata = 0
    end type fst_record_fields

#endif // IN_FORTRAN_CODE

#endif // RMN_FST_RECORD_H__
