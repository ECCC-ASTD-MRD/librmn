#ifndef RMN_FST_RECORD_H__
#define RMN_FST_RECORD_H__

#ifndef IN_FORTRAN_CODE

#include <stdint.h>
#include <stdlib.h>

#include "rmn/fstdsz.h"

#define ALIGN_TO_4(val) ((val + 3) & 0xfffffffc)

// What belongs to the record? To the writing function?
// Should have members directly accessible from Fortran? Yes, double up struct definition (in same C/Fortran header)

// This struct should only be modified by ADDING member at the end (once we're stable)
typedef struct {
    int64_t version;

    // 64-bit elements first
    void*   data;     //!< Record data
    char*   metadata; //!< Record metadata.         TODO JSON object?
    int64_t date;     //!< Date timestamp
    int64_t handle;   //!< Handle to specific record (if stored in a file)

    // 32-bit elements
    int32_t datyp; //!< Data type of elements
    int32_t npak;  //!< Compression factor (none if 0 or 1). Number of bit if negative
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

    char typvar[ALIGN_TO_4(FST_TYPVAR_LEN + 1)]; //!< Type of field (forecast, analysis, climatology)
    char grtyp [ALIGN_TO_4(FST_GTYP_LEN + 1)];   //!< Type of geographical projection
    char nomvar[ALIGN_TO_4(FST_NOMVAR_LEN + 1)]; //!< Variable name
    char etiket[ALIGN_TO_4(FST_ETIKET_LEN + 1)]; //!< Label

} fst_record;

static const fst_record default_fst_record = (fst_record){
        .version = sizeof(fst_record),
        .data     = NULL,
        .metadata = NULL,
        .date     = -1,
        .handle   = -1,

        .datyp = -1,
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

        .typvar = {' ' , ' ' , '\0', '\0'},
        .grtyp  = {' ' , '\0', '\0', '\0'},
        .nomvar = {' ' , ' ' , ' ' , ' ',
                   '\0', '\0', '\0', '\0'},
        .etiket = {' ' , ' ' , ' ' , ' ',
                   ' ' , ' ' , ' ' , ' ',
                   ' ' , ' ' , ' ' , ' ',
                   '\0', '\0', '\0', '\0'},
    };


int32_t fst23_record_is_valid(const fst_record* record);
int32_t fst23_record_validate_params(const fst_record* record);
void    fst23_record_print(const fst_record* record);

#else
    type, bind(C) :: fst_record
        integer(C_INT64_T) :: VERSION  = 128            ! Must be the number of bytes in the struct
        type(C_PTR)        :: data     = C_NULL_PTR
        type(C_PTR)        :: metadata = C_NULL_PTR
        integer(C_INT64_T) :: date     = -1
        integer(C_INT64_T) :: handle   = -1

        integer(C_INT32_T) :: datyp = -1
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

        character(len=1), dimension(4)  :: typvar = [' ', ' ', c_null_char, c_null_char]
        character(len=1), dimension(4)  :: grtyp  = [' ', c_null_char, c_null_char, c_null_char]
        character(len=1), dimension(8)  :: nomvar = [' ', ' ', ' ', ' ',        &
                                                     c_null_char, c_null_char, c_null_char, c_null_char]
        character(len=1), dimension(16) :: etiket = [' ', ' ', ' ', ' ',        &
                                                     ' ', ' ', ' ', ' ',        &
                                                     ' ', ' ', ' ', ' ',        &
                                                     c_null_char, c_null_char, c_null_char, c_null_char]
    end type fst_record

#endif // IN_FORTRAN_CODE

#endif // RMN_FST_RECORD_H__
