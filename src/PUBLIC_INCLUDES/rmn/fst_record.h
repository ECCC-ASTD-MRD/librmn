#ifndef RMN_FST_RECORD_H__
#define RMN_FST_RECORD_H__

#include <stdint.h>
#include <stdlib.h>

#include "rmn/fstdsz.h"

// What belongs to the record? To the writing function?
// Should data be compressed or not?
// Should have members directly accessible from Fortran? Yes, double up struct definition (in same C/Fortran header)
// Include NULL character in names? Yes


#define ALIGN_TO_4(val) ((val + 3) & 0xfffffffc)

// This struct should only be modified by ADDING member at the end
typedef struct {
    int64_t version;

    // 64-bit elements first
    void*   data;     //!< Record data
    void*   metadata; //!< Record metadata.         TODO JSON object?
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

    char typvar[ALIGN_TO_4(TYPVAR_LEN + 1)]; //!< Type of field (forecast, analysis, climatology)
    char grtyp [ALIGN_TO_4(GTYP_LEN + 1)];   //!< Type of geographical projection
    char nomvar[ALIGN_TO_4(NOMVAR_LEN + 1)]; //!< Variable name
    char etiket[ALIGN_TO_4(ETIKET_LEN + 1)]; //!< Label

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


int32_t fst_record_is_valid(const fst_record* record);
void fst_record_print(const fst_record* record);

#endif // RMN_FST_RECORD_H__
