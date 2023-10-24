#ifndef RMN_FST_RECORD_H__
#define RMN_FST_RECORD_H__

#include <stdint.h>
#include <stdlib.h>

// What belongs to the record? To the writing function?
// Should data be compressed or not?
// Should have members directly accessible from Fortran? Yes, double up struct definition (in same C/Fortran header)
// Include NULL character in names? Yes

static const int FT_LENGTH = 2;
static const int VN_LENGTH = 4;
static const int LB_LENGTH = 12;
static const int PT_LENGTH = 1;

#define ALIGN_TO_4(val) ((val + 3) & 0xfffffffc)

// This struct should only be modified by ADDING member at the end
typedef struct {
    int64_t version;

    // 64-bit elements first
    void*   data;            //!< Record data
    void*   metadata;        //!< Record metadata.                                           TODO JSON object?
    int64_t date;            //!< Date timestamp

    // 32-bit elements
    int32_t datatype;        //!< Data type of elements
    int32_t num_packed_bits; //!< Number of bits kept for the elements of the field (npak)
    int32_t num_elem_i;      //!< First dimension of the data field                          WHAT UNITS? Elements
    int32_t num_elem_j;      //!< Second dimension of the data field
    int32_t num_elem_k;      //!< Thierd dimension of the data field

    int32_t timestep_length; //!< Length of the time steps in seconds (deet)
    int32_t timestep_num;    //!< Time step number

    int32_t ip1;             //!< Vertical level                                             WHAT DOES IP MEAN? Integer parameter
    int32_t ip2;             //!< Forecast hour
    int32_t ip3;             //!< User defined identifier

    int32_t grid_desc_1;     //!< First grid descriptor
    int32_t grid_desc_2;     //!< Second grid descriptor
    int32_t grid_desc_3;     //!< Third grid descriptor
    int32_t grid_desc_4;     //!< Fourth grid descriptor

    char field_type     [ALIGN_TO_4(FT_LENGTH + 1)]; //!< Type of field (forecast, analysis, climatology) (typvar)
    char projection_type[ALIGN_TO_4(PT_LENGTH + 1)]; //!< Type of geographical projection
    char var_name       [ALIGN_TO_4(VN_LENGTH + 1)]; //!< Variable name
    char label          [ALIGN_TO_4(LB_LENGTH + 1)]; //!< Label

} fst_record;

static const fst_record default_fst_record = (fst_record){
        .version = sizeof(fst_record),
        .data     = NULL,
        .metadata = NULL,
        .date     = -1,

        .datatype = -1,
        .num_packed_bits = -1,
        .num_elem_i = -1,
        .num_elem_j = -1,
        .num_elem_k = -1,

        .timestep_length = -1,
        .timestep_num    = -1,

        .ip1 = -1,
        .ip2 = -1,
        .ip3 = -1,

        .grid_desc_1 = -1,
        .grid_desc_2 = -1,
        .grid_desc_3 = -1,
        .grid_desc_4 = -1,

        .field_type =      {' ' , ' ' , '\0', '\0'},
        .projection_type = {' ' , '\0', '\0', '\0'},
        .var_name   =      {' ' , ' ' , ' ' , ' ',
                            '\0', '\0', '\0', '\0'},
        .label      =      {' ' , ' ' , ' ' , ' ',
                            ' ' , ' ' , ' ' , ' ',
                            ' ' , ' ' , ' ' , ' ',
                            '\0', '\0', '\0', '\0'},
    };


int32_t is_record_valid(const fst_record* record);

#endif // RMN_FST_RECORD_H__
