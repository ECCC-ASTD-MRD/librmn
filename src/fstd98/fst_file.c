#include "rmn/fst_file.h"

#include <stdlib.h>

#include <App.h>
#include "rmn/fnom.h"
#include "qstdir.h"


fst_file* fst23_open(const char* file_name, const char* options) {
    fst_file* the_file = (fst_file *)malloc(sizeof(fst_file));
    *the_file = default_fst_file;

    const int MAX_LENGTH = 1024;
    char local_options[MAX_LENGTH];
    if (options == NULL) {
        strncpy(local_options, "", MAX_LENGTH);
    } else {
        strncpy(local_options, options, MAX_LENGTH);
    }

    int iun = 0;
    c_fnom(&(the_file->iun), file_name, local_options, 0);
    c_fstouv(the_file->iun, local_options);

    return the_file;
}

void fst23_close(fst_file* file) {
    c_fstfrm(file->iun);
    c_fclos(file->iun);
    *file = default_fst_file;
}

int32_t fst23_write(fst_file* file, const fst_record* record) {
    if (!is_record_valid(record)) return ERR_BAD_INIT;

    char field_type[FT_LENGTH];
    char var_name[VN_LENGTH];
    char label[LB_LENGTH];
    char projection_type[PT_LENGTH];

    strncpy(field_type, record->field_type, FT_LENGTH);
    strncpy(var_name, record->var_name, VN_LENGTH);
    strncpy(label, record->label, LB_LENGTH);
    strncpy(projection_type, record->projection_type, PT_LENGTH);

    return
    c_fstecr(record->data, NULL, record->num_packed_bits, file->iun, record->date, record->timestep_length,
             record->timestep_num, record->num_elem_i, record->num_elem_j, record->num_elem_k, record->ip1,
             record->ip2, record->ip3, field_type, var_name, label,
             projection_type, record->grid_desc_1, record->grid_desc_2, record->grid_desc_3,
             record->grid_desc_4, record->datatype, 0);
}
