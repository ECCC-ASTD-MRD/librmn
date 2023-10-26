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

    char typvar[FT_LENGTH];
    char nomvar[VN_LENGTH];
    char etiket[LB_LENGTH];
    char grtyp[PT_LENGTH];

    strncpy(typvar, record->typvar, FT_LENGTH);
    strncpy(nomvar, record->nomvar, VN_LENGTH);
    strncpy(etiket, record->etiket, LB_LENGTH);
    strncpy(grtyp, record->grtyp, PT_LENGTH);

    return c_fstecr(
        record->data, NULL, record->npak, file->iun, record->date, record->deet, record->npas,
        record->ni, record->nj, record->nk, record->ip1, record->ip2, record->ip3,
        typvar, nomvar, etiket, grtyp, record->ig1, record->ig2, record->ig3, record->ig4, record->datyp, 0);
}
